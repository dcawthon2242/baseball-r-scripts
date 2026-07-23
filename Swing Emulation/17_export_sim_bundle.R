# 17_export_sim_bundle.R
# Export a self-contained JSON bundle for the 3D at-bat simulator:
#  - pitchers x pitch-type average trajectories (9 params + release + plate loc)
#  - hitters: name, stand, embedding vector
#  - the swing-net weights + standardization (so the browser predicts swings)
# Pitcher names come from the MLB stats API (the CSV only has pitcher IDs).

suppressMessages({ library(tidyverse); library(jsonlite); library(torch) })
data_dir  <- file.path("Swing Emulation", "data")
model_dir <- file.path("Swing Emulation", "models")
web_dir   <- file.path("Swing Emulation", "web"); dir.create(web_dir, showWarnings = FALSE)

PT_NAMES <- c(FF="4-Seam", SI="Sinker", FC="Cutter", SL="Slider", ST="Sweeper",
              CU="Curveball", KC="Knuckle Curve", CH="Changeup", FS="Splitter",
              SV="Slurve", FO="Forkball", EP="Eephus")

raw <- map(c(2025, 2026), ~ readRDS(file.path(data_dir, paste0("savant_", .x, ".rds")))) |> list_rbind()

traj <- raw |>
  filter(!is.na(vx0), !is.na(plate_x), pitch_type %in% names(PT_NAMES)) |>
  group_by(pitcher, pitch_type) |>
  summarise(n = n(), throws = names(which.max(table(p_throws))),
            px_sd = sd(plate_x, na.rm = TRUE), pz_sd = sd(plate_z, na.rm = TRUE),
            spd_sd = sd(release_speed, na.rm = TRUE),
            pfxx_sd = sd(pfx_x, na.rm = TRUE), pfxz_sd = sd(pfx_z, na.rm = TRUE),
            across(c(release_pos_x, release_pos_y, release_pos_z, vx0, vy0, vz0,
                     ax, ay, az, release_speed, plate_x, plate_z, pfx_x, pfx_z),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") |>
  filter(n >= 25)

# league-wide correlation structure of (plate_x, plate_z, speed, pfx_x, pfx_z)
# per pitch type -- lets the sim sample movement/speed/location JOINTLY (extra
# break arrives with the location shift it causes) while each pitcher keeps his
# own scatter magnitudes
cor_dat <- raw |>
  filter(pitch_type %in% names(PT_NAMES)) |>
  select(pitch_type, plate_x, plate_z, release_speed, pfx_x, pfx_z) |>
  drop_na() |>
  group_by(pitch_type)
cors_json <- set_names(
  group_map(cor_dat, ~ round(unname(cor(as.matrix(.x))), 3)),
  group_keys(cor_dat)$pitch_type)

# keep pitchers with >= 200 total tracked pitches (regulars)
keep_pit <- traj |> group_by(pitcher) |> summarise(tot = sum(n)) |> filter(tot >= 200) |> pull(pitcher)
traj <- filter(traj, pitcher %in% keep_pit)
cat("pitchers:", n_distinct(traj$pitcher), " pitcher x pitchtype rows:", nrow(traj), "\n")

# ---- pitcher names from MLB stats API (batched) ----
ids <- unique(traj$pitcher)
name_map <- map(split(ids, ceiling(seq_along(ids) / 100)), function(chunk) {
  u <- paste0("https://statsapi.mlb.com/api/v1/people?personIds=", paste(chunk, collapse = ","))
  p <- tryCatch(fromJSON(u)$people, error = function(e) NULL)
  if (is.null(p)) return(tibble(pitcher = chunk, name = as.character(chunk)))
  tibble(pitcher = p$id, name = p$fullName)
}) |> list_rbind()
traj <- left_join(traj, name_map, by = "pitcher") |> mutate(name = coalesce(name, as.character(pitcher)))

pitchers_json <- traj |>
  mutate(pt_name = PT_NAMES[pitch_type]) |>
  group_by(pitcher, name, throws) |>
  summarise(pitches = list(tibble(
    type = pitch_type, label = pt_name, n = n,
    rel = pmap(list(release_pos_x, release_pos_y, release_pos_z), ~ round(c(...), 3)),
    v = pmap(list(vx0, vy0, vz0), ~ round(c(...), 3)),
    a = pmap(list(ax, ay, az), ~ round(c(...), 3)),
    speed = round(release_speed, 1), plate = pmap(list(plate_x, plate_z), ~ round(c(...), 3)),
    pfx = pmap(list(pfx_x, pfx_z), ~ round(c(...), 3)),
    sd = pmap(list(px_sd, pz_sd, spd_sd, pfxx_sd, pfxz_sd), ~ round(c(...), 3))
  )), .groups = "drop") |>
  arrange(name)

# ---- hitters + swing-net weights ----
meta <- readRDS(file.path(model_dir, "player_embed_meta.rds"))
swingnet <- nn_module("swingnet",
  initialize = function(n_batter, n_feat, emb_dim, n_out) {
    self$emb <- nn_embedding(n_batter, emb_dim); self$fc1 <- nn_linear(n_feat + emb_dim, 64)
    self$fc2 <- nn_linear(64, 32); self$drop <- nn_dropout(0.1); self$out <- nn_linear(32, n_out) },
  forward = function(x, b) x)
net <- torch_load(file.path(model_dir, "player_embed_net.pt"))
sd <- net$state_dict(); W <- function(k) round(as.array(sd[[k]]$cpu()), 5)

variety <- readRDS(file.path(model_dir, "swing_variety.rds"))

# swing/take decision net (script 15) -- has its OWN batter key; map onto the
# main hitter list by MLBAM batter id, -1 where the hitter lacks a decision
# embedding (JS falls back to the league-mean vector)
dmeta <- readRDS(file.path(model_dir, "swing_decision_meta.rds"))
decnet_mod <- nn_module("decnet",
  initialize = function(nb, nf, ed) {
    self$emb <- nn_embedding(nb, ed); self$fc1 <- nn_linear(nf + ed, 64)
    self$fc2 <- nn_linear(64, 32); self$drop <- nn_dropout(0.15); self$out <- nn_linear(32, 1) },
  forward = function(x, b) x)
dnet <- torch_load(file.path(model_dir, "swing_decision_net.pt"))
dsd <- dnet$state_dict(); DW <- function(k) round(as.array(dsd[[k]]$cpu()), 5)
demb <- as.array(dsd[["emb.weight"]]$cpu())

# contact net (script 18): P(whiff) + vertical miss bias + predicted miss
# distance; shares the swing net's b_idx space so JS reuses hitter.idx
cmeta <- readRDS(file.path(model_dir, "contact_meta.rds"))
cnet <- torch_load(file.path(model_dir, "contact_net.pt"))
csd <- cnet$state_dict(); CW <- function(k) round(as.array(csd[[k]]$cpu()), 5)

hitters_json <- meta$batter_key |>
  left_join(variety$hit_scale, by = "b_idx") |>
  left_join(transmute(dmeta$bkey, batter, didx = b_idx - 1), by = "batter") |>
  transmute(name = player_name, stand, idx = b_idx - 1,      # 0-based for JS
            vscale = round(coalesce(scale, 1), 3),
            didx = coalesce(didx, -1)) |>
  arrange(name)

# contact DEPTH model: how far in front of the plate the barrel meets the ball,
# as a function of pitch location + speed (inside pitches are met out front,
# away pitches deeper). intercept_y is inches relative to the batter's center
# of mass, so we export deviations-from-mean; the JS anchors the mean depth.
sw_depth <- readRDS(file.path(data_dir, "swings_all.rds")) |>
  mutate(px_in = if_else(stand == "R", -plate_x, plate_x)) |>
  filter(!is.na(intercept_y), !is.na(px_in), !is.na(plate_z_rel), !is.na(release_speed))
dfit <- lm(intercept_y ~ px_in + plate_z_rel + release_speed, data = sw_depth)
depth_json <- list(coef = round(coef(dfit)[-1], 4),
                   means = round(c(px_in = mean(sw_depth$px_in),
                                   plate_z_rel = mean(sw_depth$plate_z_rel),
                                   release_speed = mean(sw_depth$release_speed)), 4),
                   resid_sd_in = round(sd(resid(dfit)), 2))
cat("depth model coefs (in/unit):", paste(names(coef(dfit)[-1]), round(coef(dfit)[-1],2), collapse=", "), "\n")

bundle <- list(
  pitchers = pitchers_json,
  hitters  = hitters_json,
  emb      = round(meta$emb, 5),
  net = list(w1 = W("fc1.weight"), b1 = W("fc1.bias"), w2 = W("fc2.weight"),
             b2 = W("fc2.bias"), w3 = W("out.weight"), b3 = W("out.bias")),
  feats = c("px_in","plate_z_rel","plate_z","release_speed","is_rhp"),
  metrics = c("bat_speed","swing_length","attack_angle","attack_direction","swing_path_tilt"),
  fmean = round(meta$fmean, 5), fsd = round(meta$fsd, 5),
  tmean = round(meta$tmean, 5), tsd = round(meta$tsd, 5),
  bat_radius = 2.57,
  sigma = round(unname(variety$Sigma), 4),   # 5x5 swing-to-swing covariance
  depth = depth_json,
  cors = cors_json,                          # per-pitch-type 5x5 correlations
  dec = list(w1 = DW("fc1.weight"), b1 = DW("fc1.bias"), w2 = DW("fc2.weight"),
             b2 = DW("fc2.bias"), w3 = DW("out.weight"), b3 = DW("out.bias"),
             emb = round(demb, 5), meanEmb = round(colMeans(demb), 5),
             fmean = round(unname(dmeta$fmean), 5), fsd = round(unname(dmeta$fsd), 5)),
  con = list(w1 = CW("fc1.weight"), b1 = CW("fc1.bias"), w2 = CW("fc2.weight"),
             b2 = CW("fc2.bias"), w3 = CW("out.weight"), b3 = CW("out.bias"),
             emb = round(as.array(csd[["emb.weight"]]$cpu()), 5),
             fmean = round(unname(cmeta$fmean), 5), fsd = round(unname(cmeta$fsd), 5),
             bias_m = round(cmeta$bias_m, 4), bias_s = round(cmeta$bias_s, 4),
             miss_m = round(cmeta$miss_m, 4), miss_s = round(cmeta$miss_s, 4))
)
write_json(bundle, file.path(web_dir, "sim_bundle.json"), auto_unbox = TRUE, digits = 5)
cat("wrote sim_bundle.json (", round(file.size(file.path(web_dir, "sim_bundle.json"))/1024), "KB), ",
    nrow(hitters_json), "hitters\n")
