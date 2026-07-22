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
            across(c(release_pos_x, release_pos_y, release_pos_z, vx0, vy0, vz0,
                     ax, ay, az, release_speed, plate_x, plate_z), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") |>
  filter(n >= 25)

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
    speed = round(release_speed, 1), plate = pmap(list(plate_x, plate_z), ~ round(c(...), 3))
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

hitters_json <- meta$batter_key |>
  transmute(name = player_name, stand, idx = b_idx - 1) |>   # 0-based for JS
  arrange(name)

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
  bat_radius = 2.57
)
write_json(bundle, file.path(web_dir, "sim_bundle.json"), auto_unbox = TRUE, digits = 5)
cat("wrote sim_bundle.json (", round(file.size(file.path(web_dir, "sim_bundle.json"))/1024), "KB), ",
    nrow(hitters_json), "hitters\n")
