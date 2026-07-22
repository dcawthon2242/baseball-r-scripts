# 12_emulate_hitter.R
# The capstone: pick ANY of the 696 hitters + ANY pitch (location + speed) ->
# the embedding net predicts their 5 swing metrics -> the reconstruction draws
# the swing. Two demos: many hitters vs one pitch, and one hitter across the
# zone.

suppressMessages({ library(tidyverse); library(torch) })
source(file.path("Swing Emulation", "04_bat_path_reconstruction.R"))  # reconstruct_bat_path

model_dir <- file.path("Swing Emulation", "models")
out_dir   <- file.path("Swing Emulation", "output")

METRICS <- c("bat_speed", "swing_length", "attack_angle",
             "attack_direction", "swing_path_tilt")
FEATS   <- c("px_in", "plate_z_rel", "plate_z", "release_speed", "is_rhp")

meta <- readRDS(file.path(model_dir, "player_embed_meta.rds"))

# architecture must be defined for torch_load to restore the module
swingnet <- nn_module(
  "swingnet",
  initialize = function(n_batter, n_feat, emb_dim, n_out) {
    self$emb <- nn_embedding(n_batter, emb_dim)
    self$fc1 <- nn_linear(n_feat + emb_dim, 64)
    self$fc2 <- nn_linear(64, 32)
    self$drop <- nn_dropout(0.1)
    self$out <- nn_linear(32, n_out)
  },
  forward = function(x, b) {
    h <- torch_cat(list(x, self$emb(b)), dim = 2)
    h <- self$drop(nnf_relu(self$fc1(h)))
    h <- nnf_relu(self$fc2(h))
    self$out(h)
  }
)
net <- torch_load(file.path(model_dir, "player_embed_net.pt"))
net$eval()

# Look up a hitter and predict their swing vs a pitch.
# Pitch: px_in (+inside, handedness-adjusted), plate_z_rel (0 bottom .. 1 top of
# zone), release_speed (mph), rhp (pitcher throws right = TRUE).
SZ_BOT <- 1.6; SZ_TOP <- 3.4
emulate_hitter <- function(name, px_in, plate_z_rel, release_speed, rhp = TRUE) {
  row <- filter(meta$batter_key, str_detect(player_name, regex(name, TRUE)))
  if (nrow(row) == 0) stop("no hitter matches '", name, "'")
  if (nrow(row) > 1) message("'", name, "' matched ", nrow(row),
                             " hitters; using ", row$player_name[1])
  row <- row[1, ]
  plate_z <- SZ_BOT + plate_z_rel * (SZ_TOP - SZ_BOT)
  x  <- c(px_in, plate_z_rel, plate_z, release_speed, as.numeric(rhp))
  xz <- (x - meta$fmean[FEATS]) / meta$fsd[FEATS]
  with_no_grad({
    pz <- as.numeric(net(torch_tensor(matrix(xz, 1), dtype = torch_float()),
                         torch_tensor(row$b_idx, dtype = torch_long())))
  })
  pred <- set_names(pz * meta$tsd[METRICS] + meta$tmean[METRICS], METRICS)
  plate_x <- if (row$stand == "R") -px_in else px_in
  list(who = row$player_name, stand = row$stand, plate_x = plate_x,
       plate_z = plate_z, pred = pred)
}

emulate_arc <- function(em, label = em$who) {
  reconstruct_bat_path(em$pred["bat_speed"], em$pred["swing_length"],
                       em$pred["attack_angle"], em$pred["attack_direction"],
                       em$pred["swing_path_tilt"],
                       contact = c(em$plate_x, 17/12, em$plate_z),
                       stand = em$stand) |>
    mutate(label = label)
}

# ---- demo A: six hitters vs one pitch (middle-middle, 94 mph RHP) ----
crew <- c("Judge, Aaron", "Ohtani", "Altuve", "Arraez", "Kwan", "Witt")
demoA <- map(crew, ~ emulate_hitter(.x, px_in = 0, plate_z_rel = 0.5, release_speed = 94))
cat("=== six hitters vs a middle-middle 94 mph pitch ===\n")
map_dfr(demoA, ~ tibble(who = .x$who, !!!round(.x$pred, 1))) |> print(width = Inf)

arcsA <- map_dfr(demoA, emulate_arc)
ggplot(arcsA, aes(y, z, color = label)) +
  geom_path(linewidth = 1.1) +
  geom_point(data = group_by(arcsA, label) |> slice_tail(n = 1), size = 2.6) +
  annotate("point", x = 17/12, y = 1.6 + 0.5 * 1.8, shape = 4, size = 4, stroke = 1.2) +
  coord_equal(xlim = c(-4, 6)) + scale_color_brewer(palette = "Dark2") +
  labs(title = "Emulate anyone: six hitters vs one pitch (middle-middle, 94 mph)",
       subtitle = "embedding net -> predicted swing -> reconstructed sweet-spot arc (x = contact)",
       x = "y: distance from plate (ft)", y = "height (ft)", color = NULL) +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "emulate_six_hitters.png"), width = 9, height = 5, dpi = 150)

# ---- demo B: one hitter across the zone (Judge, 4 corners, 94 mph) ----
corners <- tribble(
  ~label,        ~px_in, ~zrel,
  "in / up",       0.5,   0.85,
  "in / low",      0.5,   0.15,
  "away / up",    -0.5,   0.85,
  "away / low",   -0.5,   0.15
)
demoB <- pmap(corners, function(label, px_in, zrel)
  emulate_arc(emulate_hitter("Judge, Aaron", px_in, zrel, 94), label))
cat("\n=== Judge across the zone ===\n")
map2_dfr(demoB, corners$label, ~ tibble(pitch = .y, !!!round(
  emulate_hitter("Judge, Aaron", corners$px_in[match(.y, corners$label)],
                 corners$zrel[match(.y, corners$label)], 94)$pred, 1))) |> print(width = Inf)

arcsB <- list_rbind(demoB)
ggplot(arcsB, aes(y, z, color = label)) +
  geom_path(linewidth = 1.1) +
  geom_point(data = group_by(arcsB, label) |> slice_tail(n = 1), size = 2.6) +
  coord_equal(xlim = c(-4, 6)) + scale_color_brewer(palette = "Set1") +
  labs(title = "Face this pitch as Aaron Judge: swing across the zone",
       subtitle = "same hitter, four locations (94 mph) -> the swing adapts",
       x = "y: distance from plate (ft)", y = "height (ft)", color = "pitch") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "emulate_judge_zone.png"), width = 9, height = 5, dpi = 150)
cat("\nsaved -> emulate_six_hitters.png, emulate_judge_zone.png\n")
