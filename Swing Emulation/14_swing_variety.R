# 14_swing_variety.R
# The emulator predicts the MEAN swing. Real hitters vary swing-to-swing at a
# fixed pitch. We model that variety empirically: the embedding net's residuals
# on held-out swings ARE the natural spread. Sampling from their covariance
# (scaled per hitter, since some hitters repeat more than others) yields an
# ensemble of plausible swings -> a "swing cloud".

suppressMessages({ library(tidyverse); library(torch); library(MASS) })
source(file.path("Swing Emulation", "04_bat_path_reconstruction.R"))
select <- dplyr::select
data_dir  <- file.path("Swing Emulation", "data")
model_dir <- file.path("Swing Emulation", "models")
out_dir   <- file.path("Swing Emulation", "output")

METRICS <- c("bat_speed", "swing_length", "attack_angle", "attack_direction", "swing_path_tilt")
FEATS   <- c("px_in", "plate_z_rel", "plate_z", "release_speed", "is_rhp")
SZ_BOT <- 1.6; SZ_TOP <- 3.4
meta <- readRDS(file.path(model_dir, "player_embed_meta.rds"))

swingnet <- nn_module("swingnet",
  initialize = function(n_batter, n_feat, emb_dim, n_out) {
    self$emb <- nn_embedding(n_batter, emb_dim); self$fc1 <- nn_linear(n_feat + emb_dim, 64)
    self$fc2 <- nn_linear(64, 32); self$drop <- nn_dropout(0.1); self$out <- nn_linear(32, n_out) },
  forward = function(x, b) { h <- torch_cat(list(x, self$emb(b)), dim = 2)
    h <- self$drop(nnf_relu(self$fc1(h))); h <- nnf_relu(self$fc2(h)); self$out(h) })
net <- torch_load(file.path(model_dir, "player_embed_net.pt")); net$eval()

predict_metrics <- function(px_in, plate_z_rel, plate_z, release_speed, is_rhp, b_idx) {
  Xz <- scale(matrix(c(px_in, plate_z_rel, plate_z, release_speed, is_rhp), 1),
              center = meta$fmean[FEATS], scale = meta$fsd[FEATS])
  with_no_grad({ z <- as.numeric(net(torch_tensor(Xz, dtype = torch_float()),
                                     torch_tensor(as.integer(b_idx), dtype = torch_long()))) })
  set_names(z * meta$tsd[METRICS] + meta$tmean[METRICS], METRICS)
}

# ---- residuals on a holdout fold -> the natural swing-to-swing spread ----
sw <- readRDS(file.path(data_dir, "swings_all.rds")) |>
  filter(abs(plate_x) < 1.3, plate_z_rel > -0.3, plate_z_rel < 1.3) |>
  mutate(px_in = if_else(stand == "R", -plate_x, plate_x)) |>
  filter(if_all(all_of(c(FEATS, METRICS)), ~ !is.na(.x))) |>
  inner_join(select(meta$batter_key, batter, b_idx), by = "batter") |>
  filter(game_pk %% 5 == 4)

Xz <- scale(as.matrix(sw[, FEATS]), center = meta$fmean[FEATS], scale = meta$fsd[FEATS])
with_no_grad({ pz <- as.matrix(net(torch_tensor(Xz, dtype = torch_float()),
                                   torch_tensor(sw$b_idx, dtype = torch_long()))) })
pred <- sweep(sweep(pz, 2, meta$tsd[METRICS], "*"), 2, meta$tmean[METRICS], "+")
resid <- as.matrix(sw[, METRICS]) - pred
Sigma <- cov(resid)                                   # cross-metric residual covariance
cat("=== natural swing-to-swing spread (residual SD per metric) ===\n")
print(round(sqrt(diag(Sigma)), 2))

# per-hitter spread scale: how repeatable is each hitter vs the average
hit_scale <- tibble(b_idx = sw$b_idx, r2 = rowSums((resid %*% solve(Sigma)) * resid) / length(METRICS)) |>
  group_by(b_idx) |> summarise(scale = sqrt(mean(r2)), n = n(), .groups = "drop")

sample_swings <- function(name, px_in, plate_z_rel, release_speed, rhp = TRUE, n = 30) {
  row <- filter(meta$batter_key, str_detect(player_name, regex(name, TRUE)))[1, ]
  plate_z <- SZ_BOT + plate_z_rel * (SZ_TOP - SZ_BOT)
  mu <- predict_metrics(px_in, plate_z_rel, plate_z, release_speed, as.numeric(rhp), row$b_idx)
  sc <- hit_scale$scale[match(row$b_idx, hit_scale$b_idx)]; if (is.na(sc)) sc <- 1
  draws <- MASS::mvrnorm(n, mu = mu, Sigma = Sigma * sc^2)
  plate_x <- if (row$stand == "R") -px_in else px_in
  list(who = row$player_name, stand = row$stand, plate_x = plate_x, plate_z = plate_z,
       mu = mu, draws = draws, scale = sc)
}

# ---- demo: a swing cloud for one hitter vs one pitch ----
sv <- sample_swings("Judge, Aaron", px_in = 0.3, plate_z_rel = 0.3, release_speed = 95, n = 40)
cat(sprintf("\n%s vs inside-low 95: mean bat_speed %.1f, spread scale %.2f\n",
            sv$who, sv$mu["bat_speed"], sv$scale))

arc_from <- function(v, stand, contact, id) {
  reconstruct_bat_path(v["bat_speed"], v["swing_length"], v["attack_angle"],
                       v["attack_direction"], v["swing_path_tilt"], contact, stand) |>
    mutate(id = id)
}
contact <- c(sv$plate_x, 17/12, sv$plate_z)
cloud <- map_dfr(1:nrow(sv$draws), ~ arc_from(sv$draws[.x, ], sv$stand, contact, .x))
mean_arc <- arc_from(sv$mu, sv$stand, contact, 0)

ggplot() +
  geom_path(data = cloud, aes(y, z, group = id), color = "#D85A30", alpha = 0.18, linewidth = 0.6) +
  geom_path(data = mean_arc, aes(y, z), color = "#7A2E12", linewidth = 1.4) +
  annotate("point", x = 17/12, y = sv$plate_z, shape = 4, size = 4, stroke = 1.2) +
  coord_equal(xlim = c(-4, 6)) +
  labs(title = paste0("Swing variety: ", sv$who, " vs an inside-low 95 mph pitch"),
       subtitle = "40 sampled swings (light) around the mean emulation (dark); spread from held-out residuals",
       x = "y: distance from plate (ft)", y = "height (ft)") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "swing_variety_cloud.png"), width = 9, height = 5, dpi = 150)
saveRDS(list(Sigma = Sigma, hit_scale = hit_scale), file.path(model_dir, "swing_variety.rds"))
cat("\nsaved -> swing_variety_cloud.png\n")
