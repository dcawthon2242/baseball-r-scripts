# 09_player_surfaces.R
# Phase 2, model 1: per-player swing SURFACES (the interpretable baseline for
# the "both, compare" plan). For each hitter, fit a smooth GAM of each swing
# metric over pitch location + speed -- the continuous version of the
# Freeman/Altuve 3x3 grid. emulate_player() then returns that hitter's expected
# swing at any pitch, which feeds the reconstruction in 04.
#
# Conditioning variables follow the sufficiency findings (05/07): location +
# speed. px_in is handedness-adjusted so "inside" means the same thing L vs R.

suppressMessages({ library(tidyverse); library(mgcv); library(xgboost) })
source(file.path("Swing Emulation", "04_bat_path_reconstruction.R"))  # reconstruct_bat_path

data_dir  <- file.path("Swing Emulation", "data")
model_dir <- file.path("Swing Emulation", "models")
out_dir   <- file.path("Swing Emulation", "output")

METRICS <- c("bat_speed", "swing_length", "attack_angle",
             "attack_direction", "swing_path_tilt")

sw <- readRDS(file.path(data_dir, "swings_all.rds")) |>
  filter(abs(plate_x) < 1.2, plate_z_rel > -0.25, plate_z_rel < 1.25) |>
  mutate(px_in = if_else(stand == "R", -plate_x, plate_x))

hitters <- tribble(
  ~who,              ~pat,      ~side,
  "Freddie Freeman", "Freeman", "L",
  "Luis Arraez",     "Arraez",  "L",
  "Jose Altuve",     "Altuve",  "R",
  "Aaron Judge",     "Judge",   "R"
)

get_player <- function(sw, pat, side) filter(sw, str_detect(player_name, regex(pat, TRUE)), stand == side)

fit_surface <- function(df) {
  set_names(map(METRICS, function(m) {
    f <- as.formula(paste0(m, " ~ s(px_in, plate_z_rel, k = 20) + s(release_speed)"))
    gam(f, data = df, method = "REML")
  }), METRICS)
}

predict_surface <- function(models, px_in, plate_z_rel, release_speed) {
  nd <- tibble(px_in, plate_z_rel, release_speed)
  map_dbl(models, ~ as.numeric(predict(.x, nd)))
}

# ---- fit every demo hitter ----
players <- pmap(hitters, function(who, pat, side) {
  df <- get_player(sw, pat, side)
  list(who = who, side = side, n = nrow(df), models = fit_surface(df))
})

# ---- emulate all hitters vs the SAME pitch: inside-low, 92 mph ----
demo <- list(px_in = 0.40, plate_z_rel = 0.20, release_speed = 92)
plate_z <- 1.6 + demo$plate_z_rel * (3.4 - 1.6)

cat("=== emulated swing vs an inside-low 92 mph pitch ===\n")
pred_tbl <- map_dfr(players, function(p) {
  pr <- predict_surface(p$models, demo$px_in, demo$plate_z_rel, demo$release_speed)
  tibble(who = p$who, n_swings = p$n, !!!round(pr, 1))
})
print(pred_tbl, width = Inf)

# ---- reconstruct + overlay the arcs (side view) ----
arcs <- map_dfr(players, function(p) {
  pr <- predict_surface(p$models, demo$px_in, demo$plate_z_rel, demo$release_speed)
  plate_x <- if_else(p$side == "R", -demo$px_in, demo$px_in)
  reconstruct_bat_path(pr["bat_speed"], pr["swing_length"], pr["attack_angle"],
                       pr["attack_direction"], pr["swing_path_tilt"],
                       contact = c(plate_x, 17/12, plate_z), stand = p$side) |>
    mutate(who = p$who)
})

ggplot(arcs, aes(y, z, color = who)) +
  geom_path(linewidth = 1.1) +
  geom_point(data = group_by(arcs, who) |> slice_tail(n = 1), size = 3) +
  annotate("point", x = 17/12, y = plate_z, shape = 4, size = 4, stroke = 1.2) +
  coord_equal(xlim = c(-4, 6)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Four hitters, one pitch (inside-low, 92 mph)",
       subtitle = "per-player surfaces -> predicted swing -> reconstructed sweet-spot arc (x = contact)",
       x = "y: distance from plate (ft)", y = "height (ft)", color = NULL) +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "emulated_arcs_4hitters.png"), width = 9, height = 5, dpi = 150)

# ---- does per-player beat the pooled model? held-out RMSE, bat_speed & attack_angle ----
feats <- readRDS(file.path(model_dir, "pooled_features.rds"))
pooled <- set_names(map(c("bat_speed", "attack_angle"),
                        ~ xgb.load(file.path(model_dir, paste0("pooled_", .x, ".xgb")))),
                    c("bat_speed", "attack_angle"))

cat("\n=== held-out RMSE: per-player surface vs pooled model ===\n")
cmp <- map_dfr(hitters$pat[hitters$pat != ""] |> set_names(hitters$who), .id = "who", function(pat) {
  side <- hitters$side[hitters$pat == pat]
  df <- get_player(sw, pat, side) |> mutate(fold = row_number() %% 5)
  tr <- filter(df, fold != 0); te <- filter(df, fold == 0)
  map_dfr(c("bat_speed", "attack_angle"), function(m) {
    g <- gam(as.formula(paste0(m, " ~ s(px_in, plate_z_rel, k = 20) + s(release_speed)")),
             data = tr, method = "REML")
    rmse_player <- sqrt(mean((te[[m]] - as.numeric(predict(g, te)))^2))
    rmse_pooled <- sqrt(mean((te[[m]] - predict(pooled[[m]], as.matrix(te[, feats])))^2))
    tibble(metric = m, rmse_player = round(rmse_player, 2),
           rmse_pooled = round(rmse_pooled, 2),
           improve_pct = round(100 * (rmse_pooled - rmse_player) / rmse_pooled, 1))
  })
})
print(cmp, width = Inf)
cat("\nsaved -> emulated_arcs_4hitters.png\n")
