# 05_location_sufficiency.R
# Hypothesis (user's): plate location (plate_x, plate_z) is a near-sufficient
# summary of the pitch for predicting the SWING -- i.e. swing metrics depend on
# WHERE the ball is more than HOW it got there. If true:
#   (a) the pitch input collapses from 9 params to 2D location, and
#   (b) OBP swings (no trajectory, but they DO have contact/pitch location) can
#       be aligned to Statcast on location instead of trajectory.
#
# Test: for each swing metric, compare held-out R^2 of three nested models
#   LOC   = location only (plate_x, plate_z, plate_z_rel, handedness)
#   +VELO = location + speed/approach angle/movement
#   FULL  = location + the entire 9-parameter trajectory + release + count
# The gap FULL - LOC is the information in the trajectory BEYOND location.
# Expectation: swing-PLANE metrics are location-sufficient; contact DEPTH
# (timing) needs velocity, since a faster pitch must be met further out front.

suppressMessages({ library(tidyverse); library(xgboost) })
data_dir <- file.path("Swing Emulation", "data")
out_dir  <- file.path("Swing Emulation", "output")

set.seed(42)
sw <- readRDS(file.path(data_dir, "swings_all.rds")) |>
  mutate(split = case_when(game_pk %% 5 %in% 0:2 ~ "train",
                           game_pk %% 5 == 3     ~ "valid",
                           TRUE                  ~ "test"))
# subsample training for speed; test/valid kept whole
tr_idx <- which(sw$split == "train")
keep   <- c(sample(tr_idx, min(200000, length(tr_idx))),
            which(sw$split %in% c("valid", "test")))
sw <- sw[sort(keep), ]

loc_feats  <- c("plate_x", "plate_z", "plate_z_rel", "is_rhb", "is_rhp")
velo_feats <- c(loc_feats, "release_speed", "velo_at_plate", "vaa", "haa", "pfx_x", "pfx_z")
full_feats <- c(velo_feats, "vx0", "vy0", "vz0", "ax", "ay", "az",
                "release_pos_x", "release_pos_z", "release_extension",
                "release_spin_rate", "spin_axis", "t_flight_50", "balls", "strikes")

targets <- c("intercept_y", "attack_angle", "attack_direction",
             "swing_path_tilt", "bat_speed")

r2 <- function(df, target, feats) {
  d  <- df[!is.na(df[[target]]), ]
  tr <- d$split == "train"; va <- d$split == "valid"; te <- d$split == "test"
  fit <- xgb.train(
    params = list(objective = "reg:squarederror", eta = 0.08, max_depth = 6,
                  subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 20),
    data = xgb.DMatrix(as.matrix(d[tr, feats]), label = d[[target]][tr]),
    watchlist = list(v = xgb.DMatrix(as.matrix(d[va, feats]), label = d[[target]][va])),
    nrounds = 700, early_stopping_rounds = 40, verbose = 0
  )
  pred <- predict(fit, as.matrix(d[te, feats]))
  obs  <- d[[target]][te]; base <- mean(d[[target]][tr])
  1 - sum((obs - pred)^2) / sum((obs - base)^2)
}

res <- map_dfr(targets, function(tg) {
  message("modeling ", tg)
  tibble(target = tg,
         LOC   = r2(sw, tg, loc_feats),
         VELO  = r2(sw, tg, velo_feats),
         FULL  = r2(sw, tg, full_feats))
}) |>
  mutate(loc_share = LOC / FULL,               # fraction of explainable signal from location alone
         velo_gain = VELO - LOC,               # incremental from speed/approach
         traj_gain = FULL - VELO)              # incremental from full trajectory beyond that

print(mutate(res, across(where(is.numeric), ~ round(.x, 3))), width = Inf)
saveRDS(res, file.path(out_dir, "location_sufficiency.rds"))

res |>
  select(target, LOC, VELO, FULL) |>
  pivot_longer(-target, names_to = "model", values_to = "r2") |>
  mutate(model = factor(model, c("LOC", "VELO", "FULL"))) |>
  ggplot(aes(target, r2, fill = model)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(option = "D", end = 0.85) +
  labs(title = "Is plate location a sufficient summary of the pitch?",
       subtitle = "held-out R^2 by feature set; LOC ~ FULL means location suffices for that metric",
       x = NULL, y = "held-out R^2") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "location_sufficiency.png"), width = 9, height = 4.8, dpi = 150)
cat("\ndone -> location_sufficiency.png\n")
