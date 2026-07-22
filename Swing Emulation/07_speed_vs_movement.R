# 07_speed_vs_movement.R
# Follow-up to 05. The "+VELO" set there bundled speed + approach + movement.
# OBP (via HitTrax) has pitch SPEED and contact LOCATION, but NOT pitch
# movement (pfx). So we split the velocity signal to learn, per swing metric:
#   speed_gain        = what OBP CAN add beyond location   (LOC+SPEED - LOC)
#   move_beyond_speed = what OBP CANNOT add (movement)     (BOTH - LOC+SPEED)
# obp_capture = speed_gain / (both_gain) = fraction of the velocity-family
# signal reproducible from OBP-available inputs (location + speed).

suppressMessages({ library(tidyverse); library(xgboost) })
data_dir <- file.path("Swing Emulation", "data")
out_dir  <- file.path("Swing Emulation", "output")

set.seed(42)
sw <- readRDS(file.path(data_dir, "swings_all.rds")) |>
  mutate(split = case_when(game_pk %% 5 %in% 0:2 ~ "train",
                           game_pk %% 5 == 3     ~ "valid",
                           TRUE                  ~ "test"))
tr_idx <- which(sw$split == "train")
keep   <- c(sample(tr_idx, min(150000, length(tr_idx))),
            which(sw$split %in% c("valid", "test")))
sw <- sw[sort(keep), ]

loc        <- c("plate_x", "plate_z", "plate_z_rel", "is_rhb", "is_rhp")
speed_set  <- c(loc, "release_speed", "velo_at_plate")             # OBP-available
move_set   <- c(loc, "pfx_x", "pfx_z")                             # OBP-absent
both_set   <- c(loc, "release_speed", "velo_at_plate", "pfx_x", "pfx_z")

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
    nrounds = 600, early_stopping_rounds = 40, verbose = 0
  )
  pred <- predict(fit, as.matrix(d[te, feats]))
  obs  <- d[[target]][te]; base <- mean(d[[target]][tr])
  1 - sum((obs - pred)^2) / sum((obs - base)^2)
}

res <- map_dfr(targets, function(tg) {
  message("modeling ", tg)
  tibble(target = tg,
         LOC   = r2(sw, tg, loc),
         SPEED = r2(sw, tg, speed_set),
         MOVE  = r2(sw, tg, move_set),
         BOTH  = r2(sw, tg, both_set))
}) |>
  mutate(speed_gain        = SPEED - LOC,
         move_beyond_speed = BOTH - SPEED,
         both_gain         = BOTH - LOC,
         obp_capture       = ifelse(both_gain > 0.01, speed_gain / both_gain, NA_real_))

print(mutate(res, across(where(is.numeric), ~ round(.x, 3))), width = Inf)
saveRDS(res, file.path(out_dir, "speed_vs_movement.rds"))

res |>
  select(target, LOC, SPEED, MOVE, BOTH) |>
  pivot_longer(-target, names_to = "model", values_to = "r2") |>
  mutate(model = factor(model, c("LOC", "MOVE", "SPEED", "BOTH"))) |>
  ggplot(aes(target, r2, fill = model)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  labs(title = "Speed vs movement: which velocity signal can OBP reproduce?",
       subtitle = "SPEED = OBP-available (HitTrax); MOVE = OBP-absent. SPEED near BOTH means OBP can match it.",
       x = NULL, y = "held-out R^2") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "speed_vs_movement.png"), width = 9, height = 4.8, dpi = 150)
cat("\ndone -> speed_vs_movement.png\n")
