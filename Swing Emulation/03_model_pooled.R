# 03_model_pooled.R
# Phase 1: pooled swing model -- "the average MLB hitter" (LHH/RHH via the
# stand feature). One xgboost regressor per bat-tracking target.
#
# Input:  Swing Emulation/data/model_data.rds (in-zone competitive swings)
# Output: Swing Emulation/models/pooled_<target>.xgb  (+ feature list rds)
#         printed holdout metrics vs a predict-the-mean baseline

library(tidyverse)
library(xgboost)

data_dir   <- file.path("Swing Emulation", "data")
model_dir  <- file.path("Swing Emulation", "models")
dir.create(model_dir, showWarnings = FALSE)

TARGETS <- c("bat_speed", "swing_length", "attack_angle",
             "attack_direction", "swing_path_tilt")

FEATURES <- c(
  "is_rhb", "is_rhp", "balls", "strikes",
  "release_pos_x", "release_pos_z", "release_extension",
  "vx0", "vy0", "vz0", "ax", "ay", "az",
  "release_speed", "plate_x", "plate_z", "plate_z_rel",
  "pfx_x", "pfx_z", "velo_at_plate", "vaa", "haa", "t_flight_50",
  "release_spin_rate", "spin_axis"
)

train_pooled <- function(df, target, features,
                         params = list(objective = "reg:squarederror",
                                       eta = 0.05, max_depth = 6,
                                       subsample = 0.8, colsample_bytree = 0.8,
                                       min_child_weight = 20)) {
  X <- as.matrix(df[df$split == "train", features])
  y <- df[[target]][df$split == "train"]
  Xv <- as.matrix(df[df$split == "valid", features])
  yv <- df[[target]][df$split == "valid"]

  fit <- xgb.train(
    params = params,
    data = xgb.DMatrix(X, label = y),
    watchlist = list(valid = xgb.DMatrix(Xv, label = yv)),
    nrounds = 2000, early_stopping_rounds = 50, verbose = 0
  )
  fit
}

evaluate <- function(fit, df, target, features) {
  test <- df[df$split == "test", ]
  pred <- predict(fit, as.matrix(test[, features]))
  obs  <- test[[target]]
  base <- mean(df[[target]][df$split == "train"])
  tibble(
    target        = target,
    n_test        = length(obs),
    rmse          = sqrt(mean((obs - pred)^2)),
    rmse_baseline = sqrt(mean((obs - base)^2)),
    mae           = mean(abs(obs - pred)),
    r_squared     = 1 - sum((obs - pred)^2) / sum((obs - base)^2),
    best_iter     = fit$best_iteration
  )
}

if (sys.nframe() == 0) {
  set.seed(42)
  model_data <- readRDS(file.path(data_dir, "model_data.rds")) |>
    # split by game so pitches from one game never straddle train/test
    mutate(split = case_when(
      game_pk %% 10 %in% 0:7 ~ "train",
      game_pk %% 10 == 8     ~ "valid",
      TRUE                   ~ "test"
    ))

  results <- list()
  for (tg in TARGETS) {
    fit <- train_pooled(model_data, tg, FEATURES)
    xgb.save(fit, file.path(model_dir, paste0("pooled_", tg, ".xgb")))
    results[[tg]] <- evaluate(fit, model_data, tg, FEATURES)

    imp <- xgb.importance(model = fit) |> head(6)
    cat("\n--", tg, "top features:",
        paste(imp$Feature, collapse = ", "), "\n")
  }
  saveRDS(FEATURES, file.path(model_dir, "pooled_features.rds"))

  metrics <- list_rbind(results)
  print(metrics, width = Inf)
  saveRDS(metrics, file.path(model_dir, "pooled_metrics.rds"))
}
