# 13_headtohead.R
# Clean head-to-head on IDENTICAL held-out swings: pooled xgb vs per-player GAM
# surfaces vs the embedding net. Test = game_pk %% 5 == 4 (none of the models
# trained on it). Qualified hitters = in the embedding key with >= 200 train
# swings (so a per-player GAM is fittable). RMSE per metric, pooled over all
# qualified hitters' test swings.

suppressMessages({ library(tidyverse); library(mgcv); library(xgboost); library(torch) })
data_dir  <- file.path("Swing Emulation", "data")
model_dir <- file.path("Swing Emulation", "models")

METRICS <- c("bat_speed", "swing_length", "attack_angle", "attack_direction", "swing_path_tilt")
FEATS   <- c("px_in", "plate_z_rel", "plate_z", "release_speed", "is_rhp")

meta <- readRDS(file.path(model_dir, "player_embed_meta.rds"))

sw <- readRDS(file.path(data_dir, "swings_all.rds")) |>
  filter(abs(plate_x) < 1.3, plate_z_rel > -0.3, plate_z_rel < 1.3) |>
  mutate(px_in = if_else(stand == "R", -plate_x, plate_x)) |>
  filter(if_all(all_of(c(FEATS, METRICS)), ~ !is.na(.x))) |>
  inner_join(select(meta$batter_key, batter, b_idx), by = "batter") |>
  mutate(set = if_else(game_pk %% 5 == 4, "test", "train"), rowid = row_number())

qual <- sw |> filter(set == "train") |> count(batter) |> filter(n >= 200) |> pull(batter)
sw   <- filter(sw, batter %in% qual)
te   <- filter(sw, set == "test")
cat("qualified hitters:", length(qual), " test swings:", nrow(te), "\n")

## ---- pooled xgb ----
feats_pool <- readRDS(file.path(model_dir, "pooled_features.rds"))
pooled_pred <- map(set_names(METRICS), function(m) {
  fit <- xgb.load(file.path(model_dir, paste0("pooled_", m, ".xgb")))
  predict(fit, as.matrix(te[, feats_pool]))
})

## ---- embedding net ----
swingnet <- nn_module("swingnet",
  initialize = function(n_batter, n_feat, emb_dim, n_out) {
    self$emb <- nn_embedding(n_batter, emb_dim); self$fc1 <- nn_linear(n_feat + emb_dim, 64)
    self$fc2 <- nn_linear(64, 32); self$drop <- nn_dropout(0.1); self$out <- nn_linear(32, n_out) },
  forward = function(x, b) {
    h <- torch_cat(list(x, self$emb(b)), dim = 2); h <- self$drop(nnf_relu(self$fc1(h)))
    h <- nnf_relu(self$fc2(h)); self$out(h) })
net <- torch_load(file.path(model_dir, "player_embed_net.pt")); net$eval()
Xz <- scale(as.matrix(te[, FEATS]), center = meta$fmean[FEATS], scale = meta$fsd[FEATS])
with_no_grad({ ez <- as.matrix(net(torch_tensor(Xz, dtype = torch_float()),
                                   torch_tensor(te$b_idx, dtype = torch_long()))) })
embed_pred <- as_tibble(sweep(sweep(ez, 2, meta$tsd[METRICS], "*"), 2, meta$tmean[METRICS], "+")) |>
  set_names(METRICS)

## ---- per-player GAM surfaces (fit on each hitter's train, predict their test) ----
gm <- matrix(NA_real_, nrow(te), length(METRICS), dimnames = list(NULL, METRICS))
for (bid in qual) {
  tr_b <- filter(sw, batter == bid, set == "train"); te_b <- filter(sw, batter == bid, set == "test")
  if (nrow(te_b) == 0) next
  pos <- match(te_b$rowid, te$rowid)
  for (m in METRICS) {
    g <- tryCatch(gam(as.formula(paste0(m, " ~ s(px_in, plate_z_rel, k = 15) + s(release_speed)")),
                      data = tr_b, method = "REML"), error = function(e) NULL)
    gm[pos, m] <- if (is.null(g)) mean(tr_b[[m]]) else as.numeric(predict(g, te_b))
  }
}
gam_pred <- as_tibble(gm)

## ---- RMSE per metric per model ----
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
res <- map_dfr(METRICS, function(m) tibble(
  metric = m,
  pooled  = rmse(te[[m]], pooled_pred[[m]]),
  surface = rmse(te[[m]], gam_pred[[m]]),
  embed   = rmse(te[[m]], embed_pred[[m]])))
res <- mutate(res, best = c("pooled","surface","embed")[max.col(-as.matrix(res[,-1]))])
print(mutate(res, across(where(is.numeric), ~ round(.x, 3))), width = Inf)
saveRDS(res, file.path(model_dir, "headtohead.rds"))
cat("\ndone\n")
