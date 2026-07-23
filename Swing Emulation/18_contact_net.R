# 18_contact_net.R
# The contact model for the simulator: given a pitch (location, speed,
# movement), count, and batter, predict
#   (1) P(whiff)            -- trained on all competitive swings
#   (2) vertical miss bias  -- signed under/over tendency. Not directly
#       observable, so we use its footprint on contact: launch_angle minus
#       attack_angle (undercutting launches the ball above the swing plane).
#       A high fastball should show the bat under the ball more often.
#   (3) predicted miss distance (inches) -- trained on Statcast miss_distance
#       for whiffs.
# One torch net, shared trunk, 6-dim batter embedding (same b_idx key as the
# swing net so the sim reuses hitter.idx), three heads with masked losses.

suppressMessages({ library(tidyverse); library(torch) })
data_dir  <- file.path("Swing Emulation", "data")
model_dir <- file.path("Swing Emulation", "models")
set.seed(3); torch_manual_seed(3)

CFEATS <- c("px_in","plate_z_rel","plate_z","release_speed","pfx_x","pfx_z",
            "balls","strikes","is_rhp","is_rhb")
SWING <- c("hit_into_play","foul","foul_tip","swinging_strike","swinging_strike_blocked")

meta <- readRDS(file.path(model_dir, "player_embed_meta.rds"))

raw <- map(c(2025, 2026), ~ readRDS(file.path(data_dir, paste0("savant_", .x, ".rds")))) |>
  list_rbind() |>
  filter(description %in% SWING, !is.na(bat_speed), bat_speed >= 55,
         !str_detect(coalesce(des, ""), regex("bunt", ignore_case = TRUE)),
         !is.na(plate_x), !is.na(sz_top), !is.na(release_speed),
         !is.na(pfx_x), !is.na(pfx_z)) |>
  transmute(batter, game_pk,
            px_in = if_else(stand == "R", -plate_x, plate_x),
            plate_z_rel = (plate_z - sz_bot) / (sz_top - sz_bot),
            plate_z, release_speed, pfx_x, pfx_z, balls, strikes,
            is_rhp = as.integer(p_throws == "R"), is_rhb = as.integer(stand == "R"),
            whiff = as.integer(str_detect(description, "swinging_strike")),
            bias_deg = if_else(description == "hit_into_play",
                               pmax(-60, pmin(60, launch_angle - attack_angle)), NA_real_),
            miss_in = if_else(str_detect(description, "swinging_strike"),
                              pmax(0, pmin(18, miss_distance)), NA_real_)) |>
  filter(if_all(all_of(CFEATS), ~ is.finite(.x))) |>
  inner_join(select(meta$batter_key, batter, b_idx), by = "batter") |>
  mutate(split = if_else(game_pk %% 5 == 4, "test", "train"))

cat("swings:", nrow(raw), " whiff rate:", round(mean(raw$whiff), 3),
    " with bias target:", sum(!is.na(raw$bias_deg)),
    " with miss target:", sum(!is.na(raw$miss_in)), "\n")

tr <- raw$split == "train"
fmean <- map_dbl(CFEATS, ~ mean(raw[[.x]][tr])); fsd <- map_dbl(CFEATS, ~ sd(raw[[.x]][tr]))
names(fmean) <- names(fsd) <- CFEATS
bias_m <- mean(raw$bias_deg[tr], na.rm = TRUE); bias_s <- sd(raw$bias_deg[tr], na.rm = TRUE)
miss_m <- mean(raw$miss_in[tr],  na.rm = TRUE); miss_s <- sd(raw$miss_in[tr],  na.rm = TRUE)

Xz <- scale(as.matrix(raw[, CFEATS]), center = fmean, scale = fsd)
mk <- function(m) list(
  x = torch_tensor(Xz[m, ], dtype = torch_float()),
  b = torch_tensor(raw$b_idx[m], dtype = torch_long()),
  yw = torch_tensor(raw$whiff[m], dtype = torch_float())$unsqueeze(2),
  yb = torch_tensor(replace_na((raw$bias_deg[m] - bias_m) / bias_s, 0), dtype = torch_float())$unsqueeze(2),
  mb = torch_tensor(as.numeric(!is.na(raw$bias_deg[m])), dtype = torch_float())$unsqueeze(2),
  ym = torch_tensor(replace_na((raw$miss_in[m] - miss_m) / miss_s, 0), dtype = torch_float())$unsqueeze(2),
  mm = torch_tensor(as.numeric(!is.na(raw$miss_in[m])), dtype = torch_float())$unsqueeze(2))
dtr <- mk(tr); dte <- mk(!tr)

n_batter <- max(raw$b_idx)
net <- nn_module("contactnet",
  initialize = function(nb, nf, ed) {
    self$emb <- nn_embedding(nb, ed); self$fc1 <- nn_linear(nf + ed, 64)
    self$fc2 <- nn_linear(64, 32); self$drop <- nn_dropout(0.1); self$out <- nn_linear(32, 3) },
  forward = function(x, b) {
    h <- torch_cat(list(x, self$emb(b)), dim = 2)
    h <- self$drop(nnf_relu(self$fc1(h))); h <- nnf_relu(self$fc2(h)); self$out(h) }
)(n_batter, length(CFEATS), 6)

opt <- optim_adam(net$parameters, lr = 2e-3, weight_decay = 1e-5)
n <- dtr$x$size(1); bs <- 8192; best <- Inf; patience <- 0
loss_fn <- function(o, d, idx) {
  nnf_binary_cross_entropy_with_logits(o[, 1, drop = FALSE], d$yw[idx, , drop = FALSE]) +
  torch_sum(d$mb[idx,,drop=FALSE] * (o[,2,drop=FALSE] - d$yb[idx,,drop=FALSE])^2) / (torch_sum(d$mb[idx,,drop=FALSE]) + 1) +
  torch_sum(d$mm[idx,,drop=FALSE] * (o[,3,drop=FALSE] - d$ym[idx,,drop=FALSE])^2) / (torch_sum(d$mm[idx,,drop=FALSE]) + 1)
}
for (epoch in 1:80) {
  net$train(); perm <- sample(n)
  for (i in seq(1, n, by = bs)) { idx <- perm[i:min(i + bs - 1, n)]; opt$zero_grad()
    loss <- loss_fn(net(dtr$x[idx, ], dtr$b[idx]), dtr, idx)
    loss$backward(); opt$step() }
  net$eval()
  with_no_grad({ vl <- as.numeric(loss_fn(net(dte$x, dte$b), dte, seq_len(dte$x$size(1)))) })
  if (vl < best - 1e-4) { best <- vl; patience <- 0
    best_state <- lapply(net$state_dict(), function(t) t$clone())
  } else { patience <- patience + 1; if (patience >= 10) break }
}
net$load_state_dict(best_state); net$eval()
cat("stopped epoch", epoch, " best test loss:", round(best, 4), "\n")

with_no_grad({ o <- as.matrix(net(dte$x, dte$b)) })
te <- raw[!tr, ]
p <- 1/(1+exp(-o[,1])); y <- te$whiff
np <- as.numeric(sum(y)); nn2 <- as.numeric(sum(y==0)); r <- rank(p)
cat(sprintf("whiff head:  AUC %.3f (base rate %.3f)\n",
            (sum(r[y==1]) - np*(np+1)/2)/(np*nn2), mean(y)))
bmask <- !is.na(te$bias_deg)
cat(sprintf("bias head:   cor %.3f on contact (n=%d)\n",
            cor(o[bmask,2], te$bias_deg[bmask]), sum(bmask)))
mmask <- !is.na(te$miss_in)
cat(sprintf("miss head:   cor %.3f on whiffs (n=%d)\n",
            cor(o[mmask,3], te$miss_in[mmask]), sum(mmask)))

# sanity: does the bias field say "under the high fastball, over the low bender"?
probe <- function(lbl, px, zrel, spd, fx, fz) {
  x <- (c(px, zrel, 1.6+zrel*1.8, spd, fx, fz, 1, 1, 1, 1) - fmean) / fsd
  with_no_grad({ v <- as.numeric(net(torch_tensor(matrix(x,1), dtype=torch_float()),
                                     torch_tensor(300L, dtype=torch_long()))) })
  cat(sprintf("  %-28s P(whiff) %.2f  bias %+.1f deg  miss %.1f in\n", lbl,
              1/(1+exp(-v[1])), v[2]*bias_s + bias_m, v[3]*miss_s + miss_m))
}
cat("\nprobe (generic hitter):\n")
probe("high 4-seam 97 (in zone)",  0, 0.85, 97,  -0.6, 1.4)
probe("low slider 85 (in zone)",   0, 0.15, 85,   0.5, 0.1)
probe("low curve 79 (below zone)", 0, -0.2, 79,   0.7, -0.7)

torch_save(net, file.path(model_dir, "contact_net.pt"))
saveRDS(list(fmean = fmean, fsd = fsd, bias_m = bias_m, bias_s = bias_s,
             miss_m = miss_m, miss_s = miss_s, feats = CFEATS),
        file.path(model_dir, "contact_meta.rds"))
cat("\nsaved contact_net.pt + contact_meta.rds\n")
