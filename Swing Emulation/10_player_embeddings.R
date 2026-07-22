# 10_player_embeddings.R
# Phase 2, model 2: a learned player-embedding net (the second half of the
# "both, compare" plan). One network for ALL hitters: batter -> learned vector,
# concatenated with the pitch (location + speed), through an MLP -> 5 swing
# metrics. Advantages over per-player GAMs: partial pooling (works for hitters
# with few swings) and a similarity space for free (KNN = the user's idea).
#
# Requires {torch}. Run 09 first for the GAM/pooled comparison numbers.

suppressMessages({ library(tidyverse); library(torch) })
data_dir  <- file.path("Swing Emulation", "data")
model_dir <- file.path("Swing Emulation", "models")
out_dir   <- file.path("Swing Emulation", "output")

set.seed(1)
torch_manual_seed(1)

METRICS <- c("bat_speed", "swing_length", "attack_angle",
             "attack_direction", "swing_path_tilt")
FEATS   <- c("px_in", "plate_z_rel", "plate_z", "release_speed", "is_rhp")

sw <- readRDS(file.path(data_dir, "swings_all.rds")) |>
  filter(abs(plate_x) < 1.3, plate_z_rel > -0.3, plate_z_rel < 1.3) |>
  mutate(px_in = if_else(stand == "R", -plate_x, plate_x)) |>
  filter(if_all(all_of(c(FEATS, METRICS)), ~ !is.na(.x))) |>
  add_count(batter, name = "n_bat") |>
  filter(n_bat >= 30) |>
  mutate(split = case_when(game_pk %% 5 %in% 0:2 ~ "train",
                           game_pk %% 5 == 3     ~ "valid",
                           TRUE                  ~ "test"),
         b_idx = as.integer(factor(batter)))

# one row per b_idx (switch hitters appear as both L and R -> take their modal
# stand) so batter_key row order == b_idx and stays aligned with the embedding
# weight matrix rows.
batter_key <- sw |>
  group_by(b_idx, batter) |>
  summarise(player_name = first(player_name),
            stand = names(which.max(table(stand))), .groups = "drop") |>
  arrange(b_idx)
n_batter <- max(sw$b_idx)
stopifnot(nrow(batter_key) == n_batter)
cat("hitters:", n_batter, " swings:", nrow(sw), "\n")

# standardize features & targets on train
tr <- sw$split == "train"
fmean <- map_dbl(FEATS, ~ mean(sw[[.x]][tr])); tmean <- map_dbl(METRICS, ~ mean(sw[[.x]][tr]))
fsd   <- map_dbl(FEATS, ~ sd(sw[[.x]][tr]));   tsd   <- map_dbl(METRICS, ~ sd(sw[[.x]][tr]))
names(fmean) <- names(fsd) <- FEATS; names(tmean) <- names(tsd) <- METRICS

Xz <- scale(as.matrix(sw[, FEATS]), center = fmean, scale = fsd)
Yz <- scale(as.matrix(sw[, METRICS]), center = tmean, scale = tsd)

mk <- function(mask) list(
  x = torch_tensor(Xz[mask, ], dtype = torch_float()),
  b = torch_tensor(sw$b_idx[mask], dtype = torch_long()),
  y = torch_tensor(Yz[mask, ], dtype = torch_float()))
dtr <- mk(tr); dva <- mk(sw$split == "valid"); dte <- mk(sw$split == "test")

emb_dim <- 12
net <- nn_module(
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
)(n_batter, length(FEATS), emb_dim, length(METRICS))

opt <- optim_adam(net$parameters, lr = 2e-3, weight_decay = 1e-4)
n <- dtr$x$size(1); bs <- 4096; best <- Inf; patience <- 0
for (epoch in 1:120) {
  net$train(); perm <- sample(n)
  for (i in seq(1, n, by = bs)) {
    idx <- perm[i:min(i + bs - 1, n)]
    opt$zero_grad()
    pred <- net(dtr$x[idx, ], dtr$b[idx])
    loss <- nnf_mse_loss(pred, dtr$y[idx, ])
    loss$backward(); opt$step()
  }
  net$eval()
  with_no_grad({ vl <- as.numeric(nnf_mse_loss(net(dva$x, dva$b), dva$y)) })
  if (vl < best - 1e-4) { best <- vl; patience <- 0; best_state <- lapply(net$state_dict(), function(t) t$clone())
  } else { patience <- patience + 1; if (patience >= 12) break }
}
net$load_state_dict(best_state); net$eval()
cat("stopped epoch", epoch, " best valid MSE(z):", round(best, 4), "\n")

# test R^2 per metric (unstandardized), vs predict-the-mean baseline
with_no_grad({ pz <- as.matrix(net(dte$x, dte$b)) })
pred <- sweep(sweep(pz, 2, tsd, "*"), 2, tmean, "+")
obs  <- as.matrix(sw[sw$split == "test", METRICS])
r2 <- 1 - colSums((obs - pred)^2) / colSums(sweep(obs, 2, tmean, "-")^2)
cat("\n=== embedding model held-out R^2 (pooled xgb from 03 in parens) ===\n")
pooled_r2 <- c(bat_speed = .125, swing_length = .386, attack_angle = .262,
               attack_direction = .374, swing_path_tilt = .359)
for (m in METRICS) cat(sprintf("  %-17s %.3f   (pooled %.3f)\n", m, r2[m], pooled_r2[m]))

# similarity space: nearest neighbours of star hitters (the KNN payoff)
E <- as.matrix(net$emb$weight)
En <- E / sqrt(rowSums(E^2))
sim <- En %*% t(En)
neighbours <- function(pat, side, k = 5) {
  i <- batter_key |> mutate(rn = row_number()) |>
    filter(str_detect(player_name, regex(pat, TRUE)), stand == side) |> slice(1) |> pull(rn)
  if (length(i) == 0) return(invisible())
  ord <- order(sim[i, ], decreasing = TRUE)[2:(k + 1)]
  cat("\nmost similar to", batter_key$player_name[i], ":\n")
  cat(paste0("  ", batter_key$player_name[ord], " (", round(sim[i, ord], 3), ")"), sep = "\n")
}
for (p in list(c("Altuve","R"), c("Judge","R"), c("Arraez","L"), c("Freeman","L")))
  neighbours(p[1], p[2])

torch_save(net, file.path(model_dir, "player_embed_net.pt"))
saveRDS(list(batter_key = batter_key, emb = E, fmean = fmean, fsd = fsd,
             tmean = tmean, tsd = tsd, r2 = r2), file.path(model_dir, "player_embed_meta.rds"))
cat("\nsaved embedding model + metadata\n")
