# 15_swing_decision.R
# The swing/take extension: does the hitter swing? A torch classifier with its
# OWN batter embedding (swing-DECISION identity differs from swing-MECHANICS
# identity), trained on ALL pitches (swings + takes) with location, speed,
# movement, and count. Output: P(swing). Demo: swing-probability surface over
# the zone for a disciplined vs an aggressive hitter (the chase map).

suppressMessages({ library(tidyverse); library(torch) })
data_dir  <- file.path("Swing Emulation", "data")
model_dir <- file.path("Swing Emulation", "models")
out_dir   <- file.path("Swing Emulation", "output")
set.seed(7); torch_manual_seed(7)

SWING <- c("hit_into_play","foul","foul_tip","swinging_strike","swinging_strike_blocked",
           "foul_bunt","missed_bunt","bunt_foul_tip")
TAKE  <- c("ball","called_strike","blocked_ball","hit_by_pitch","pitchout")
FEATS <- c("px_in","plate_z_rel","plate_x","plate_z","release_speed","pfx_x","pfx_z","balls","strikes","is_rhp")

raw <- map(c(2025, 2026), ~ readRDS(file.path(data_dir, paste0("savant_", .x, ".rds")))) |> list_rbind()

pit <- raw |>
  filter(description %in% c(SWING, TAKE), !is.na(plate_x), !is.na(sz_top), !is.na(release_speed)) |>
  transmute(
    batter, player_name, stand, game_pk,
    swing = as.integer(description %in% SWING),
    px_in = if_else(stand == "R", -plate_x, plate_x),
    plate_x, plate_z,
    plate_z_rel = (plate_z - sz_bot) / (sz_top - sz_bot),
    release_speed, pfx_x, pfx_z, balls, strikes, is_rhp = as.integer(p_throws == "R")) |>
  filter(if_all(all_of(FEATS), ~ !is.na(.x))) |>
  add_count(batter, name = "n_bat") |> filter(n_bat >= 100) |>
  mutate(b_idx = as.integer(factor(batter)),
         split = case_when(game_pk %% 5 %in% 0:2 ~ "train", game_pk %% 5 == 3 ~ "valid", TRUE ~ "test"))

bkey <- pit |> group_by(b_idx, batter) |>
  summarise(player_name = first(player_name), stand = names(which.max(table(stand))), .groups = "drop") |>
  arrange(b_idx)
n_batter <- max(pit$b_idx)
cat("pitches:", nrow(pit), " hitters:", n_batter, " swing rate:", round(mean(pit$swing), 3), "\n")

tr <- pit$split == "train"
fmean <- map_dbl(FEATS, ~ mean(pit[[.x]][tr])); fsd <- map_dbl(FEATS, ~ sd(pit[[.x]][tr]))
names(fmean) <- names(fsd) <- FEATS
Xz <- scale(as.matrix(pit[, FEATS]), center = fmean, scale = fsd)

mk <- function(m) list(x = torch_tensor(Xz[m, ], dtype = torch_float()),
                       b = torch_tensor(pit$b_idx[m], dtype = torch_long()),
                       y = torch_tensor(pit$swing[m], dtype = torch_float())$unsqueeze(2))
dtr <- mk(tr); dva <- mk(pit$split == "valid"); dte <- mk(pit$split == "test")

net <- nn_module("decnet",
  initialize = function(nb, nf, ed) {
    self$emb <- nn_embedding(nb, ed); self$fc1 <- nn_linear(nf + ed, 64)
    self$fc2 <- nn_linear(64, 32); self$drop <- nn_dropout(0.15); self$out <- nn_linear(32, 1) },
  forward = function(x, b) { h <- torch_cat(list(x, self$emb(b)), dim = 2)
    h <- self$drop(nnf_relu(self$fc1(h))); h <- nnf_relu(self$fc2(h)); self$out(h) }
)(n_batter, length(FEATS), 8)

opt <- optim_adam(net$parameters, lr = 2e-3, weight_decay = 1e-5)
n <- dtr$x$size(1); bs <- 8192; best <- Inf; patience <- 0
for (epoch in 1:60) {
  net$train(); perm <- sample(n)
  for (i in seq(1, n, by = bs)) { idx <- perm[i:min(i + bs - 1, n)]; opt$zero_grad()
    loss <- nnf_binary_cross_entropy_with_logits(net(dtr$x[idx, ], dtr$b[idx]), dtr$y[idx, ])
    loss$backward(); opt$step() }
  net$eval(); with_no_grad({ vl <- as.numeric(nnf_binary_cross_entropy_with_logits(net(dva$x, dva$b), dva$y)) })
  if (vl < best - 1e-4) { best <- vl; patience <- 0; best_state <- lapply(net$state_dict(), function(t) t$clone())
  } else { patience <- patience + 1; if (patience >= 8) break } }
net$load_state_dict(best_state); net$eval()

with_no_grad({ p <- as.numeric(torch_sigmoid(net(dte$x, dte$b))) })
y <- pit$swing[pit$split == "test"]
logloss <- -mean(y * log(p + 1e-9) + (1 - y) * log(1 - p + 1e-9))
auc <- { r <- rank(p); np <- as.numeric(sum(y)); nn <- as.numeric(sum(y == 0))
         (sum(r[y == 1]) - np * (np + 1) / 2) / (np * nn) }
cat(sprintf("\ntest AUC %.3f  logloss %.3f  (base rate %.3f)\n", auc, logloss, mean(y)))

torch_save(net, file.path(model_dir, "swing_decision_net.pt"))
saveRDS(list(bkey = bkey, fmean = fmean, fsd = fsd, auc = auc), file.path(model_dir, "swing_decision_meta.rds"))

# ---- chase maps: P(swing) over the zone for contrasting hitters ----
grid <- expand_grid(plate_x = seq(-1.5, 1.5, 0.05), plate_z = seq(1.3, 4.0, 0.05))
prob_surface <- function(pat, side) {
  row <- filter(bkey, str_detect(player_name, regex(pat, TRUE)), stand == side)[1, ]
  g <- grid |> mutate(
    px_in = if (side == "R") -plate_x else plate_x,
    plate_z_rel = (plate_z - 1.6) / 1.8,
    release_speed = 89, pfx_x = 0, pfx_z = 2, balls = 1, strikes = 1, is_rhp = 1L)
  Xz <- scale(as.matrix(g[, FEATS]), center = fmean, scale = fsd)
  with_no_grad({ pr <- as.numeric(torch_sigmoid(net(torch_tensor(Xz, dtype = torch_float()),
                    torch_tensor(rep(row$b_idx, nrow(g)), dtype = torch_long())))) })
  mutate(g, p_swing = pr, who = paste0(row$player_name, " (", side, ")"))
}
maps <- bind_rows(prob_surface("Soto, Juan", "L"), prob_surface("ez, Javier", "R"))

ggplot(maps, aes(plate_x, plate_z, fill = p_swing)) +
  geom_raster(interpolate = TRUE) +
  annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.6, ymax = 3.4, fill = NA, color = "white", linewidth = 0.6) +
  facet_wrap(~who) + coord_equal() +
  scale_fill_viridis_c(option = "B", name = "P(swing)", limits = c(0, 1)) +
  labs(title = "Swing-decision surface (1-1 count, 89 mph): discipline vs aggression",
       subtitle = "P(swing) over the zone (catcher's view); white box = rulebook strike zone",
       x = "plate_x (ft)", y = "plate_z (ft)") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "swing_decision_maps.png"), width = 9.5, height = 5, dpi = 150)
cat("saved -> swing_decision_maps.png\n")
