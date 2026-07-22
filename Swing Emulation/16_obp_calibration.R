# 16_obp_calibration.R
# OBP calibration (#1): validate script 04's reconstruction geometry against the
# REAL tracked sweet-spot path in the OpenBiomechanics landmarks (677 swings,
# 357 Hz, meters). Checks the three assumptions:
#   (a) arc radius ~ 2.75 ft, constant  -> fit radius of curvature at contact
#   (b) speed ramps 30%->100% linearly  -> mean normalized speed profile
#   (c) the swing is planar near contact -> out-of-plane residual via SVD
# Contact is proxied by peak sweet-spot speed (~= bat speed at contact).

suppressMessages({ library(tidyverse) })
d <- file.path("Swing Emulation", "data", "obp")
out_dir <- file.path("Swing Emulation", "output")
FT <- 3.28084  # meters -> feet

lm <- read_csv(file.path(d, "landmarks", "landmarks.csv"), show_col_types = FALSE) |>
  select(session_swing, time, sweet_spot_x, sweet_spot_y, sweet_spot_z) |>
  mutate(across(starts_with("sweet_spot"), ~ .x * FT))

analyze_swing <- function(s) {
  s <- s |> filter(if_all(c(sweet_spot_x, sweet_spot_y, sweet_spot_z), is.finite)) |> arrange(time)
  n <- nrow(s); if (n < 40) return(NULL)
  P <- as.matrix(s[, c("sweet_spot_x", "sweet_spot_y", "sweet_spot_z")]); t <- s$time
  # finite-diff speed, lightly smoothed, to find contact = peak speed
  v <- rbind(NA, (P[-1, ] - P[-n, ]) / (t[-1] - t[-n]))
  spd <- sqrt(rowSums(v^2))
  spd_s <- as.numeric(stats::filter(spd, rep(1/5, 5))); spd_s[is.na(spd_s)] <- spd[is.na(spd_s)]
  ci <- which.max(spd_s); tc <- t[ci]
  # 40 ms window before contact: fit cubic per axis, read v & a at contact
  w <- which(t >= tc - 0.040 & t <= tc); if (length(w) < 6) return(NULL)
  tt <- t[w] - tc
  co <- map(1:3, ~ coef(lm.fit(cbind(1, tt, tt^2, tt^3), P[w, .x])))
  vel <- map_dbl(co, 2); acc <- map_dbl(co, ~ 2 * .x[3])
  cross <- c(vel[2]*acc[3] - vel[3]*acc[2], vel[3]*acc[1] - vel[1]*acc[3], vel[1]*acc[2] - vel[2]*acc[1])
  R_curv <- sum(vel^2)^1.5 / sqrt(sum(cross^2))
  contact_speed <- sqrt(sum(vel^2))                 # ft/s
  attack <- atan2(vel[3], sqrt(vel[1]^2 + vel[2]^2)) * 180/pi
  # planarity over the 40 ms window
  Pc <- scale(P[w, ], scale = FALSE); sv <- svd(Pc)$d
  planarity <- sv[3] / sv[1]
  # speed ramp: 150 ms window, speed / contact speed vs time-to-contact
  wr <- which(t >= tc - 0.150 & t <= tc)
  ramp <- tibble(rel_t = t[wr] - tc, ratio = spd_s[wr] / contact_speed)
  list(stat = tibble(R_curv = R_curv, contact_speed_mph = contact_speed * 0.681818,
                     attack = attack, planarity = planarity), ramp = ramp)
}

res <- lm |> group_split(session_swing) |> map(analyze_swing) |> compact()
stats <- map_dfr(res, "stat") |> filter(is.finite(R_curv), R_curv < 20, R_curv > 0.5)
ramp  <- map_dfr(res, "ramp")

cat("=== script 04 assumption checks (n =", nrow(stats), "swings) ===\n")
cat(sprintf("(a) arc radius at contact:  median %.2f ft  [IQR %.2f-%.2f]  (assumed 2.75)\n",
            median(stats$R_curv), quantile(stats$R_curv, .25), quantile(stats$R_curv, .75)))
cat(sprintf("(c) planarity (out/in-plane): median %.3f  (0 = perfectly planar)\n",
            median(stats$planarity)))
cat(sprintf("    contact speed: median %.1f mph  (sanity vs OBP bat speed ~66)\n",
            median(stats$contact_speed_mph)))

# (b) mean speed ramp vs the assumed linear 30%->100%
ramp_prof <- ramp |> filter(rel_t >= -0.15, rel_t <= 0) |>
  mutate(bin = round(rel_t / 0.005) * 0.005) |>
  group_by(bin) |> summarise(ratio = median(ratio, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(stats, aes(R_curv)) +
  geom_histogram(bins = 40, fill = "#1D9E75", color = NA) +
  geom_vline(xintercept = 2.75, color = "#D85A30", linewidth = 1) +
  annotate("text", x = 2.9, y = Inf, vjust = 2, hjust = 0, label = "assumed 2.75 ft", color = "#D85A30") +
  coord_cartesian(xlim = c(0, 8)) +
  labs(title = "(a) real arc radius at contact vs the reconstruction's fixed 2.75 ft",
       x = "radius of curvature (ft)", y = "swings") + theme_minimal(base_size = 12)

p2 <- ggplot(ramp_prof, aes(bin * 1000, ratio)) +
  geom_line(color = "#1D9E75", linewidth = 1.1) +
  geom_abline(intercept = 1, slope = (1 - 0.30) / 150, color = "#D85A30", linewidth = 1, linetype = 2) +
  annotate("text", x = -140, y = 0.34, hjust = 0, color = "#D85A30", label = "assumed linear 30%->100%") +
  labs(title = "(b) real speed ramp (median) vs the assumed linear ramp",
       x = "time to contact (ms)", y = "sweet-spot speed / contact speed") +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "obp_calib_radius.png"), p1, width = 8, height = 4.5, dpi = 150)
ggsave(file.path(out_dir, "obp_calib_speedramp.png"), p2, width = 8, height = 4.5, dpi = 150)
saveRDS(stats, file.path(d, "..", "..", "models", "obp_calibration.rds") |> normalizePath(mustWork = FALSE))
cat("\nsaved -> obp_calib_radius.png, obp_calib_speedramp.png\n")
