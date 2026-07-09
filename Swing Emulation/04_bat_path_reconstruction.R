# 04_bat_path_reconstruction.R
# Reconstruct an approximate 3D bat sweet-spot path from the five predicted
# bat-tracking metrics, and plot it against the incoming pitch trajectory.
#
# What is measured vs. inferred:
#   measured/predicted -- bat speed, swing length, attack angle, attack
#     direction, swing path tilt (all describe the bat near contact)
#   inferred -- the earlier part of the arc. We model the sweet spot as moving
#     on a circular arc (radius ~ arms + bat) inside a plane whose tilt matches
#     swing_path_tilt, ending at the contact point with the bat head traveling
#     in the measured attack direction at the measured speed, with total path
#     length equal to swing_length. A plausible shape, not tracking data.
#
# Statcast coordinates: origin at the point of home plate, +y toward the
# pitcher, +z up, +x toward the first-base side (catcher's right).

library(tidyverse)
library(xgboost)

model_dir <- file.path("Swing Emulation", "models")
out_dir   <- file.path("Swing Emulation", "output")
dir.create(out_dir, showWarnings = FALSE)

BAT_ARC_RADIUS <- 2.75  # ft, hands-to-sweet-spot pivot radius (approximation)

# Bat sweet-spot path ending at `contact` (c(x, y, z) ft).
# attack_angle (deg up from horizontal), attack_direction (deg toward pull),
# swing_path_tilt (deg plane vs ground), swing_length (ft), bat_speed (mph).
reconstruct_bat_path <- function(bat_speed, swing_length, attack_angle,
                                 attack_direction, swing_path_tilt,
                                 contact, stand = "R", n_points = 60) {
  alpha <- attack_angle * pi / 180
  delta <- attack_direction * pi / 180
  tau   <- max(swing_path_tilt * pi / 180, abs(alpha) + 1e-6)
  pull  <- if (stand == "R") -1 else 1  # RHH pulls toward third base (-x)

  # final bat-head direction: forward (+y), deflected toward pull and upward
  v_hat <- c(pull * sin(delta) * cos(alpha), cos(delta) * cos(alpha), sin(alpha))

  # in-plane unit vector w perpendicular to v_hat, pointing from contact back
  # toward the hands (up and toward the catcher). Its vertical angle beta is
  # set so the plane spanned by {v_hat, w} has tilt tau:
  # sin(tau)^2 = sin(alpha)^2 + sin(beta)^2
  sin_beta <- sqrt(pmax(sin(tau)^2 - sin(alpha)^2, 0))
  # horizontal direction of w: perpendicular to v_hat's horizontal projection,
  # signed so w points toward the catcher (-y)
  h <- c(v_hat[1], v_hat[2], 0) / sqrt(v_hat[1]^2 + v_hat[2]^2)
  perp <- c(-h[2], h[1], 0)
  if (perp[2] > 0) perp <- -perp
  w <- c(perp[1] * sqrt(1 - sin_beta^2), perp[2] * sqrt(1 - sin_beta^2), sin_beta)
  # orthogonalize against v_hat (guards small numeric drift), renormalize
  w <- w - sum(w * v_hat) * v_hat
  w <- w / sqrt(sum(w^2))

  theta  <- swing_length / BAT_ARC_RADIUS          # arc angle subtended
  center <- contact + BAT_ARC_RADIUS * w
  phi    <- seq(-theta, 0, length.out = n_points)

  pts <- map(phi, \(p) center + BAT_ARC_RADIUS * (-cos(p) * w + sin(p) * v_hat))
  path <- do.call(rbind, pts) |> as_tibble(.name_repair = ~ c("x", "y", "z"))

  # timestamps: speed ramps linearly from 30% to 100% of contact speed
  v_c   <- bat_speed * 1.466667                     # mph -> ft/s
  seg   <- swing_length / (n_points - 1)
  speed <- seq(0.3 * v_c, v_c, length.out = n_points)
  path$t <- c(0, cumsum(seg / ((head(speed, -1) + tail(speed, -1)) / 2)))
  path$t <- path$t - max(path$t)                    # t = 0 at contact
  path
}

# Pitch flight from the 9 parameters (measured at y0 = 50 ft) down to yf,
# returned with x/z relative to the plate-crossing point (caller shifts to
# plate_x/plate_z, which anchors the path without needing x/z at y = 50).
pitch_flight <- function(vx0, vy0, vz0, ax, ay, az,
                         y0 = 50, yf = 17 / 12, n_points = 60) {
  t_end <- (-sqrt(vy0^2 + 2 * ay * (yf - y0)) - vy0) / ay
  t <- seq(0, t_end, length.out = n_points)
  tibble(
    t = t - t_end,                                  # t = 0 at plate crossing
    x = vx0 * t + 0.5 * ax * t^2,
    y = y0 + vy0 * t + 0.5 * ay * t^2,
    z = vz0 * t + 0.5 * az * t^2
  ) |>
    mutate(x = x - last(x), z = z - last(z))
}

# Predict the five swing metrics for a pitch (one-row data frame with the
# feature columns from 03) and return the reconstructed path + pitch path.
emulate_swing <- function(pitch_row, model_dir, stand = "R") {
  features <- readRDS(file.path(model_dir, "pooled_features.rds"))
  pitch_row$is_rhb <- as.integer(stand == "R")
  X <- as.matrix(pitch_row[, features])

  targets <- c("bat_speed", "swing_length", "attack_angle",
               "attack_direction", "swing_path_tilt")
  preds <- map_dbl(targets, \(tg) {
    fit <- xgb.load(file.path(model_dir, paste0("pooled_", tg, ".xgb")))
    predict(fit, X)
  }) |> set_names(targets)

  contact <- c(pitch_row$plate_x, 17 / 12, pitch_row$plate_z)
  bat <- reconstruct_bat_path(preds["bat_speed"], preds["swing_length"],
                              preds["attack_angle"], preds["attack_direction"],
                              preds["swing_path_tilt"], contact, stand)

  ball <- pitch_flight(pitch_row$vx0, pitch_row$vy0, pitch_row$vz0,
                       pitch_row$ax, pitch_row$ay, pitch_row$az) |>
    mutate(x = x + pitch_row$plate_x, z = z + pitch_row$plate_z)

  list(preds = preds, bat = bat, ball = ball)
}

plot_emulated_swing <- function(em, title) {
  bat  <- mutate(em$bat, what = "bat sweet spot")
  ball <- mutate(em$ball, what = "pitch")

  side <- ggplot(mapping = aes(y, z, color = what)) +
    geom_path(data = ball, linewidth = 1) +
    geom_path(data = bat, linewidth = 1) +
    geom_point(data = slice_tail(bat, n = 1), size = 3) +
    coord_equal(xlim = c(-1, 12)) +
    labs(title = title, subtitle = "side view (catcher to the left)",
         x = "y: distance from plate (ft)", y = "height (ft)") +
    theme_minimal()

  top <- ggplot(mapping = aes(y, x, color = what)) +
    geom_path(data = ball, linewidth = 1) +
    geom_path(data = bat, linewidth = 1) +
    geom_point(data = slice_tail(bat, n = 1), size = 3) +
    coord_equal(xlim = c(-1, 12)) +
    labs(subtitle = "top view",
         x = "y: distance from plate (ft)", y = "x: first-base side (ft)") +
    theme_minimal()

  list(side = side, top = top)
}

if (sys.nframe() == 0) {
  data_dir <- file.path("Swing Emulation", "data")
  md <- readRDS(file.path(data_dir, "model_data.rds"))

  # two contrasting example pitches: a center-cut four-seamer and a low slider
  ff <- md |> filter(pitch_type == "FF", abs(plate_x) < 0.3,
                     plate_z_rel > 0.45, plate_z_rel < 0.55, release_speed > 94) |>
    slice(1)
  sl <- md |> filter(pitch_type == "SL", plate_z_rel < 0.25, release_speed > 84) |>
    slice(1)

  for (ex in list(list(row = ff, name = "95mph_four_seam_middle"),
                  list(row = sl, name = "slider_low"))) {
    em <- emulate_swing(ex$row, model_dir, stand = "R")
    cat("\n", ex$name, "predicted swing:\n"); print(round(em$preds, 1))
    pl <- plot_emulated_swing(em, paste("Average RHH vs", gsub("_", " ", ex$name)))
    ggsave(file.path(out_dir, paste0("swing_", ex$name, "_side.png")),
           pl$side, width = 8, height = 5, dpi = 150)
    ggsave(file.path(out_dir, paste0("swing_", ex$name, "_top.png")),
           pl$top, width = 8, height = 5, dpi = 150)
  }
  cat("\nplots saved to", out_dir, "\n")
}
