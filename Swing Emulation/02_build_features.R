# 02_build_features.R
# Build the modeling table for the swing emulation project.
#
# Input:  Swing Emulation/data/savant_2025.rds, savant_2026.rds (from 01)
# Output: Swing Emulation/data/swings_all.rds   -- every competitive swing
#         Swing Emulation/data/model_data.rds   -- in-zone competitive swings
#
# Targets (Savant bat tracking, per swing):
#   bat_speed, swing_length, attack_angle, attack_direction, swing_path_tilt
# Also carried: intercept_ball_minus_batter_pos_x/y_inches (timing/depth).
#
# Pitch features come from the 9-parameter trajectory (release pos, velocity
# and acceleration at y = 50 ft). The 9 params fully determine the flight, so
# we feed them plus physics-derived features (plate crossing velocity and
# approach angles) rather than sampling points along the path.

library(tidyverse)

data_dir <- file.path("Swing Emulation", "data")

SWING_DESC <- c("hit_into_play", "foul", "foul_tip",
                "swinging_strike", "swinging_strike_blocked")
BUNT_DESC  <- c("foul_bunt", "missed_bunt", "bunt_foul_tip")

# Savant does not expose its competitive-swing flag in the CSV. Proxy: drop
# bunts and drop bat_speed < 55 mph (check/defensive swings live in that tail;
# MLB avg competitive swing is ~71 mph). Tune after looking at the histogram.
MIN_COMPETITIVE_BAT_SPEED <- 55

# Plate-crossing kinematics from the 9-parameter constant-acceleration fit.
# Statcast reports velocity/acceleration at y0 = 50 ft; the front of home
# plate is y = 17/12 ft. Standard equations (Nathan/Kagan).
add_plate_kinematics <- function(df, y0 = 50, yf = 17 / 12) {
  df |>
    mutate(
      vy_f          = -sqrt(vy0^2 + 2 * ay * (yf - y0)),
      t_flight_50   = (vy_f - vy0) / ay,
      vx_f          = vx0 + ax * t_flight_50,
      vz_f          = vz0 + az * t_flight_50,
      velo_at_plate = sqrt(vx_f^2 + vy_f^2 + vz_f^2) * 0.6818182,  # ft/s -> mph
      vaa           = atan2(vz_f, -vy_f) * 180 / pi,  # vertical approach angle (deg, negative = downward)
      haa           = atan2(vx_f, -vy_f) * 180 / pi   # horizontal approach angle (deg)
    )
}

build_swings <- function(seasons = c(2025, 2026), data_dir) {
  raw <- seasons |>
    map(\(yr) readRDS(file.path(data_dir, paste0("savant_", yr, ".rds")))) |>
    list_rbind()

  swings <- raw |>
    filter(description %in% SWING_DESC,
           !description %in% BUNT_DESC,
           !(description == "hit_into_play" & str_detect(coalesce(des, ""), regex("bunt", ignore_case = TRUE))),
           !is.na(bat_speed), !is.na(swing_length),
           !is.na(attack_angle), !is.na(attack_direction), !is.na(swing_path_tilt),
           !is.na(vx0), !is.na(plate_x), !is.na(sz_top),
           bat_speed >= MIN_COMPETITIVE_BAT_SPEED) |>
    add_plate_kinematics() |>
    mutate(
      season   = year(game_date),
      is_rhb   = as.integer(stand == "R"),
      is_rhp   = as.integer(p_throws == "R"),
      whiff    = as.integer(str_detect(description, "swinging_strike")),
      # rulebook zone, personalized vertically to the batter
      in_zone  = abs(plate_x) <= 0.83 & plate_z >= sz_bot & plate_z <= sz_top,
      # location relative to the batter's own zone (helps the model generalize
      # across batter heights)
      plate_z_rel = (plate_z - sz_bot) / (sz_top - sz_bot)
    ) |>
    select(
      # ids / context
      game_pk, game_date, season, batter, player_name, pitcher,
      stand, p_throws, is_rhb, is_rhp, balls, strikes, pitch_type, description, whiff, in_zone,
      # 9-parameter trajectory
      release_pos_x, release_pos_z, release_extension,
      vx0, vy0, vz0, ax, ay, az,
      # derived pitch features
      release_speed, plate_x, plate_z, plate_z_rel, pfx_x, pfx_z,
      velo_at_plate, vaa, haa, t_flight_50, sz_top, sz_bot,
      release_spin_rate, spin_axis,
      # swing targets
      bat_speed, swing_length, attack_angle, attack_direction, swing_path_tilt,
      intercept_x = intercept_ball_minus_batter_pos_x_inches,
      intercept_y = intercept_ball_minus_batter_pos_y_inches
    )

  swings
}

if (sys.nframe() == 0) {
  swings <- build_swings(c(2025, 2026), data_dir)
  saveRDS(swings, file.path(data_dir, "swings_all.rds"))

  model_data <- filter(swings, in_zone)
  saveRDS(model_data, file.path(data_dir, "model_data.rds"))

  cat(sprintf("competitive swings: %s  |  in-zone: %s (%.1f%%)\n",
              format(nrow(swings), big.mark = ","),
              format(nrow(model_data), big.mark = ","),
              100 * nrow(model_data) / nrow(swings)))
  cat("\nswings by season:\n")
  print(count(swings, season, in_zone))
  cat("\ntarget summaries (in-zone):\n")
  model_data |>
    select(bat_speed, swing_length, attack_angle, attack_direction, swing_path_tilt) |>
    summary() |>
    print()
}
