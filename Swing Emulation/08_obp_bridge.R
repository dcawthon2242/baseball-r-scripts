# 08_obp_bridge.R
# Load the OpenBiomechanics hitting data, harmonize its swing metrics to
# Statcast conventions, and quantify how much the two populations OVERLAP.
#
# OBP ships computed metrics, so no C3D parsing is needed for 3 of the 5
# Statcast targets:
#   bat_speed        <- bat_speed_mph_contact_x         (direct)
#   attack_angle     <- attack_angle_contact_x          (direct; also derivable)
#   attack_direction <- atan2(sweet_spot_velo_y, _x)    (from the velocity vector)
# swing_path_tilt and swing_length still need the raw bat-marker path (C3D).
#
# Bridge conditioning variables both datasets have:
#   contact location (poi_x/z) and pitch speed (hittrax.pitch).
# CAUTION: OBP pitches come from a ~55-65 mph machine -- far below MLB velocity.

suppressMessages({ library(tidyverse) })
obp_dir  <- file.path("Swing Emulation", "data", "obp")
sc_dir   <- file.path("Swing Emulation", "data")
out_dir  <- file.path("Swing Emulation", "output")

download_obp <- function(obp_dir) {
  dir.create(obp_dir, recursive = TRUE, showWarnings = FALSE)
  base <- "https://raw.githubusercontent.com/drivelineresearch/openbiomechanics/main/baseball_hitting/data"
  files <- c("metadata.csv", "data_dictionary.csv",
             "poi/poi_metrics.csv", "poi/hittrax.csv")
  for (f in files) {
    dest <- file.path(obp_dir, basename(f))
    if (!file.exists(dest)) download.file(file.path(base, f), dest, quiet = TRUE, mode = "wb")
  }
}

load_obp <- function(obp_dir) {
  download_obp(obp_dir)
  meta <- read_csv(file.path(obp_dir, "metadata.csv"),   show_col_types = FALSE)
  poi  <- read_csv(file.path(obp_dir, "poi_metrics.csv"), show_col_types = FALSE)
  ht   <- read_csv(file.path(obp_dir, "hittrax.csv"),     show_col_types = FALSE)

  poi |>
    left_join(select(meta, session_swing, user, highest_playing_level,
                     hitter_side, bat_weight_oz, bat_length_in), by = "session_swing") |>
    left_join(select(ht, session_swing, pitch_speed = pitch, strike_zone,
                     poi_x, poi_y, poi_z, pitch_angle, la, bearing), by = "session_swing") |>
    transmute(
      session_swing, user, highest_playing_level, stand = hitter_side,
      # --- harmonized swing metrics (Statcast conventions) ---
      bat_speed        = bat_speed_mph_contact_x,
      attack_angle     = attack_angle_contact_x,
      attack_direction = atan2(sweet_spot_velo_mph_contact_y,
                               sweet_spot_velo_mph_contact_x) * 180 / pi,
      # --- bridge / context ---
      pitch_speed, strike_zone, poi_x, poi_z, pitch_angle, la,
      # --- a few biomechanics carried through for later ---
      x_factor = x_factor_fp_x, pelvis_av = pelvis_angular_velocity_swing_max_x,
      torso_av = torso_angular_velocity_swing_max_x, hand_speed = hand_speed_mag_max_x
    ) |>
    filter(pitch_speed > 30 | is.na(pitch_speed))   # drop machine-misread 0 mph
}

if (sys.nframe() == 0) {
  obp <- load_obp(obp_dir)
  saveRDS(obp, file.path(sc_dir, "obp_swings.rds"))

  sc <- readRDS(file.path(sc_dir, "swings_all.rds")) |>
    slice_sample(n = 60000) |>
    transmute(bat_speed, attack_angle, attack_direction,
              pitch_speed = release_speed, src = "Statcast (MLB)")
  both <- bind_rows(mutate(obp, src = "OBP (amateur)") |>
                      select(bat_speed, attack_angle, attack_direction, pitch_speed, src),
                    sc)

  # overlap coefficient (area shared by the two densities) per metric
  ovl <- function(a, b, n = 512) {
    a <- a[is.finite(a)]; b <- b[is.finite(b)]
    lo <- min(a, b); hi <- max(a, b)
    da <- density(a, from = lo, to = hi, n = n)$y
    db <- density(b, from = lo, to = hi, n = n)$y
    sum(pmin(da, db)) / sum(pmax(da, db))
  }
  cat("=== distribution overlap (0 = disjoint, 1 = identical) ===\n")
  for (m in c("bat_speed", "attack_angle", "attack_direction", "pitch_speed")) {
    o <- ovl(obp[[m]], sc[[m]])
    cat(sprintf("  %-17s overlap = %.2f\n", m, o))
  }

  both |>
    pivot_longer(-src, names_to = "metric", values_to = "val") |>
    filter(is.finite(val)) |>
    ggplot(aes(val, fill = src)) +
    geom_density(alpha = 0.5, color = NA) +
    facet_wrap(~metric, scales = "free", ncol = 2) +
    scale_fill_manual(values = c("OBP (amateur)" = "#D85A30", "Statcast (MLB)" = "#1D9E75")) +
    labs(title = "OBP vs Statcast: where do the swing populations overlap?",
         subtitle = "metrics map cleanly, but OBP pitch speed (machine) misses MLB range entirely",
         x = NULL, y = "density", fill = NULL) +
    theme_minimal(base_size = 12) + theme(legend.position = "top")
  ggsave(file.path(out_dir, "obp_vs_statcast_overlap.png"), width = 9, height = 6, dpi = 150)
  cat("\nsaved -> obp_vs_statcast_overlap.png ; obp_swings.rds (", nrow(obp), " swings )\n", sep = "")
}
