# 01_download_data.R
# Download Statcast per-pitch data (all pitches, swings AND takes) from Baseball
# Savant for the swing emulation project. Bat tracking targets:
#   bat_speed, swing_length            -> available from May 2024
#   attack_angle, attack_direction,
#   swing_path_tilt, intercept_*       -> available from 2025
# We pull 2025 + 2026-to-date so every swing has all five path metrics.
#
# Savant's CSV endpoint caps a query at 25,000 rows, so we chunk by 3-day
# windows and cache each chunk to data/raw/ before combining per season.

library(tidyverse)

data_dir <- file.path("Swing Emulation", "data")
raw_dir  <- file.path(data_dir, "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

savant_url <- function(start_date, end_date) {
  paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
    "&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=",
    "&hfGT=R%7C",                       # regular season only
    "&hfC=&hfSea=&hfSit=&player_type=batter&hfOuts=&opponent=",
    "&pitcher_throws=&batter_stands=&hfSA=",
    "&game_date_gt=", start_date,
    "&game_date_lt=", end_date,
    "&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfPull=",
    "&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name",
    "&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc",
    "&min_pas=0&type=details&"
  )
}

download_chunk <- function(start_date, end_date, raw_dir) {
  cache_file <- file.path(raw_dir, paste0("savant_", start_date, "_", end_date, ".rds"))
  if (file.exists(cache_file)) {
    message("cached:  ", start_date, " to ", end_date)
    return(invisible(cache_file))
  }
  df <- tryCatch(
    read_csv(savant_url(start_date, end_date), show_col_types = FALSE,
             guess_max = 30000),
    error = function(e) {
      warning("FAILED ", start_date, " to ", end_date, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(df)) return(invisible(NULL))
  if (nrow(df) >= 25000) {
    warning("Chunk ", start_date, " to ", end_date,
            " hit the 25,000-row cap -- rows were dropped. Use smaller windows.")
  }
  message(sprintf("fetched: %s to %s (%s rows)", start_date, end_date,
                  format(nrow(df), big.mark = ",")))
  if (nrow(df) > 0) saveRDS(df, cache_file)
  Sys.sleep(2)  # be polite to Savant
  invisible(cache_file)
}

download_season <- function(year, raw_dir,
                            season_start = as.Date(paste0(year, "-03-15")),
                            season_end   = min(as.Date(paste0(year, "-11-05")),
                                               Sys.Date() - 1)) {
  starts <- seq(season_start, season_end, by = "3 days")
  for (s in starts) {
    s <- as.Date(s, origin = "1970-01-01")
    e <- min(s + 2, season_end)
    download_chunk(format(s), format(e), raw_dir)
  }
}

combine_season <- function(year, raw_dir, data_dir) {
  files <- list.files(raw_dir, pattern = paste0("^savant_", year), full.names = TRUE)
  season <- files |>
    map(readRDS) |>
    list_rbind() |>
    distinct(game_pk, at_bat_number, pitch_number, .keep_all = TRUE) |>
    arrange(game_date, game_pk, at_bat_number, pitch_number)
  out <- file.path(data_dir, paste0("savant_", year, ".rds"))
  saveRDS(season, out)
  message(sprintf("%d: %s pitches -> %s", year,
                  format(nrow(season), big.mark = ","), out))
  invisible(season)
}

if (sys.nframe() == 0) {  # run only when executed as a script
  download_season(2025, raw_dir)
  download_season(2026, raw_dir)
  combine_season(2025, raw_dir, data_dir)
  combine_season(2026, raw_dir, data_dir)
}
