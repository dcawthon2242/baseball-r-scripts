# 06_player_swing_surface.R
# "A swing independent of location" = a player's swing metric as a SURFACE over
# the strike zone. We bin each hitter's competitive swings into a 3x3 zone grid
# (horizontal relative to handedness, vertical relative to their own zone) and
# summarize the swing in each cell. The whole 3x3 field is the location-
# independent signature; a single cell is "this hitter vs a pitch there".
#
# Demo hitters: Freddie Freeman (LHH, smooth line-drive) vs Jose Altuve (RHH,
# famous low-ball golfer) -- do their surfaces actually differ in shape?

suppressMessages(library(tidyverse))
data_dir <- file.path("Swing Emulation", "data")
out_dir  <- file.path("Swing Emulation", "output")

sw <- readRDS(file.path(data_dir, "swings_all.rds"))

pick <- function(sw, pat, side, who) {
  sw |> filter(str_detect(player_name, regex(pat, ignore_case = TRUE)), stand == side) |>
    mutate(who = who)
}

dat <- bind_rows(
  pick(sw, "Freeman", "L", "Freddie Freeman (LHH)"),
  pick(sw, "Altuve",  "R", "Jose Altuve (RHH)")
) |>
  filter(abs(plate_x) < 1.1, plate_z_rel > -0.15, plate_z_rel < 1.15) |>
  mutate(
    px_in = if_else(stand == "R", -plate_x, plate_x),   # higher = more inside
    h = cut(px_in, c(-Inf, -0.3, 0.3, Inf), labels = c("away", "middle", "inside")),
    v = cut(plate_z_rel, c(-Inf, 1/3, 2/3, Inf), labels = c("low", "middle", "high"))
  )

agg <- dat |>
  group_by(who, v, h) |>
  summarise(n = n(),
            attack_angle = mean(attack_angle),
            bat_speed    = mean(bat_speed),
            tilt         = mean(swing_path_tilt),
            depth_in     = mean(intercept_y),
            .groups = "drop")

cat("\n=== attack angle (deg) by zone cell ===\n")
agg |> select(who, v, h, attack_angle, n) |>
  pivot_wider(names_from = h, values_from = attack_angle, id_cols = c(who, v)) |>
  arrange(who, factor(v, c("high", "middle", "low"))) |> print(n = Inf)

surface_plot <- function(agg, metric, lab, palette) {
  ggplot(agg, aes(h, v, fill = .data[[metric]])) +
    geom_tile(color = "white", linewidth = 1.2) +
    geom_text(aes(label = sprintf("%.1f\n(n=%d)", .data[[metric]], n)), size = 3.2) +
    facet_wrap(~who) +
    scale_x_discrete(limits = c("inside", "middle", "away")) +
    scale_y_discrete(limits = c("low", "middle", "high")) +
    scale_fill_viridis_c(option = palette, name = lab) +
    labs(title = paste0(lab, " over the strike zone"),
         subtitle = "each hitter's location-independent signature = the whole 3x3 field",
         x = "horizontal (batter's view)", y = "pitch height") +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank())
}

ggsave(file.path(out_dir, "surface_attack_angle.png"),
       surface_plot(agg, "attack_angle", "attack angle (deg)", "C"),
       width = 9, height = 4.6, dpi = 150)
ggsave(file.path(out_dir, "surface_bat_speed.png"),
       surface_plot(agg, "bat_speed", "bat speed (mph)", "D"),
       width = 9, height = 4.6, dpi = 150)

cat("\nsurfaces saved to", out_dir, "\n")
