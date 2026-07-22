# 11_embedding_map.R
# Visualize the learned player-embedding space (from 10) as a 2D PCA map,
# colored by each hitter's mean bat speed. If the embedding captured real swing
# structure, similar hitters sit near each other and bat speed varies smoothly.

suppressMessages({ library(tidyverse) })
has_repel <- requireNamespace("ggrepel", quietly = TRUE)
data_dir  <- file.path("Swing Emulation", "data")
model_dir <- file.path("Swing Emulation", "models")
out_dir   <- file.path("Swing Emulation", "output")

meta <- readRDS(file.path(model_dir, "player_embed_meta.rds"))
key  <- meta$batter_key
E    <- meta$emb

prof <- readRDS(file.path(data_dir, "swings_all.rds")) |>
  group_by(batter) |>
  summarise(bat_speed = mean(bat_speed, na.rm = TRUE),
            attack_angle = mean(attack_angle, na.rm = TRUE), n = n(), .groups = "drop")

pc <- prcomp(E, scale. = TRUE)$x[, 1:2]
df <- key |>
  mutate(PC1 = pc[, 1], PC2 = pc[, 2]) |>
  left_join(prof, by = "batter") |>
  mutate(last = str_extract(player_name, "^[^,]+"))

stars <- c("Judge", "Altuve", "Arraez", "Freeman", "Ohtani", "Buxton",
           "Hinds", "Turner", "Witt", "Rodríguez", "Kwan", "Schanuel")
lab <- df |> filter(str_detect(player_name, str_c(stars, collapse = "|")), n > 300) |>
  group_by(last) |> slice_max(n, n = 1) |> ungroup()

lab_layer <- if (has_repel) {
  ggrepel::geom_text_repel(data = lab, aes(label = last), size = 3.4,
                           max.overlaps = 20, min.segment.length = 0, seed = 1)
} else {
  geom_text(data = lab, aes(label = last), size = 3.4, vjust = -0.8)
}

ggplot(df, aes(PC1, PC2)) +
  geom_point(aes(color = bat_speed, size = n), alpha = 0.75) +
  lab_layer +
  scale_color_viridis_c(option = "C", name = "mean\nbat speed") +
  scale_size_continuous(range = c(0.6, 4), guide = "none") +
  labs(title = "The learned swing-embedding space (696 hitters)",
       subtitle = "PCA of the player vectors; nearby = similar swing, color = bat speed",
       x = "embedding PC1", y = "embedding PC2") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "embedding_map.png"), width = 9, height = 6.2, dpi = 150)
cat("saved -> embedding_map.png\n")
