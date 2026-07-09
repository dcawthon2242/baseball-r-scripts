library(ggplot2)
library(dplyr)

plot_deception_histogram <- function(pitcher_name) {
  
  dataset <- Deception25 %>%
    filter(Pitcher == pitcher_name, !is.na(correct_pct)) %>%
    mutate(
      deception_pct = 100 - correct_pct  # Higher = more deceptive
      )
  
  # Global median deception
  global_median <- median(dataset$deception_pct, na.rm = TRUE)
  
  # Mean deception by pitch type
  pitch_averages <- dataset %>%
    group_by(TaggedPitchType) %>%
    summarise(mean_deception = mean(deception_pct, na.rm = TRUE)) %>%
    ungroup()
  
  # Plot smoothed density curves
  ggplot(dataset, aes(x = deception_pct)) +
    geom_density(fill = "steelblue", alpha = 0.6) +
    facet_wrap(~ TaggedPitchType, scales = "free_y") +
    geom_vline(data = pitch_averages, aes(xintercept = mean_deception), 
               linetype = "solid", color = "black", linewidth = 1.2) +
    geom_vline(xintercept = global_median, 
               linetype = "dashed", color = "red", linewidth = 1) +
    labs(
      title = paste("Smoothed Deception Distribution for", pitcher_name),
      x = "Model Deception (100 - correct_pct)",
      y = NULL,
      subtitle = "Bold black = average deception by pitch type | Red dashed = global median"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}
plot_deception_histogram("Montgomery, Kellan")

