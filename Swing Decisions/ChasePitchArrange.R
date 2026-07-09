library(dplyr)

# Define the function to filter fastball-non-fastball sequences
filter_fastball_sequence <- function(data) {
  data %>%
    group_by(game_pk, pitcher, at_bat_number) %>%  # Group by game, pitcher, and at-bat
    # Identify sequences where a fastball in the zone is followed by a non-fastball out of the zone
    filter(
      (IsFastball == TRUE & InZone == TRUE & 
         lead(IsFastball) == FALSE & lead(InZone) == FALSE) | 
        (lag(IsFastball) == TRUE & lag(InZone) == TRUE & 
           IsFastball == FALSE & InZone == FALSE)
    ) %>%
    ungroup()  # Ungroup for further analysis
}

# Example usage with pbp_2024 dataset
filtered_pbp_2024 <- filter_fastball_sequence(pbp_2024)

# Add the FastballZone column to the dataset
filtered_pbp_2024 <- filtered_pbp_2024 %>%
  mutate(FastballZone = IsFastball == TRUE & InZone == TRUE)

# Add the FastballZone column to the dataset
filtered_pbp_2024 <- filtered_pbp_2024 %>%
  mutate(OffspeedBBZone = IsFastball == FALSE & InZone == FALSE)

library(dplyr)

# Check if FastballZone alternates between TRUE and FALSE
check_FB_alternating <- filtered_pbp_2024 %>%
  mutate(Alternating = FastballZone != lag(FastballZone)) %>%  # Check if it alternates with the previous row
  filter(!is.na(Alternating))  # Remove NA values caused by the first row

# View the results of the check
all(check_FB_alternating$Alternating)

library(dplyr)

# Check if FastballZone alternates between TRUE and FALSE
check_nonFB_alternating <- filtered_pbp_2024 %>%
  mutate(Alternating = OffspeedBBZone != lag(OffspeedBBZone)) %>%  # Check if it alternates with the previous row
  filter(!is.na(Alternating))  # Remove NA values caused by the first row

# View the results of the check
all(check_nonFB_alternating$Alternating)


