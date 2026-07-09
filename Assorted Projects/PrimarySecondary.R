# Assuming your data frame is called FullertonPitching23 with columns 'Pitcher', 'TaggedPitchType', and 'Usage'

# Install and load the dplyr package if not already installed
if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)

# Group by 'Pitcher' and find the 'TaggedPitchType' with the second maximum 'Usage'
# Remove the 'TaggedPitchType' column
NCAAPitcherLeaderboard23 <- NCAAPitchesLeaderboard23 %>%
  group_by(Pitcher) %>%
  summarise(
    PitchCount = sum(PitchCount),
    Primary = TaggedPitchType[which.max(Usage)],
    Secondary = TaggedPitchType[which.max(sort(Usage)[2])],
    woba = round(mean(woba, na.rm = TRUE), 3),
    ZonePct = round(mean(ZonePct, na.rm = TRUE), 1)
  )

