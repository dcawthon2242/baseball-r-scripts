
#Make a column that notes if a ball is a strike or ball
CollegeTrackmanStartTo5_31_2022PitchResult <- CollegeTrackmanStartTo5_31_2022 %>%
  mutate(
    PitchResult = case_when(
      ((PlateLocHeight >= 1.5241) 
       & (PlateLocHeight <= 3.6733)
       & (PlateLocSide >= -.8308)
       & (PlateLocSide <= .8308)) ~ "strike",
      TRUE ~ "ball"
    )
  )
# Create a new column Chases in the database
TrackmanChaseResult <- CollegeTrackmanStartTo5_31_2022PitchResult <- CollegeTrackmanStartTo5_31_2022PitchResult %>%
  mutate(Chases = ifelse(PitchCall == "StrikeSwinging" & PitchResult == "Strike", "Chase", "No Chase"))


# Load data
data <- TrackmanChaseResult.csv

# Group by Pitchers and TaggedPitchType, calculate average RelPointX, RelPointY, and StrikeSwinging/Chase rate
PitchReleasePointAvg <- data %>%
  group_by(data, Pitchers, TaggedPitchType) %>%
  summarize(
    avg_RelPointX = mean(RelPointX),
    avg_RelPointY = mean(RelPointY),
    StrikeSwinging_rate = mean(PitchCall == "StrikeSwinging"),
    Chase_rate = mean(Chases == "Swing")
  )

# View the resulting table
view(PitchReleasePointAvg)


