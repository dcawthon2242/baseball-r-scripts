# Sample dataframe


# Function to get the last event when PlayResult changes
get_last_events <- function(df) {
  # Identify changes in PlayResult
  change_indices <- c(TRUE, diff(df$PAofInning) != 0)
  
  # Find the last index before each change
  last_indices <- which(c(change_indices[-1], TRUE))
  
  # Extract rows corresponding to these indices
  df[last_indices, , drop = FALSE]
}

# Apply the function to the dataframe
TM24_pbp <- get_last_events(D1TM24)

TM24_pbp$wobavalue <-
  ifelse(TM24_pbp$KorBB == "Strikeout", 0,
         ifelse(TM24_pbp$PlayResult == "Out", 0,
                ifelse(TM24_pbp$KorBB == "Walk", 0.76,
                       ifelse(TM24_pbp$PitchCall == "HitByPitch", 0.79,
                              ifelse(TM24_pbp$PlayResult == "Single", 0.94,
                                     ifelse(TM24_pbp$PlayResult == "Double", 1.34,
                                            ifelse(TM24_pbp$PlayResult == "Triple", 1.67,
                                                   ifelse(TM24_pbp$PlayResult == "HomeRun", 2.08,
                                                          NA))))))))

# Assuming TM24_pbp is already loaded in your R environment
# Create the linear model
woba_model <- lm(wobavalue ~ ExitSpeed + Angle, data = TM24_pbp)

# Add the predictions to the dataset
TM24_pbp$xwoba <- predict(woba_model, newdata = TM24_pbp)

TM24_pbp$PitchHeight <-
  ifelse(TM24_pbp$PlateLocHeight <= 2.23, "Low",
         ifelse(TM24_pbp$PlateLocHeight >= 2.87 , "High",
                "Middle"))

TM24_pbp$PitchSide <-
  ifelse(TM24_pbp$PlateLocSide >= 0.28 & TM24_pbp$BatterSide == "Right", "Inside",
         ifelse(TM24_pbp$PlateLocSide <=  -0.28 & TM24_pbp$BatterSide == "Right", "Outside",
                ifelse(TM24_pbp$PlateLocSide <= -0.28 & TM24_pbp$BatterSide == "Left", "Inside",
                       ifelse(TM24_pbp$PlateLocSide >= 0.28 & TM24_pbp$BatterSide == "Left", "Outside",
                "Middle"))))

ggplot(data = TM24_pbp)+
  geom_point(mapping = aes(x = PlateLocSide, y = PlateLocHeight, color = PitchHeight))+
  xlim(-0.83, 0.83)+
  ylim(1.6, 3.5)

ggplot(data = filter(TM24_pbp, BatterSide == "Right"))+
  geom_point(mapping = aes(x = PlateLocSide, y = PlateLocHeight, color = PitchSide))+
  xlim(-0.83, 0.83)+
  ylim(1.6, 3.5)

  ggplot(data = filter(TM24_pbp, BatterSide == "Left"))+
           geom_point(mapping = aes(x = PlateLocSide, y = PlateLocHeight, color = PitchSide))+
           xlim(-0.83, 0.83)+
           ylim(1.6, 3.5)

TM24_pbp_in_play <- filter(TM24_pbp, PlayResult != "Undefined")

fullerton_hitters_24 <- TM24_pbp_in_play %>%
  filter(BatterTeam == "CAL_FUL") %>%
  group_by(Batter) %>%
  summarise(
    PA = n(),
    xwOBAcon = round(mean(xwoba, na.rm = TRUE), 3),
    high_xwOBAcon = round(mean(xwoba[PitchHeight == "High"], na.rm = TRUE), 3),
    high_PA = sum(PitchHeight == "High", na.rm = TRUE),
    low_xwOBAcon = round(mean(xwoba[PitchHeight == "Low"], na.rm = TRUE), 3),
    low_PA = sum(PitchHeight == "Low", na.rm = TRUE),
    belt_high_xwOBAcon = round(mean(xwoba[PitchHeight == "Middle"], na.rm = TRUE), 3),
    belt_high_PA = sum(PitchHeight == "Middle", na.rm = TRUE),
    outside_xwOBAcon = round(mean(xwoba[PitchSide == "Outside"], na.rm = TRUE), 3),
    outside_PA = sum(PitchSide == "Outside", na.rm = TRUE),
    inside_xwOBAcon = round(mean(xwoba[PitchSide == "Inside"], na.rm = TRUE), 3),
    inside_PA = sum(PitchSide == "Inside", na.rm = TRUE),
    middle_xwOBAcon = round(mean(xwoba[PitchSide == "Middle"], na.rm = TRUE), 3),
    middle_PA = sum(PitchSide == "Middle", na.rm = TRUE)
  )






