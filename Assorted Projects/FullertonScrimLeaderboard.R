FullertonPitching09292023 <- X20230929_GoodwinField_Private_1_unverified %>%
group_by(Pitcher, TaggedPitchType) %>%
  summarise(
    PitchCount = n(),
    AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1),
    AvgHMov = round(mean((abs(HorzBreak)), na.rm = TRUE), 1),
    AvgIVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
    ZonePct = round(mean(zone == 1, na.rm = TRUE)* 100, 1),
  ) %>%
  group_by(Pitcher) %>%
  mutate(TotalPitches = sum(PitchCount)) %>%
  ungroup() %>%
  mutate(Usage = (PitchCount / TotalPitches) * 100) %>%
  #Change pitch filter
  filter(PitchCount >= 10)
