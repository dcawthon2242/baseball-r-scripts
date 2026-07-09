ggplot(data=filter(CollegeTrackmanStartTo5_31_2022, Pitcher == "Yates, Evan"))+
  geom_point(mapping = aes(x = HorzBreak, y = InducedVertBreak, color = AutoPitchType))