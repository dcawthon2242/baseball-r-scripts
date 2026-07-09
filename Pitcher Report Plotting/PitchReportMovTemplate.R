ggplot(data = filter(CollegeTrackmanStartTo5_31_2022, Pitcher == "Juline, Tucker"))+
  geom_point(mapping = aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType))
