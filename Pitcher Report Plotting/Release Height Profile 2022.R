ggplot(data=filter(CollegeTrackmanStartTo5_31_2022, Pitcher == "Dowd, Drew" | Pitcher == "Mathews, Quinn"))+
  geom_point(mapping = aes(x = RelSide, y = RelHeight, Color = AutoPitchType))+
  facet_wrap(~ Pitcher, nrow = 1)
