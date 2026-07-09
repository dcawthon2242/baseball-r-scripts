ggplot(data=filter(Michigan, Pitcher == "Horwedel, Eamon"))+
  geom_point(mapping = aes(x = AutoPitchType, y = RelSpeed))
