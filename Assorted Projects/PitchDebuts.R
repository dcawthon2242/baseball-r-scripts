ggplot(data = filter(KrakFallFB, TaggedPitchType == "Fastball"), aes(x = RelSide, y = RelHeight, alpha = RelSpeed))+
  geom_point()+
  labs(title = "Krakoski Fastball Release Point vs. Velocity Fall 2023")+
  #scale_color_gradient(high = "blue", low = "red")+
  xlim(1, 3)+
  ylim(5, 7)
  
ggplot(data =filter(KrakFallFB, RelHeight < 7), aes(x = RelHeight, y = RelSpeed))+
  geom_point()+
  geom_smooth()