median_age<-pers_year %>% group_by(Age, class, career_length) %>% 
  summarize(median_age_PER = median(PER))

median_age %>% 
  ggplot(aes(x=Age, y = median_age_PER)) +
  geom_bar(stat="identity")+
  facet_wrap(career_length~class )+
  scale_fill_identity() +
  scale_colour_identity() +
  labs(x = "Age in Years", y = "PER") +
  theme_minimal() +
  theme(#text = element_text(family = "Courier"),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(colour = "red", size = 1),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
#+
 # scale_x_continuous(breaks=min(pers_player_compare$StandardYear):max(pers_player_compare$StandardYear))