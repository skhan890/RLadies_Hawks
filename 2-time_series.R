

## we only have from 1997 data, so I deleted all nba before that for this analysis

pers_year %>% group_by(class) %>% 
  mutate(lowest_year = min(Year)) %>% select(lowest_year, class) %>% unique()


median_time<-pers_year %>% 
  filter(Year > 1996) %>% 
  group_by(class, StandardYear) %>% 
  summarise(median_PER = median(PER))


 ggplot(median_time, aes(x = StandardYear,
                               y=median_PER, group=class)) +
  geom_bar(stat="identity")+
   facet_wrap(~class)+   
   scale_fill_identity() +
   scale_colour_identity() +
   labs(x = "Year", y = "PER") +
   theme_minimal() +
   theme(#text = element_text(family = "Courier"),
     panel.grid.minor.x = element_blank(),
     axis.line.x = element_line(colour = "red", size = 1),
     axis.text = element_text(size = 18),
     panel.grid.major.x = element_blank(),
     axis.ticks.x = element_line(),
     plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
 
 number_players<-pers_year %>% 
   filter(Year > 1996) %>% 
   group_by(class) %>% select(Player) %>% unique() %>%  
   summarise(n=n())
 
 ## non stationary dataset
 library(TSdist)
 install.packages("TSdist")
 
 DTWDistance((median_time %>% filter(class=="WNBA"))$median_PER,
             (median_time %>% filter(class!="WNBA"))$median_PER)
 