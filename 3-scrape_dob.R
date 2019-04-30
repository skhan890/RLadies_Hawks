library(plyr)
library(XML)
library(rvest)
library(dplyr)
library(magrittr)
library(data.table)

nba<-readxl::read_excel("data/nba.xlsx")

players_info<-list()

for(i in 1:6)
{
  float<-del.me[i,2]$URL
 # float <- paste("squad", i, sep ="")
 # print(float)
  html = read_html(paste0("https://en.wikipedia.org",float))
  
  # tt <- try(html_nodes(html, "table")[[1]])
  # ifelse(is(tt,"try-error"),"There was a warning or an error","OK")

  
  #if(length(html_nodes(html, "table")[[1]])>0){
    
    try({
  table = html_table(html_nodes(html, "table")[[1]], fill=T)
  
  table<-table[,c(1,2)]
  
  table<-table %>% dplyr::rename(Info = 1, 
                          PlayerInfo = 2)
  if(nrow(table %>% filter(`Info` %in% c("Born","Position","Career history")))==0){
    
    
  }else{
    
    table<-table %>% filter(`Info` %in% c("Born","Position","Career history")) %>%
    mutate(Player = float)
  players_info[[i]]<-table
  }
}, silent = TRUE)
}
 # scraping code
#players_info5<-do.call(rbind.data.frame, players_info) %>% rownames_to_column()

#players_info4<-do.call(rbind.data.frame, players_info) %>% rownames_to_column()

#players_info3<-do.call(rbind.data.frame, players_info) %>% rownames_to_column()

#players_info2<-do.call(rbind.data.frame, players_info) %>% rownames_to_column()


players_info6<-do.call(rbind.data.frame, players_info) %>% rownames_to_column()

players_info_all5<-players_info_all4 %>% 
  # for some reason, "Bealton, VA" came in all the rows. Removing this
  filter(!str_detect(PlayerInfo, "Bealeton")) %>% 
  # only get the player information with "DOB"/Born information
  filter(Info=="Born") %>% 
  rbind(players_info6)


del.me<-nba %>% anti_join(players_info6 %>%   filter(Info=="Born") %>% 
                            filter(!str_detect(PlayerInfo, "Bealeton")) , by=c("URL"="Player")) %>%
  # dropping one record, as it was having a faulty wiki link
  filter(!Link == "Jazmon Gwathmey")

del.me<-nba %>%
  filter(!Link=="Main page") %>% 
  
  anti_join(players_info_all5 %>%   filter(Info=="Born") %>% 
                            filter(!str_detect(PlayerInfo, "Bealeton")) , by=c("URL"="Player")) %>%
  # dropping one record, as it was having a faulty wiki link
  filter(!Link == "Jazmon Gwathmey")




players_info_all<-rbind(players_info2,
                        players_info3,
                        players_info4,
                        players_info5) %>% unique()


saveRDS(players_info_all5,"data/players_info_all.RDS")
players_info_all<-players_info_all5

saveRDS(players_info_all5,"data/players_info_all.RDS")
