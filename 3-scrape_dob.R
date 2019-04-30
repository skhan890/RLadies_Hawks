library(plyr)
library(XML)
library(rvest)
library(dplyr)
library(magrittr)
library(data.table)

# for(i in 1:16)
# {
#   float <- paste("squad", i, sep ="")
#   print(float)
#   html = read_html("https://en.wikipedia.org/wiki/UEFA_Euro_2012_squads")
#   assign(float, html_table(html_nodes(html, "table")[[i]]))
# }

players_info<-list()

for(i in 1:nrow(sth3))
{
  float<-sth3[i,2]$URL
 # float <- paste("squad", i, sep ="")
 # print(float)
  html = read_html(paste0("https://en.wikipedia.org","/wiki/Jazmon_Gwathmey"))
  
  
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

# del.me<-nba %>% anti_join(players_info_all, by=c("URL"="Player")) %>% 
#   # dropping one record, as it was having a faulty wiki link
#   filter(!Link == "Jazmon Gwathmey")

players_info_all<-rbind(players_info2,
                        players_info3,
                        players_info4,
                        players_info5) %>% unique()

saveRDS(players_info_all,"data/players_info_all.RDS")


library(stringi)

wnba_dob<-players_info_all %>% 
  filter(Info=="Born") %>% rowwise() %>% 
  mutate(# extract the four digit Year of Birth for each player
    DOB_year = stri_sub(PlayerInfo, stri_locate_last_regex(PlayerInfo, "\\d{4}")),
        # get the name from the wiki URL for each player
         PlayerName = as.character(sub(".*/", "", Player)),
        # clean up the wiki name to remove the underscore ("_") to an empty space
         PlayerName2 = gsub("_"," ", PlayerName),
        # finally, make the name uppercase
         PlayerName3 = toupper(PlayerName2)) %>% unique()
