# A Comparison of Player Efficiency Ratings (PERs) over time: trends from the 
# Women's National Basketball Association (WNBA) 
#to the National Basketball Association (NBA)

# yes, but we would have to do the analysis over time, 
# so multiple years of data would have to be pulled (instead of just one years data) 
# and each years data would have to be normalized for
# either player age of years of w/nba experience
# we'd have to pick whether we're normalizing for player
# age or years of w/nba experience 

# 1997 onwards

library(rvest)
library(tidyverse)
library(readxl)
# stats <- read_html("http://insider.espn.com/nba/hollinger/statistics/_/year/2016")
# 
# 
# sth3<-stats %>%  as("character")
# 
# 
# table3<-sth3 %>% read_html %>% html_node("table") %>% html_text
# 
# table<-read.table(text=table3, sep=",")
# 
# 
# dd2 <- read.table(table3, sep = "\n", comment.char = "",
#                   col.names = c("RK",	"PLAYER",	
#                                 "GP",	"MPG"	,"TS%",
#                                 "AST"	,"TO",	"USG",
#                                 "ORR",	"DRR"	,"REBR",
#                                 "PER"	,"VA",	"EWA"),
#                   colClasses = c("character", rep("numeric", 13)),
#                   encoding = "utf8")

### WNBA PERs -----


# Function read_excel_allsheets 
# Purpose: function that reads each spreadsheet/tab in a single excel workbook    
#read_excel_allsheets <- function(filename) {
#   
#   filename = "data/wnba_pers.xlsx"
#   sheets <- readxl::excel_sheets(filename)
#   
#   x <-    lapply(sheets, function(X){
#   
#     readxl::read_excel(filename, sheet = X) %>%
#       select(Player, `PER`)# %>% 
#     #  mutate(Year = X)
#     })
#   
#   names(x) <- sheets
# 
# 
#  #x<-x %>% mutate(year = x)
#   
#   #bring in all of the sheets into seperate tables into the environment
#  # list2env(mysheets,envir=.GlobalEnv)
#   
#   
# }


read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}



w_pers<-read_excel_allsheets("data/wnba_pers.xlsx")

#w_pers<-lapply(w_pers, function(x) { x[["Year"]] <- x })

wnba.pers<-do.call(rbind.data.frame, w_pers) %>% rownames_to_column() %>% 
  # let's get the year of each dataset
  mutate(Year= sub("\\..*", "", rowname)) %>% 
  select(Year, Player, PER) %>% 
  # let's standardize the Player names by making them all caps
  mutate(Player = toupper(Player),
         Year=as.numeric(Year), # make Year numeric
         PER = as.numeric(PER))%>% #and convert PER numeric 
  # cleaning the data to remove the "Player" variable header
  filter(!Player == "PLAYER")%>% dplyr::group_by(Player, Year) %>% 
  dplyr::summarise(PER = median(PER))  


library(stringi)
detach("package:dplyr", unload=TRUE)
library(dplyr)

players_info_all<-readRDS("data/players_info_all.RDS")

wnba.pers<-players_info_all %>% 
  # for some reason, "Bealton, VA" came in all the rows. Removing this
  filter(!str_detect(PlayerInfo, "Bealeton")) %>% 
  # only get the player information with "DOB"/Born information
  filter(Info=="Born") %>% rowwise() %>% 
  mutate(# extract the four digit Year of Birth for each player
    DOB_year = stri_sub(PlayerInfo, stri_locate_last_regex(PlayerInfo, "\\d{4}")),
    # get the name from the wiki URL for each player
    PlayerName = as.character(sub(".*/", "", Player)),
    # clean up the wiki name to remove the underscore ("_") to an empty space
    PlayerName2 = gsub("_"," ", PlayerName),
    # finally, make the name uppercase
    # desperate late night attempt - can use some work on making sure it's the exact same variable being overwritten
    PlayerName3 = toupper(PlayerName2)) %>% unique() %>% 
  # let's select only the year of birth and rename PlayerName to Player for easier merging later on
  select(DOB_year, Player=PlayerName3) %>% unique() %>% 
  group_by(Player) %>% 
  # finally, join to the wnba.pers dataset
  right_join(wnba.pers) %>% 
  mutate(Age = ifelse(is.na(DOB_year),NA,
                      Year - as.numeric(DOB_year)))


### years of data for WNBA players
wnba.number_years<-wnba.pers %>% group_by(Player) %>% 
  summarise(number_of_PER_data_available = n()) %>% 
  
  group_by(number_of_PER_data_available) %>% 
  summarise(players_with_years_avail = n())
# 
# ## plot of how many years of data we have PERs data for
#  ggplot(wnba.number_years, aes(x = number_of_PER_data_available, 
#                                y=players_with_years_avail)) +
#   geom_bar(stat="identity")
 
 
# only have WNBA players with at least 8 years of PER data
 wnba_pers_8<- wnba.pers %>%
   summarise(number_of_PER_data_available = n()) %>% 
   #filter(number_of_PER_data_available>3) %>% 
   # join back in the WNBA stats for PERs
   left_join(wnba.pers)
 
 # 165 players with atleast 8 yearas of data
 
 ### NBA pers =====
 
 # Season Stats
 #https://www.kaggle.com/drgilermo/nba-players-stats/version/2#Seasons_Stats.csv
 
 
 
 nba_pers<-read_csv("data/nba_pers.csv") %>% 
   select(Year,Player, PER, Age) %>% unique() %>%  
   mutate(Player = toupper(Player),
          Player = gsub("\\*", "", Player)) 
 
 nba_pers<-nba_pers %>% 
   # let's standardize the Player names by making them all caps
   # data cleaning: removing NA player names or missing PER
   filter(is.na(Player)==F,
          is.na(PER)==F) %>% 
      group_by(Player, Year) %>% 
     summarise(PER = median(PER))  %>% 
   left_join(nba_pers %>% select(Year, Player, Age) %>% unique())
 
 
 nba.number_years<-nba_pers %>% group_by(Player) %>% 
   summarise(number_of_PER_data_available = n()) %>% 
   group_by(number_of_PER_data_available) %>% 
   summarise(players_with_years_avail = n())
 # 
 # ggplot(nba.number_years, aes(x = number_of_PER_data_available, 
 #                               y=players_with_years_avail)) +
 #   geom_bar(stat="identity")
 # 
 # only have NBA players with at least 8 years of PER data
nba_pers_8<- nba_pers %>% group_by(Player, Year) %>% 
  summarise(PER = median(PER)) %>% 
  # summarise(number_of_PER_data_available = n()) %>% 
   filter(number_of_PER_data_available>3)%>% 
  # join back in the NBA stats for PERs
  left_join(nba_pers)


##### join all datasets together =====

pers_year <- nba_pers_8 %>% mutate(class = "NBA") %>% 
  bind_rows(wnba_pers_8 %>% mutate(class = "WNBA")) %>%
  # create a meaningful layer to show if a player has a long career or not
  mutate(career_length = ifelse(number_of_PER_data_available>7, "Long Career", "Short Career")) 

pers_year <- pers_year %>% 
  mutate(LastName =  str_extract(Player, '[^ ]+$'))

## Standardize the year number

pers_year<-pers_year %>% group_by(Player) %>% 
  arrange(Year) %>% 
 mutate(StandardYear = row_number()) %>% 
  ungroup() %>% 
  arrange(LastName)

saveRDS(pers_year, "pers_year.RDS")

### looking at it by conference ====

# age of retirement, vs agg PER when retired


# 
# table((wnba_dob %>% select(Player, DOB_year) %>% unique() %>% mutate(flag = ifelse(is.na(DOB_year),
#                                                                                    "Missing DOB",
#                                                                                    "Not missing DOB")))$flag)
# 

# majority are m.RDSissing
# now majority are not missing!
