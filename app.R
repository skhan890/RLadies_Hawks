.libPaths("C:/R/Rlibrary")
library(shiny)
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

library(tidyverse)
library(ggrepel)

pers_year<-read_rds("pers_year.RDS")

pers_year_long<-pers_year %>%  filter(career_length == "Long Career")

## we only have from 1997 data, so I deleted all nba before that for this analysis
pers_year_long %>% group_by(class) %>% 
  mutate(lowest_year = min(Year)) %>% select(lowest_year, class) %>% unique()

median_time<-pers_year_long %>% 
  filter(Year > 1996) %>% 
  group_by(class, StandardYear) %>% 
  summarise(median_PER = median(PER))

number_players<-pers_year_long %>% 
  filter(Year > 1996) %>% 
  group_by(class) %>% select(Player) %>% unique() %>%  
  summarise(n=n())

ui <- navbarPage(inverse = TRUE,
                  "WNBA, NBA and PERs",
               
                  # Second Page  - Compare 
                  tabPanel("Compare 2 Players",
                           fluidPage(sidebarLayout(position = "right",
                                                   sidebarPanel(style = "background: black",
                                                                wellPanel(style = "background: white",
                                                                         
                                                                          selectizeInput("player1", 
                                                                                         HTML(paste(tags$span(style="color:#5ab4ac", "Player 1:"), sep = "")),
                                                                                       #  HTML(tags$span(style="color:red", "Player 1:")),
                                                                                         
                                                                                        
                                                                                      choices = unique(pers_year_long$Player),
                                                                                      selected = "DIANA TAURASI",
                                                                                      multiple = F),
                                                                          
                                                                          selectInput("player2",
                                                                                      HTML(paste(tags$span(style="color:#d8b365", "Player 2:"), sep = "")),
                                                                                      
                                                                                      choices = unique(pers_year_long$Player),
                                                                                      selected = "MICHAEL JORDAN",
                                                                                      multiple = F)
                                                                          
                                                                          )),
                                     
                                     mainPanel(
                                       
                                     #  p(strong(em("\Pick a player - any player!"))),
                                       br(),
                                       p("Choose ", 
                                         em("any "),
                                         " two players, and see how their Player Efficiency Ratings (PERs) look against each other by standardized years:"),
                                       br(),
                                       
                                       
                                       plotOutput("persComparePlot", width = "100%")
                                     )))
                  ),
                 
                 # Second Page      
                 tabPanel("Time Series Analysis", includeCSS("styles.css"),
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white", 
                                                                         h3("Standardizing PERs")
                                                                         ),
                                                               wellPanel(style = "background: white",
                                                                         h4("Info:"),
                                                                         p("Players who have had a basketball career from the year 1997 onwards are included in this analysis."),
                                                                         p("")),
                                                               wellPanel(style = "background: white",
                                                                         h4("Median PERs:"),
                                                                         p("This shows the median PER over time for all NBA and WNBA players with atleast 8 years of continuous player data available."),
                                                                         p("There are ", number_players[1,2]$n, "NBA players, and ", 
                                                                           number_players[2,2], " WNBA players."))),
                                                  
                                                  mainPanel( 
                                                    p(strong(em("\"I love the game of basketball - so it doesn't matter if you're a male or a female (player). I love to watch the game of basketball.\""), "- Lebron James")),
                                                    br(),
                                                    p("Let's put all these numbers into context. Explore the PERs difference below:"),
                                                    plotOutput("median_plot", width="100%")
                                                    #,
                                                  #  visNetworkOutput("lovenetwork", width = "100%", height = "565px")
                                                  )
                 )
                 
                 )),
                 
                 
                 
                 # Last Page         
                 tabPanel("Age and Longevity", includeCSS("styles.css"),
                          fluidPage(h1("So... are WNBA and NBA PERs drastically different from each other?"))
                          , plotOutput("longevity_plot"))
                 )



server <- function(input, output) {
  
  
  
  pers_player_compare<-reactive({
    req(input$player1)
    req(input$player2)
    
    
    pers_year_long2<- pers_year_long  %>% 
    filter(Player==input$player1 | Player==input$player2)
    
    pers_year_long2 %>% group_by(Player) %>% 
      mutate(Max_Year = max(Year),
             Min_Year = min(Year)) %>% 
      mutate(Year = ifelse(Year == Max_Year,
                           Max_Year,
                           ifelse(Year == Min_Year,
                           Min_Year, NA
      )))
    
  })
    
  output$persComparePlot <- renderPlot({  
    pers_player_compare<-pers_player_compare()
    pers_player_compare %>% filter(Player == input$player1) %>% 
    ggplot(aes(x=StandardYear, y = PER, color="#5ab4ac")) +
    geom_line(data= pers_player_compare %>% filter(Player == input$player2),
              aes(color="#d8b365"), size = 2)+
    geom_line(size = 2)+
    scale_fill_identity() +
    scale_colour_identity() +
    labs(x = "Year", y = "PER") +
    theme_minimal() +
    theme(#text = element_text(family = "Courier"),
          panel.grid.minor.x = element_blank(),
          axis.line.x = element_line(colour = "red", size = 1),
          axis.text = element_text(size = 12),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_line(),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))+
      scale_x_continuous(breaks=min(pers_player_compare$StandardYear):max(pers_player_compare$StandardYear))+
      geom_label_repel(aes(label = Year),
                       nudge_x = 1,
                       na.rm = TRUE)+
      geom_label_repel(data= pers_player_compare %>% filter(Player == input$player2),
                       aes(label = Year,
                           color = "#d8b365"),
                       nudge_x = 1,
                       na.rm = TRUE)
  })
  
  output$longevity_plot <- renderPlot({
    
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
        text = element_text(size=18),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_line(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        strip.text = element_text(size=25))
  })
  
  
  
  
  
output$median_plot <- renderPlot({  
  ggplot(median_time, aes(x = StandardYear,
                          y=median_PER, group=class)) +
    geom_bar(stat="identity")+
    facet_wrap(~class)+    scale_fill_identity() +
    scale_colour_identity() +
    labs(x = "Year", y = "PER") +
    theme_minimal() +
    theme(#text = element_text(family = "Courier"),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(colour = "red", size = 1),
      text = element_text(size=18),
      axis.text = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      axis.ticks.x = element_line(),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      strip.text = element_text(size=25))+
    scale_x_continuous(breaks=c(1,5,10,15,17,20, 26))
    
})

  }
# 
# 
# pers_year_long  %>% 
#   filter(str_detect(Player, "MICHAEL JORD")) %>% 
#   ggplot(aes(x=StandardYear, y = PER, color="red")) +
#   geom_line()+
#   geom_line(data= pers_year_long %>% filter(str_detect(Player, "TAURA")),
#             aes(color="blue"))+
#   scale_fill_identity() +
#   scale_colour_identity() +
#   labs(x = "Year", y = "PER") +
#   theme_minimal() +
#   theme(text = element_text(family = "Courier"),
#         panel.grid.minor.x = element_blank(),
#         axis.line.x = element_line(colour = "black", size = 1),
#         axis.text = element_text(size = 12),
#         panel.grid.major.x = element_blank(),
#         axis.ticks.x = element_line(),
#         plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))


# Run the application 
shinyApp(ui = ui, server = server)
# 
# 
# install.packages(c("forecast", "tseries"))
# 
# library('forecast')
# library('tseries')
# 
# count_ma = ts(na.omit(wnba_pers_8$PER), frequency=30)
# decomp = stl(count_ma, s.window="periodic")
# deseasonal_cnt <- seasadj(decomp)
# plot(decomp)
# 
# 
# pred_ave_rpart <- wnba_pers_8[, .(value = median(PER)), by = .(Year)]
# pred_ave_rpart[, Var1 := "RPART_Bagg"]


# 
# ks.test(wnba_pers_8$PER,
#         nba_pers_8$PER,
#         alternative = c("two.sided"),
#         exact = NULL)
# 
