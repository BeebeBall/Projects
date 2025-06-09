library(shiny)
library(tidyverse)
library(Lahman)

stat_order = c("avgBA",
               "BA",
               "avgSLG",
               "SLG",
               "avgOBP",
               "OBP",
               "avgOPS",
               "OPS")
team = as.tibble(Teams) %>% 
  select(teamID, franchID, name, yearID) %>% 
  unique() 

baseball = as.tibble(Batting) %>% 
  left_join(People) %>% 
  unite(col = "Name", nameFirst, nameLast, sep = " ", remove = TRUE) %>% 
  select(c(playerID:GIDP, Name)) %>% 
  left_join(team) %>% 
  rename(Team = name, Season  = yearID) %>% 
  filter(AB > 0) %>%
  mutate(BA = round(H / AB, 3)) %>% 
  select(Name, Team, Season, AB, BA, R:GIDP) %>% 
  group_by(Season) %>% 
  mutate(X1B = H - X2B - X3B - HR,
         SLG = round((X1B + 2*X2B + 3*X3B + 4*HR)/AB,3),
         OBP = round((H + BB + HBP)/(AB+BB+HBP+SF),3),
         OPS = SLG + OBP,
         avgBA = mean(BA),
         avgSLG = mean(SLG),
         avgOBP = mean(OBP),
         avgOPS = mean(OPS)) %>% 
  mutate(scaleBA = round(BA/mean(BA), 3),
         scaleOBP = round(OBP/mean(OBP), 3),
         scaleSLG = round(SLG/mean(SLG), 3),
         scaleOPS = round(OPS/mean(OPS), 3))


ui = navbarPage(
  title = "Rate Stats with League Averages and Scales",
  tabPanel(title = "Player Selection and League Averages", 
           titlePanel(title = "Major League Baseball Data from 1871-2023"),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "Team", 
                           label = "Team:", 
                           choices = sort(unique(baseball$Team)),
                           selected = "Kansas City Royals"),
               selectInput(inputId = "Season", 
                           label = "Season:", 
                           choices = sort(unique(baseball$Season)),
                           selected = 2015),
               selectInput(inputId = "Player", 
                           label = "Player:", 
                           choices = sort(unique(baseball$Name)),
                           selected = "Salvador Perez")
             ),
             mainPanel(plotOutput("statplot"))
           )
  ),
  tabPanel(title = "Scaled Stats", dataTableOutput("table")),
  tabPanel(title = "About",
           tags$p("Author: ", tags$p("Nate Beebe")), 
           br(),
           tags$p("The following R packages need to be installed for the app to function: tidyverse and Lahman."),
           br(),
           tags$p("The purpose of this app is to compare how certain rate statistics (batting average, slugging percentage, on-base percentage, and on-base-plus-slugging) of players compare to league averages and how they relate to the scaled versions of those statistics. This could be continued further in the final project by analyzing which seasons have the most variance in these statistics and investgating why that could be the case. Also, I would investigate seasons with higher and lower averages than surrounding years. I could also determine the importance of these statistics as they relate to winning, and how they have become more or less important over decades."),
           br(),
           tags$p("To begin, select the team, season, and player that you would like to observe. By selecting a team and a season, a list of players that appeared that sesaon for that team will become available."),
           br(),
           tags$p("The graph will always display the league average statistic for the selected season on the left of the selected player's statistic value for that season."),
           br(),
           tags$p("The table tab shows all player statistics for the selected team and season as well as their scaled version of that statisitc."),
           br(),
           tags$p("To interpret the scaled statistics, a value of 1.000 would say that that player matched the league average. A value of 1.200 would say that this player was 20% above league average, and a value of 0.800 would say they were 20% below league average."),
           br(),
           tags$p("To create this app I referenced David Dalpiaz's STAT385 video, Daniel Eck's provided shiny app, lab1 for this course, and MLB's statistic calcuations."))
)

server = function(input, output) {
  
  baseball_team = reactive({
    baseball %>% 
      filter(Team == input$Team)
  })
  
  observeEvent(eventExpr = input$Team,
               handlerExpr = {
                 updateSelectInput(inputId = "Season", 
                                   choices = sort(unique(baseball_team()$Season), decreasing = TRUE),
                                   selected = max(sort(unique(baseball_team()$Season), decreasing = TRUE)),
                                   updateSelectInput(inputId = "Player", 
                                                     choices = sort(unique(baseball_team_season()$Name), decreasing = TRUE),
                                                     selected = sort(unique(baseball_team_season()$Name), decreasing = TRUE)[1]))
               }
  )
  
  baseball_team_season = reactive({
    baseball_team() %>% 
      filter(Season == input$Season)
  })
  
  observeEvent(eventExpr = input$Season,
               handlerExpr = {
                 updateSelectInput(inputId = "Player", choices = sort(unique(baseball_team_season()$Name), decreasing = TRUE),
                                   selected = sort(unique(baseball_team_season()$Name), decreasing = TRUE)[1])
               }
  )
  
  output$statplot = renderPlot({
    
    baseball %>% 
      filter(Team == input$Team) %>% 
      filter(Season == input$Season) %>%
      filter(Name == input$Player) %>%
      pivot_longer(c(avgBA, BA, avgSLG, SLG, avgOBP, OBP, avgOPS, OPS), names_to = "Statistic", values_to = "Value") %>% 
      group_by(Statistic) %>% 
      summarise(Value = sum(Value)) %>% 
      mutate(Statistic = factor(Statistic, levels = stat_order)) %>% 
      ggplot()+
      aes(x = Statistic, y = Value, fill = Statistic, ) %>% 
      geom_bar(stat = "identity")
    
  })
  
  output$table = renderDataTable({
    
    tab = baseball_team_season()
    
    tab %>% 
      select(Name, Team, Season, scaleBA, BA, scaleSLG, SLG, scaleOBP, OBP, scaleOPS, OPS)
      })
}

shinyApp(ui = ui, server = server)
