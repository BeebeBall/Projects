library(shiny)
library(tidyverse)

baseball = read.csv(file = "data/baseball.csv")

stat_order = c("H",
               "X2B",
               "X3B",
               "HR",
               "RBI",
               "R",
               "SO",
               "BB",
               "SB")


ui = navbarPage(
  title = "Baseball Stats by Player Season",
  tabPanel(title = "Player Selection and Graph", 
           titlePanel(title = "Major League Baseball Statistics (1871-2023)"),
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
                         selected = "Salvador Perez"),
             checkboxInput(inputId = "year", label = "Limit Table to Stats from Only the Selected Season",
                           value = FALSE)
           ),
             mainPanel(plotOutput("statplot"))
           )
           ),
  tabPanel(title = "Stats in Table Form", dataTableOutput("table")),
  tabPanel(title = "About", includeMarkdown("about.Rmd"))
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
        pivot_longer(c(R:SB, BB:SO), names_to = "Statistic", values_to = "Count") %>% 
        group_by(Statistic) %>% 
        summarise(Count = sum(Count)) %>% 
        mutate(Statistic = factor(Statistic, levels = stat_order)) %>% 
        ggplot()+
        aes(x = Statistic, y = Count, fill = Statistic)%>% 
        geom_bar(stat = "identity")
      
    })
    
    output$table = renderDataTable({
      
     tab = baseball_team() %>% 
        calc_ops()
     
     if(input$year){
       tab = tab %>% 
         filter(Season == input$Season)
     }
     tab %>% 
       select(-X)
    })
}

shinyApp(ui = ui, server = server)
