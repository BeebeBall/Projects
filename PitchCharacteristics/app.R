
library(tidyverse)

l1 = read.csv("LHP1.csv")
l2 = read.csv("LHP2.csv")
l3 = read.csv("LHP3.csv")
l4 = read.csv("LHP4.csv")
l5 = read.csv("LHP5.csv")
l6 = read.csv("LHP6.csv")
l7 = read.csv("LHP7.csv")
l8 = read.csv("LHP8.csv")
LHP = rbind(l1,l2,l3,l4,l5,l6,l7,l8)

r1 = read.csv("savant_data.csv")
r2 = read.csv("savant_data (1).csv")
r3 = read.csv("savant_data (2).csv")
r4 = read.csv("savant_data (3).csv")
r5 = read.csv("savant_data (4).csv")
r6 = read.csv("savant_data (5).csv")
r7 = read.csv("savant_data (6).csv")
r8 = read.csv("savant_data (7).csv")
r9 = read.csv("savant_data (8).csv")
r10 = read.csv("savant_data (9).csv")
r11 = read.csv("savant_data (10).csv")
r12 = read.csv("savant_data (11).csv")
r13 = read.csv("savant_data (12).csv")
r14 = read.csv("savant_data (13).csv")
r15 = read.csv("savant_data (14).csv")
r16 = read.csv("savant_data (15).csv")
r17 = read.csv("savant_data (16).csv")
r18 = read.csv("savant_data (17).csv")
RHP = rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18)

pitchers = rbind(LHP,RHP)

z = read.csv("pitcher_arm_angles.csv")

z = z %>% 
  select(pitcher_name, ball_angle) %>% 
  mutate(player_name = pitcher_name) %>% 
  select(player_name, ball_angle)

y = pitchers %>% 
  select(player_name, p_throws) %>% 
  unique()

x = pitchers %>% 
  group_by(player_name, pitch_name) %>% 
  summarise(mean_spin = mean(release_spin_rate, na.rm = T),
            mean_velo = mean(release_speed, na.rm = T),
            mean_extension = mean(release_extension, na.rm = T),
            total_thrown = n()) %>% 
  ungroup() %>% 
  group_by(pitch_name) %>% 
  mutate(spin_percentile = round((percent_rank(mean_spin)) * 100),
         velo_percentile = round((percent_rank(mean_velo)) * 100),
         extens_percentile = round((percent_rank(mean_extension)) * 100)) %>% 
  left_join(y) %>% 
  left_join(z) %>% 
  ungroup() %>% 
  arrange(desc(total_thrown)) %>%
  group_by(player_name) %>% 
  mutate(total = sum(total_thrown)) %>% 
  filter(total >= 100) %>%
  ungroup() %>%
  select(player_name, pitch_name, p_throws, total_thrown, mean_velo, velo_percentile, mean_spin, spin_percentile, mean_extension, extens_percentile, ball_angle) %>% 
  rename("Type of Pitch" = pitch_name,
         "Handness" = p_throws,
         "Arm Angle" = ball_angle,
         "Total Pitches Thrown" = total_thrown,
         "Average Spin Rate (RPM)" = mean_spin,
         "Spin Rate Percentile" = spin_percentile,
         "Average Pitch Speed (MPH)" = mean_velo,
         "Speed Percentile" = velo_percentile,
         "Mean Extension (FT)" = mean_extension,
         "Extension Percentile" = extens_percentile
         )


new_z = z %>% 
  rename("Player" = player_name,
         "Arm Angle" = ball_angle)

every_pitch = pitchers %>% 
  select(pitch_name, game_date, player_name, release_speed, release_spin_rate, release_extension, p_throws) %>% 
  rename("Type of Pitch" = pitch_name,
         "Player" = player_name,
         "Pitch Speed" = release_speed,
         "Spin Rate (RPM)" = release_spin_rate,
         "Extension (FT)" = release_extension,
         "Handness" = p_throws,
         "Date" = game_date) %>% 
  left_join(new_z)

library(shiny)

ui <- navbarPage(
  title = "Pitch Characteristics (2025 Season Through July)",
  
  tabPanel(
    title = "Player Averages",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "pitcher", label = "Select a Pitcher", 
                    choices = y$player_name, selected = "Bubic, Kris")
      ),
      mainPanel(tableOutput("table"))
    )
  ),
  tabPanel(title = "Find Specific Pitches",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "stat", label = "Sort by...",
                           choices = colnames(every_pitch)[c(4,5,6)]),
               selectInput(inputId = "type", label = "Filter by Pitch Type",
                           choices = c("All", unique(x$`Type of Pitch`))),
               selectInput(inputId = "player", label = "Filter by Pitcher",
                           choices = c("All", unique(x$player_name))),
               selectInput(inputId = "hand", label = "Throws",
                           choices = c("Both", unique(x$Handness))),
               selectInput(inputId = "date", label = "Filter by Date",
                           choices = c("All", unique(every_pitch$Date)))
             ),
             mainPanel(tableOutput("all_pitch"))
           ))
)


server <- function(input, output) {
  
  output$table = renderTable({
    
    tab = x %>% 
      filter(player_name == input$pitcher) %>% 
      select(-c(player_name))
    tab
    
  })
  
  
  output$all_pitch = renderTable({
    
    tab3 = every_pitch
    
    if (input$type != "All") {
      tab3 = tab3 %>% 
        filter(`Type of Pitch` == input$type)
    }
    
    if (input$player != "All") {
      tab3 = tab3 %>% 
        filter(Player == input$player)
    }
    
    if (input$hand != "Both"){
      tab3 = tab3 %>%
        filter(Handness == input$hand)
    }
    
    if (input$date != "All"){
      tab3 = tab3 %>% 
        filter(Date == input$date)
    }
    
    tab3 %>%
      arrange(desc(!!rlang::sym(input$stat))) %>%  
      head(50)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
