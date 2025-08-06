
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
  mutate(spin_percentile = round((mean_spin/max(mean_spin)) * 100 ),
         velo_percentile = round((mean_velo/max(mean_velo)) * 100),
         extens_percentile = round((mean_extension/max(mean_extension)) * 100)) %>% 
  left_join(y) %>% 
  left_join(z) %>% 
  ungroup() %>% 
  arrange(desc(total_thrown)) %>%
  group_by(player_name) %>% 
  mutate(total = sum(total_thrown)) %>% 
  filter(total >= 100) %>%
  ungroup() %>% 
  mutate("Type of Pitch" = pitch_name,
         "Handness" = p_throws,
         "Arm Angle" = ball_angle,
         "Total Pitches Thrown" = total_thrown,
         "Average Spin Rate (RPM)" = mean_spin,
         "Spin Rate Percentile" = spin_percentile,
         "Average Pitch Speed (MPH)" = mean_velo,
         "Speed Percentile" = velo_percentile,
         "Mean Extension (FT)" = mean_extension,
         "Extension Percentile" = extens_percentile
         ) %>% 
  select(-c(total, mean_spin,mean_extension,mean_velo,total_thrown,spin_percentile,velo_percentile,extens_percentile,pitch_name,p_throws,ball_angle))




library(shiny)

ui <- navbarPage(
  title = "Pitcher Spin Rate Percentile Rankings",
  
  tabPanel(
    title = "Raw Numbers",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "pitcher", label = "Select a Pitcher", 
                    choices = y$player_name, selected = "Bubic, Kris")
      ),
      mainPanel(tableOutput("table"))
    )
  ),
  tabPanel(title = "Percentiles", tableOutput("percentiles"))
)


server <- function(input, output) {
  
  output$table = renderTable({
    
    tab = x %>% 
      filter(player_name == input$pitcher) %>% 
      select(-c(player_name, "Spin Rate Percentile", "Speed Percentile", "Extension Percentile"))
    tab
    
  })
  
  output$percentiles = renderTable({
    tab2 = x %>% 
      filter(player_name == input$pitcher) %>% 
      select(-c(player_name, "Handness", "Arm Angle", "Total Pitches Thrown", `Average Spin Rate (RPM)`, `Average Pitch Speed (MPH)`, `Mean Extension (FT)`))
    tab2
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
