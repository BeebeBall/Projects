library(Lahman)
library(tidyverse)

stat_order = c("AB",
               "H",
               "X2B",
               "X3B",
               "HR",
               "RBI",
               "R",
               "SO",
               "BB",
               "SB")


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
  select(Name, Team, Season, AB, BA, R:GIDP) 
  


view(baseball %>% 
  filter(Team == "Houston Colt .45's"))


write.csv(x = baseball, file = "data/baseball.csv")

as.tibble(baseball) %>% 
  filter(Team == "Kansas City Royals") %>% 
  filter(Name == "Salvador Perez") %>% 
  filter(Season == "2015") %>%
  pivot_longer(c(AB:SB, BB:SO), names_to = "Statistic", values_to = "Count") %>%
  group_by(Statistic) %>% 
  summarise(Count = sum(Count)) %>% 
  mutate(Statistic = factor(Statistic, levels = stat_order)) %>% 
  ggplot()+
  aes(x = Statistic, y = Count, fill = Statistic) %>% 
  geom_bar(stat = "identity")

