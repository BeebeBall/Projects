#Setting Up
library(tidyverse)

setwd("C:/Users/STP/Desktop/Baseball Projects/Fantasy App")

#Loading Position Data

positions = read.csv("PositionData.csv")

#Creating Projection System Weights

projection_rankings = read.csv("projection_summary.csv")

FanGraphsSystems = c("ATC", "OOPSY", "Steamer", "The BAT", "The BAT X", "ZiPS")

MAE = projection_rankings %>% 
  filter(Year == 2025,
         Stat.Group == "Standard") %>% 
  select(System, Player.Type, Stat, WLA.MAE) %>% 
  filter(System %in% FanGraphsSystems) %>% 
  group_by(Stat, Player.Type) %>% 
  mutate(raw = 1/WLA.MAE,
         sum = sum(raw),
         weight = raw/sum,
         check = sum(weight)) %>% 
  select(System, Player.Type, Stat, weight)

#Creating Weighted Fantasy Scores
notused = c("AVG", "BABIP", "HBP/PA", "OBP", "PA", "SLG", "wOBA", "ERA", "HBP/BF", "WHIP", "BF", "R/BF")

weights = MAE %>% 
  filter(! Stat %in% notused)

denoms = c(
  "BB/BF" = "BB/TBF",
  "ER/BF" = "ER/TBF",
  "HLD/G" = "HLD/G",
  "SV/G" = "SV/G",
  "L/G" = "L/G",
  "SO/BF" = "SO/TBF",
  "W/G" = "W/G")

#ZiPS
ZiPSWeightPitch = weights %>% 
  filter(System == "ZiPS",
         Player.Type == "pitching")
ZiPSWeightPitch = ZiPSWeightPitch[-c(1:3, 7),]

ZiPSPitch = read.csv("Zips Pitchers.csv") %>% 
  mutate("BB/BF" = BB/TBF,
         "ER/BF" = ER/TBF,
         "HLD/G" = HLD/G,
         "SV/G" = SV/G,
         "L/G" = L/G,
         "SO/BF" = SO/TBF,
         "W/G" = W/G)


# get ZiPS weights
w_vec <- ZiPSWeightPitch %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- ZiPSPitch %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))


for (stat in names(denoms)) {
  data_weighted[[paste0(stat, "_ZiPS")]] <- data_weighted[[stat]]  # already ratios, just copy
}

zips = data_weighted[,-c(2:76)]

colnames(zips) <- str_replace_all(colnames(zips), "/[A-Z]+(?=_)", "")
colnames(zips) <- str_replace_all(colnames(zips), "([A-Z0-9]+)(/.*|-.*)?(_ZiPS)", "\\1\\3")


x = zips %>% 
  left_join(ZiPSPitch) %>% 
  mutate(HLD_ZiPS = HLD_ZiPS * G,
         SV_ZiPS = SV_ZiPS * G,
         W_ZiPS = W_ZiPS * G,
         L_ZiPS = L_ZiPS * G,
         SO_ZiPS = SO_ZiPS * TBF,
         BB_ZiPS = BB_ZiPS * TBF,
         ER_ZiPS = ER_ZiPS * TBF)

zips = x[,c(1:8,19,21)] %>% 
  mutate(IP_ZiPS = IP / 5,
         H_ZiPS = H / 5) %>% 
  select(-c(IP,H))





#ATC
ATCWeightPitch = weights %>% 
  filter(System == "ATC",
         Player.Type == "pitching")
ATCWeightPitch = ATCWeightPitch[-c(1:3, 7),]

ATCPitch = read.csv("ATC Pitchers.csv") %>% 
  mutate("BB/BF" = BB/TBF,
         "ER/BF" = ER/TBF,
         "HLD/G" = HLD/G,
         "SV/G" = SV/G,
         "L/G" = L/G,
         "SO/BF" = SO/TBF,
         "W/G" = W/G)


# get ATC weights
w_vec <- ATCWeightPitch %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- ATCPitch %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))


for (stat in names(denoms)) {
  data_weighted[[paste0(stat, "_ATC")]] <- data_weighted[[stat]]  # already ratios, just copy
}

atc = data_weighted[,-c(2:76)]

colnames(atc) <- str_replace_all(colnames(atc), "/[A-Z]+(?=_)", "")
colnames(atc) <- str_replace_all(colnames(atc), "([A-Z0-9]+)(/.*|-.*)?(_ATC)", "\\1\\3")


x = atc %>% 
  left_join(ATCPitch) %>% 
  mutate(HLD_ATC = HLD_ATC * G,
         SV_ATC = SV_ATC * G,
         W_ATC = W_ATC * G,
         L_ATC = L_ATC * G,
         SO_ATC = SO_ATC * TBF,
         BB_ATC = BB_ATC * TBF,
         ER_ATC = ER_ATC * TBF)

atc = x[,c(1:8,19,21)] %>% 
  mutate(IP_ATC = IP / 5,
         H_ATC = H / 5) %>% 
  select(-c(IP,H))




#OOPSY
OOPSYWeightPitch = weights %>% 
  filter(System == "OOPSY",
         Player.Type == "pitching")
OOPSYWeightPitch = OOPSYWeightPitch[-c(1:3, 7),]

OOPSYPitch = read.csv("OOPSY Pitchers.csv") %>% 
  mutate("BB/BF" = BB/TBF,
         "ER/BF" = ER/TBF,
         "HLD/G" = HLD/G,
         "SV/G" = SV/G,
         "L/G" = L/G,
         "SO/BF" = SO/TBF,
         "W/G" = W/G)


# get OOPSY weights
w_vec <- OOPSYWeightPitch %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- OOPSYPitch %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))


for (stat in names(denoms)) {
  data_weighted[[paste0(stat, "_OOPSY")]] <- data_weighted[[stat]]  # already ratios, just copy
}

oopsy = data_weighted[,-c(2:76)]

colnames(oopsy) <- str_replace_all(colnames(oopsy), "/[A-Z]+(?=_)", "")
colnames(oopsy) <- str_replace_all(colnames(oopsy), "([A-Z0-9]+)(/.*|-.*)?(_OOPSY)", "\\1\\3")


x = oopsy %>% 
  left_join(OOPSYPitch) %>% 
  mutate(HLD_OOPSY = HLD_OOPSY * G,
         SV_OOPSY = SV_OOPSY * G,
         W_OOPSY = W_OOPSY * G,
         L_OOPSY = L_OOPSY * G,
         SO_OOPSY = SO_OOPSY * TBF,
         BB_OOPSY = BB_OOPSY * TBF,
         ER_OOPSY = ER_OOPSY * TBF)

oopsy = x[,c(1:8,19,21)] %>% 
  mutate(IP_OOPSY = IP / 5,
         H_OOPSY = H / 5) %>% 
  select(-c(IP,H))





#Steamer
SteamerWeightPitch = weights %>% 
  filter(System == "Steamer",
         Player.Type == "pitching")
SteamerWeightPitch = SteamerWeightPitch[-c(1:3, 7),]

SteamerPitch = read.csv("Steamer Pitchers.csv") %>% 
  mutate("BB/BF" = BB/TBF,
         "ER/BF" = ER/TBF,
         "HLD/G" = HLD/G,
         "SV/G" = SV/G,
         "L/G" = L/G,
         "SO/BF" = SO/TBF,
         "W/G" = W/G)


# get Steamer weights
w_vec <- SteamerWeightPitch %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- SteamerPitch %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))


for (stat in names(denoms)) {
  data_weighted[[paste0(stat, "_Steamer")]] <- data_weighted[[stat]]  # already ratios, just copy
}

steamer = data_weighted[,-c(2:76)]

colnames(steamer) <- str_replace_all(colnames(steamer), "/[A-Z]+(?=_)", "")
colnames(steamer) <- str_replace_all(colnames(steamer), "([A-Z0-9]+)(/.*|-.*)?(_Steamer)", "\\1\\3")


x = steamer %>% 
  left_join(SteamerPitch) %>% 
  mutate(HLD_Steamer = HLD_Steamer * G,
         SV_Steamer = SV_Steamer * G,
         W_Steamer = W_Steamer * G,
         L_Steamer = L_Steamer * G,
         SO_Steamer = SO_Steamer * TBF,
         BB_Steamer = BB_Steamer * TBF,
         ER_Steamer = ER_Steamer * TBF)

steamer = x[,c(1:8,19,21)] %>% 
  mutate(IP_Steamer = IP / 5,
         H_Steamer = H / 5) %>% 
  select(-c(IP,H))






#The BAT
BATWeightPitch = weights %>% 
  filter(System == "The BAT",
         Player.Type == "pitching")
BATWeightPitch = BATWeightPitch[-c(1:3, 7),]

BATPitch = read.csv("The Bat Pitchers.csv") %>% 
  mutate("BB/BF" = BB/TBF,
         "ER/BF" = ER/TBF,
         "HLD/G" = HLD/G,
         "SV/G" = SV/G,
         "L/G" = L/G,
         "SO/BF" = SO/TBF,
         "W/G" = W/G)


# get The BAT weights
w_vec <- BATWeightPitch %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- BATPitch %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))


for (stat in names(denoms)) {
  data_weighted[[paste0(stat, "_BAT")]] <- data_weighted[[stat]]  # already ratios, just copy
}

bat = data_weighted[,-c(2:76)]

colnames(bat) <- str_replace_all(colnames(bat), "/[A-Z]+(?=_)", "")
colnames(bat) <- str_replace_all(colnames(bat), "([A-Z0-9]+)(/.*|-.*)?(_BAT)", "\\1\\3")


x = bat %>% 
  left_join(BATPitch) %>% 
  mutate(HLD_BAT = HLD_BAT * G,
         SV_BAT = SV_BAT * G,
         W_BAT = W_BAT * G,
         L_BAT = L_BAT * G,
         SO_BAT = SO_BAT * TBF,
         BB_BAT = BB_BAT * TBF,
         ER_BAT = ER_BAT * TBF)

bat = x[,c(1:8,19,21)] %>% 
  mutate(IP_BAT = IP / 5,
         H_BAT = H / 5) %>% 
  select(-c(IP,H))



#COMBINE PROJECTIONS INTO ONE DATASET
master = zips %>% 
  left_join(oopsy) %>% 
  left_join(steamer) %>% 
  left_join(bat) %>%
  left_join(atc)


x = master %>% 
  mutate(
    `ER_total` = rowSums(across(starts_with("ER_")), na.rm = TRUE),
    `HLD_total` = rowSums(across(starts_with("HLD_")), na.rm = TRUE),
    `SV_total` = rowSums(across(starts_with("SV_")), na.rm = TRUE),
    `L_total` = rowSums(across(starts_with("L_")), na.rm = TRUE),
    `BB_total` = rowSums(across(starts_with("BB_")), na.rm = TRUE),
    `SO_total` = rowSums(across(starts_with("SO_")), na.rm = TRUE),
    `W_total` = rowSums(across(starts_with("W_")), na.rm = TRUE),
    `IP_total` = rowSums(across(starts_with("IP_")), na.rm = TRUE),
    `H_total` = rowSums(across(starts_with("H_")), na.rm = TRUE)
    )




proj_totals_pitcher = x[,-c(2:46)]

positions = read.csv("PositionData.csv") %>% 
  rename(Name = PLAYER,
         Position = POS)

#ADD GROUPING BY POSITION (MAYBE ALSO / OR BY TEAM) TO DEAL WITH UNINTENTIONAL GROUPINGS

fantasy_totals_pitcher = proj_totals_pitcher %>% 
  mutate(FPTS = 3*IP_total - H_total - 2*ER_total - BB_total + SO_total + 2*W_total - 2*L_total + 5*SV_total + 2*HLD_total) %>% 
  left_join(positions) %>% 
  group_by(Name, Position) %>% 
  summarise(across(where(is.numeric),
                   mean,
                   na.rm = TRUE))



write.csv(fantasy_totals_pitcher, "pitchers_weighted_projections.csv")
