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
notused = c("AVG", "BABIP", "HBP/PA", "OBP", "PA", "SLG", "wOBA", "BF", "ERA", "HBP/BF", "WHIP")

weights = MAE %>% 
  filter(! Stat %in% notused)

denoms = c(
  "HR/BIP" = "BIP",
  "1B/(BIP-HR)" = "BIP_HR",
  "2B/(BIP-HR)" = "BIP_HR",
  "3B/(BIP-HR)" = "BIP_HR",
  "BB/PA" = "PA",
  "R/PA" = "PA",
  "RBI/PA" = "PA",
  "SO/PA" = "PA",
  "SB/TOF" = "TOF"
)


#ZiPS
ZiPSWeightBat = weights %>% 
  filter(System == "ZiPS",
         Player.Type == "batting")

ZiPSBat = read.csv("Zips Batters.csv") %>% 
  mutate(BIP = AB - SO + SF,
         BB = BB + IBB,
         TOF = BB + X1B) %>% 
  select(Name, G, PA, H, X1B, X2B, X3B, HR, R, RBI, BB, SO, SB, CS, TOF, ADP, BIP) %>% 
  mutate(FPTS = 2*X1B + 3*X2B + 4*X3B + 5*HR + RBI + R + SB - SO + H) %>% 
  arrange(desc(FPTS)) %>% 
  mutate("1B/(BIP-HR)" = X1B/(BIP-HR),
         "2B/(BIP-HR)" = X2B/(BIP-HR),
         "3B/(BIP-HR)" = X3B/(BIP-HR),
         "HR/BIP" = HR/BIP,
         "BB/PA" = BB/PA,
         "R/PA" = R/PA,
         "RBI/PA" = RBI/PA,
         "SO/PA" = SO/PA,
         "SB/TOF" = SB/TOF)


# get ZiPS weights
w_vec <- ZiPSWeightBat %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- ZiPSBat %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))


data_weighted <- data_weighted %>%
  mutate(BIP_HR = BIP - HR)

for (stat in names(denoms)) {
  denom <- denoms[[stat]]
  data_weighted[[paste0(stat, "_ZiPS")]] <- data_weighted[[stat]] * data_weighted[[denom]]
}

zips = data_weighted[,-c(2:28)]

colnames(zips) <- str_replace_all(colnames(zips), "/[A-Z]+(?=_)", "")
colnames(zips) <- str_replace_all(colnames(zips), "([A-Z0-9]+)(/.*|-.*)?(_ZiPS)", "\\1\\3")







#ATC
ATCWeightBat = weights %>% 
  filter(System == "ATC",
         Player.Type == "batting")

ATCBat = read.csv("ATC Batters.csv") %>% 
  mutate(BIP = AB - SO + SF,
         BB = BB + IBB,
         TOF = BB + X1B) %>% 
  select(Name, G, PA, H, X1B, X2B, X3B, HR, R, RBI, BB, SO, SB, CS, TOF, ADP, BIP) %>% 
  mutate(FPTS = 2*X1B + 3*X2B + 4*X3B + 5*HR + RBI + R + SB - SO + H) %>% 
  arrange(desc(FPTS)) %>% 
  mutate("1B/(BIP-HR)" = X1B/(BIP-HR),
         "2B/(BIP-HR)" = X2B/(BIP-HR),
         "3B/(BIP-HR)" = X3B/(BIP-HR),
         "HR/BIP" = HR/BIP,
         "BB/PA" = BB/PA,
         "R/PA" = R/PA,
         "RBI/PA" = RBI/PA,
         "SO/PA" = SO/PA,
         "SB/TOF" = SB/TOF)


# get ATC weights
w_vec <- ATCWeightBat %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- ATCBat %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))

data_weighted <- data_weighted %>%
  mutate(BIP_HR = BIP - HR)

for (stat in names(denoms)) {
  denom <- denoms[[stat]]
  data_weighted[[paste0(stat, "_ATC")]] <- data_weighted[[stat]] * data_weighted[[denom]]
}

atc = data_weighted[,-c(2:28)]

colnames(atc) <- str_replace_all(colnames(atc), "/[A-Z]+(?=_)", "")
colnames(atc) <- str_replace_all(colnames(atc), "([A-Z0-9]+)(/.*|-.*)?(_ATC)", "\\1\\3")








#OOPSY

OOPSYWeightBat = weights %>% 
  filter(System == "OOPSY",
         Player.Type == "batting")

OOPSYBat = read.csv("OOPSY Batters.csv") %>% 
  mutate(BIP = AB - SO + SF,
         BB = BB + IBB,
         TOF = BB + X1B) %>% 
  select(Name, G, PA, H, X1B, X2B, X3B, HR, R, RBI, BB, SO, SB, CS, TOF, ADP, BIP) %>% 
  mutate(FPTS = 2*X1B + 3*X2B + 4*X3B + 5*HR + RBI + R + SB - SO + H) %>% 
  arrange(desc(FPTS)) %>% 
  mutate("1B/(BIP-HR)" = X1B/(BIP-HR),
         "2B/(BIP-HR)" = X2B/(BIP-HR),
         "3B/(BIP-HR)" = X3B/(BIP-HR),
         "HR/BIP" = HR/BIP,
         "BB/PA" = BB/PA,
         "R/PA" = R/PA,
         "RBI/PA" = RBI/PA,
         "SO/PA" = SO/PA,
         "SB/TOF" = SB/TOF)


# get OOPSY weights
w_vec <- OOPSYWeightBat %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- OOPSYBat %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))


data_weighted <- data_weighted %>%
  mutate(BIP_HR = BIP - HR)

for (stat in names(denoms)) {
  denom <- denoms[[stat]]
  data_weighted[[paste0(stat, "_OOPSY")]] <- data_weighted[[stat]] * data_weighted[[denom]]
}

oopsy = data_weighted[,-c(2:28)]

colnames(oopsy) <- str_replace_all(colnames(oopsy), "/[A-Z]+(?=_)", "")
colnames(oopsy) <- str_replace_all(colnames(oopsy), "([A-Z0-9]+)(/.*|-.*)?(_OOPSY)", "\\1\\3")






#Steamer
SteamerWeightBat = weights %>% 
  filter(System == "Steamer",
         Player.Type == "batting")

SteamerBat = read.csv("Steamer Batters.csv") %>% 
  mutate(BIP = AB - SO + SF,
         BB = BB + IBB,
         TOF = BB + X1B) %>% 
  select(Name, G, PA, H, X1B, X2B, X3B, HR, R, RBI, BB, SO, SB, CS, TOF, ADP, BIP) %>% 
  mutate(FPTS = 2*X1B + 3*X2B + 4*X3B + 5*HR + RBI + R + SB - SO + H) %>% 
  arrange(desc(FPTS)) %>% 
  mutate("1B/(BIP-HR)" = X1B/(BIP-HR),
         "2B/(BIP-HR)" = X2B/(BIP-HR),
         "3B/(BIP-HR)" = X3B/(BIP-HR),
         "HR/BIP" = HR/BIP,
         "BB/PA" = BB/PA,
         "R/PA" = R/PA,
         "RBI/PA" = RBI/PA,
         "SO/PA" = SO/PA,
         "SB/TOF" = SB/TOF)


# get Steamer weights
w_vec <- SteamerWeightBat %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- SteamerBat %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))


data_weighted <- data_weighted %>%
  mutate(BIP_HR = BIP - HR)

for (stat in names(denoms)) {
  denom <- denoms[[stat]]
  data_weighted[[paste0(stat, "_Steamer")]] <- data_weighted[[stat]] * data_weighted[[denom]]
}

steamer = data_weighted[,-c(2:28)]

colnames(steamer) <- str_replace_all(colnames(steamer), "/[A-Z]+(?=_)", "")
colnames(steamer) <- str_replace_all(colnames(steamer), "([A-Z0-9]+)(/.*|-.*)?(_Steamer)", "\\1\\3")







#The BAT
BATWeightBat = weights %>% 
  filter(System == "The BAT",
         Player.Type == "batting")

BATBat = read.csv("The Bat Batters.csv") %>% 
  mutate(BIP = AB - SO + SF,
         BB = BB + IBB,
         TOF = BB + X1B) %>% 
  select(Name, G, PA, H, X1B, X2B, X3B, HR, R, RBI, BB, SO, SB, CS, TOF, ADP, BIP) %>% 
  mutate(FPTS = 2*X1B + 3*X2B + 4*X3B + 5*HR + RBI + R + SB - SO + H) %>% 
  arrange(desc(FPTS)) %>% 
  mutate("1B/(BIP-HR)" = X1B/(BIP-HR),
         "2B/(BIP-HR)" = X2B/(BIP-HR),
         "3B/(BIP-HR)" = X3B/(BIP-HR),
         "HR/BIP" = HR/BIP,
         "BB/PA" = BB/PA,
         "R/PA" = R/PA,
         "RBI/PA" = RBI/PA,
         "SO/PA" = SO/PA,
         "SB/TOF" = SB/TOF)


# get The BAT weights
w_vec <- BATWeightBat %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- BATBat %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))


data_weighted <- data_weighted %>%
  mutate(BIP_HR = BIP - HR)

for (stat in names(denoms)) {
  denom <- denoms[[stat]]
  data_weighted[[paste0(stat, "_TheBAT")]] <- data_weighted[[stat]] * data_weighted[[denom]]
}

bat = data_weighted[,-c(2:28)]

colnames(bat) <- str_replace_all(colnames(bat), "/[A-Z]+(?=_)", "")
colnames(bat) <- str_replace_all(colnames(bat), "([A-Z0-9]+)(/.*|-.*)?(_TheBAT)", "\\1\\3")







#The BAT X
BATXWeightBat = weights %>% 
  filter(System == "The BAT X",
         Player.Type == "batting")

BATXBat = read.csv("BatX Batters.csv") %>% 
  mutate(BIP = AB - SO + SF,
         BB = BB + IBB,
         TOF = BB + X1B) %>% 
  select(Name, G, PA, H, X1B, X2B, X3B, HR, R, RBI, BB, SO, SB, CS, TOF, ADP, BIP) %>% 
  mutate(FPTS = 2*X1B + 3*X2B + 4*X3B + 5*HR + RBI + R + SB - SO + H) %>% 
  arrange(desc(FPTS)) %>% 
  mutate("1B/(BIP-HR)" = X1B/(BIP-HR),
         "2B/(BIP-HR)" = X2B/(BIP-HR),
         "3B/(BIP-HR)" = X3B/(BIP-HR),
         "HR/BIP" = HR/BIP,
         "BB/PA" = BB/PA,
         "R/PA" = R/PA,
         "RBI/PA" = RBI/PA,
         "SO/PA" = SO/PA,
         "SB/TOF" = SB/TOF)


# get The BAT X weights
w_vec <- BATXWeightBat %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- BATXBat %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))


data_weighted <- data_weighted %>%
  mutate(BIP_HR = BIP - HR)

for (stat in names(denoms)) {
  denom <- denoms[[stat]]
  data_weighted[[paste0(stat, "_BATX")]] <- data_weighted[[stat]] * data_weighted[[denom]]
}

batx = data_weighted[,-c(2:28)]

colnames(batx) <- str_replace_all(colnames(batx), "/[A-Z]+(?=_)", "")
colnames(batx) <- str_replace_all(colnames(batx), "([A-Z0-9]+)(/.*|-.*)?(_BATX)", "\\1\\3")



#COMBINE PROJECTIONS INTO ONE DATASET
master = zips %>% 
  left_join(oopsy) %>% 
  left_join(steamer) %>% 
  left_join(bat) %>% 
  left_join(batx) %>% 
  left_join(atc)

x = master %>% 
  mutate(
  `1B_total` = rowSums(across(starts_with("1B_")), na.rm = TRUE),
  `2B_total` = rowSums(across(starts_with("2B_")), na.rm = TRUE),
  `3B_total` = rowSums(across(starts_with("3B_")), na.rm = TRUE),
  `HR_total` = rowSums(across(starts_with("HR_")), na.rm = TRUE),
  `BB_total` = rowSums(across(starts_with("BB_")), na.rm = TRUE),
  `SO_total` = rowSums(across(starts_with("SO_")), na.rm = TRUE),
  `BB_total` = rowSums(across(starts_with("BB_")), na.rm = TRUE),
  `RBI_total` = rowSums(across(starts_with("RBI_")), na.rm = TRUE),
  `SB_total` = rowSums(across(starts_with("SB_")), na.rm = TRUE),
  `R_total` = rowSums(across(starts_with("R_")), na.rm = TRUE)
)

proj_totals_batter = x[,-c(2:55)]

positions = read.csv("PositionData.csv") %>% 
  rename(Name = PLAYER,
         Position = POS)

fantasy_totals = proj_totals_batter %>% 
  mutate(H_total = rowSums(across(c("1B_total", "2B_total", "3B_total", "HR_total")), na.rm = TRUE),
         FPTS = 2*`1B_total` + 3*`2B_total` + 4*`3B_total` + 5*`HR_total` + `RBI_total` + `R_total` + `SB_total` - `SO_total` + H_total) %>% 
  left_join(positions) %>% 
  filter(!Position %in% c("SP", "RP", "SP/RP", "RP/SP")) %>% 
  group_by(Name, Position) %>% 
  summarise(across(where(is.numeric),
                   mean,
                   na.rm = TRUE))


write.csv(fantasy_totals, "batters_weighted_projections.csv")
