#The BAT X
BATXWeightPitch = weights %>% 
  filter(System == "The BAT X",
         Player.Type == "pitching")
BATXWeightPitch = BATXWeightPitch[-c(1:3, 7),]

BATXPitch = read.csv("BatX Pitchers.csv") %>% 
  mutate("BB/BF" = BB/TBF,
         "ER/BF" = ER/TBF,
         "HLD/G" = HLD/G,
         "SV/G" = SV/G,
         "L/G" = L/G,
         "SO/BF" = SO/TBF,
         "W/G" = W/G)


# get BATX weights
w_vec <- BATXWeightPitch %>%
  ungroup() %>% 
  select(Stat, weight)%>%
  deframe()

# apply weights
data_weighted <- BATXPitch %>%
  mutate(across(all_of(names(w_vec)), ~ .x * w_vec[cur_column()]))


for (stat in names(denoms)) {
  data_weighted[[paste0(stat, "_BATX")]] <- data_weighted[[stat]]  # already ratios, just copy
}

batx = data_weighted[,-c(2:76)]

colnames(batx) <- str_replace_all(colnames(batx), "/[A-Z]+(?=_)", "")
colnames(batx) <- str_replace_all(colnames(batx), "([A-Z0-9]+)(/.*|-.*)?(_BATX)", "\\1\\3")


x = batx %>% 
  left_join(BATXPitch) %>% 
  mutate(HLD_BATX = HLD_BATX * G,
         SV_BATX = SV_BATX * G,
         W_BATX = W_BATX * G,
         L_BATX = L_BATX * G,
         SO_BATX = SO_BATX * TBF,
         BB_BATX = BB_BATX * TBF,
         ER_BATX = ER_BATX * TBF)

batx = x[,c(1:8,19)] %>% 
  mutate(IP_BATX = IP / 6) %>% 
  select(-IP)
