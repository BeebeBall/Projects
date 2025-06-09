#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidyverse)

data = read.csv("savant_data.csv")
data2 = read.csv("savant_data (1).csv")
data3 = read.csv("savant_data (2).csv")
data4 = read.csv("savant_data (3).csv")
data5 = read.csv("savant_data (4).csv")
data6 = read.csv("savant_data (5).csv")
x = data %>% 
  mutate(season = 2024) %>% 
  mutate(player_type = 2) %>% 
  mutate(hbp = pa - bip - so - bb) %>% 
  mutate(X1B_per = singles/pa + 0.001,
         X2B_per = doubles/pa + 0.001,
         X3B_per = triples/pa + 0.001,
         HR_per = hrs/pa + 0.001,
         HBP_per = hbp/pa + 0.001,
         K_per = k_percent/100,
         BB_per = bb_percent/100) %>%
  mutate(O_per = 1 - (X1B_per + X2B_per + X3B_per + HR_per + HBP_per + BB_per)) %>% 
  select(player_name, season, player_type, pa:bb_percent, hbp, babip, X1B_per:BB_per, O_per) %>% 
  mutate(
    groupBB =
      case_when(
        BB_per <= 0.05 ~ 1,
        BB_per > 0.05 & BB_per <= 0.075 ~ 2,
        BB_per > 0.075 & BB_per <= 0.1 ~ 3,
        BB_per > 0.1 & BB_per <= 0.125 ~ 4,
        BB_per > 0.125 & BB_per <= 0.15 ~ 5,
        .default = 6
      ),
    groupK =
      case_when(
        K_per <= 0.15 ~ 1,
        K_per > 0.15 & K_per <= 0.20 ~ 2,
        K_per > 0.2 & K_per <= 0.25 ~ 3,
        K_per > 0.25 & K_per <= 0.30 ~ 4,
        K_per > 0.3 & K_per <= 0.35 ~ 5,
        .default = 6
      ),
    group1B =
      case_when(
        X1B_per <= 0.05 ~ 1,
        X1B_per > 0.05 & X1B_per <= 0.10 ~ 2,
        X1B_per > 0.1 & X1B_per <= 0.125 ~ 3,
        X1B_per > 0.125 & X1B_per <= 0.15 ~ 4,
        X1B_per > 0.15 & X1B_per <= 0.175 ~ 5,
        X1B_per > 0.175 & X1B_per <= 0.2 ~ 6,
        .default = 7
      ),
    group2B =
      case_when(
        X2B_per <= 0.025 ~ 1,
        X2B_per > 0.025 & X2B_per <= 0.04 ~ 2,
        X2B_per > 0.04 & X2B_per <= 0.05 ~ 3,
        X2B_per > 0.05 & X2B_per <= 0.07 ~ 4,
        .default = 5
      ),
    group3B =
      case_when(
        X3B_per <= 0.001 ~ 1,
        X3B_per > 0.001 & X3B_per <= 0.002 ~ 2,
        X3B_per > 0.002 & X3B_per <= 0.003 ~ 3,
        X3B_per > 0.003 & X3B_per <= 0.004 ~ 4,
        X3B_per > 0.004 & X3B_per <= 0.005 ~ 5,
        X3B_per > 0.005 & X3B_per <= 0.006 ~ 6,
        X3B_per > 0.006 & X3B_per <= 0.007 ~ 7,
        .default = 8
      ),
    groupHR =
      case_when(
        HR_per <= 0.01 ~ 1,
        HR_per > 0.01 & HR_per <= 0.015 ~ 2,
        HR_per > 0.015 & HR_per <= 0.03 ~ 3,
        HR_per > 0.03 & HR_per <= 0.045 ~ 4,
        HR_per > 0.045 & HR_per <= 0.07 ~ 5,
        .default = 6
      ),
    groupHBP =
      case_when(
        HBP_per <= 0.005 ~ 1,
        HBP_per > 0.005 & HBP_per <= 0.0075 ~ 2,
        HBP_per > 0.0075 & HBP_per <= 0.01 ~ 3,
        HBP_per > 0.01 & HBP_per <= 0.015 ~ 4,
        HBP_per > 0.015 & HBP_per <= 0.02 ~ 5,
        HBP_per > 0.02 & HBP_per <= 0.04 ~ 6,
        .default = 7
      ),
    groupBABIP =
      case_when(
        babip <= 0.2 ~ 1,
        babip > 0.2 & babip <= 0.25 ~ 2,
        babip > 0.25 & babip <= 0.275 ~ 3,
        babip > 0.275 & babip <= 0.3 ~ 4,
        babip > 0.3 & babip <= 0.325 ~ 5,
        babip > 0.325 & babip <= 0.375 ~ 6,
        .default = 7
      )) %>% 
  group_by(groupBB) %>% 
  mutate(totalBB_per = round(sum(bb)/sum(pa),3)) %>% 
  ungroup() %>% 
  group_by(groupK) %>%
  mutate(totalK_per = round(sum(so)/sum(pa),3)) %>%
  ungroup() %>% 
  group_by(group1B) %>%
  mutate(total1B_per = round(sum(singles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group2B) %>%
  mutate(total2B_per = round(sum(doubles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group3B) %>%
  mutate(total3B_per = round(sum(triples)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHR) %>%
  mutate(totalHR_per = round(sum(hrs)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHBP) %>%
  mutate(totalHBP_per = round(sum(hbp)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupBABIP) %>%
  mutate(avgBABIP = round(mean(babip),3)) %>%
  arrange(desc(pa)) %>% 
  head(500)

x2 = data2 %>% 
  mutate(season = 2024) %>% 
  mutate(player_type = 1) %>% 
  mutate(hbp = pa - bip - so - bb) %>%
  mutate(X1B_per = singles/pa + 0.001,
         X2B_per = doubles/pa + 0.001,
         X3B_per = triples/pa + 0.001,
         HR_per = hrs/pa + 0.001,
         HBP_per = hbp/pa + 0.001,
         K_per = k_percent/100,
         BB_per = bb_percent/100) %>%
  mutate(O_per = 1 - (X1B_per + X2B_per + X3B_per + HR_per + HBP_per + BB_per)) %>% 
  select(player_name, season, player_type, pa:bb_percent, hbp, babip, X1B_per:BB_per, O_per) %>% 
  mutate(
    groupBB =
      case_when(
        BB_per <= 0.05 ~ 1,
        BB_per > 0.05 & BB_per <= 0.075 ~ 2,
        BB_per > 0.075 & BB_per <= 0.1 ~ 3,
        BB_per > 0.1 & BB_per <= 0.125 ~ 4,
        BB_per > 0.125 & BB_per <= 0.15 ~ 5,
        .default = 6
      ),
    groupK =
      case_when(
        K_per <= 0.15 ~ 1,
        K_per > 0.15 & K_per <= 0.20 ~ 2,
        K_per > 0.2 & K_per <= 0.25 ~ 3,
        K_per > 0.25 & K_per <= 0.30 ~ 4,
        K_per > 0.3 & K_per <= 0.35 ~ 5,
        .default = 6
      ),
    group1B =
      case_when(
        X1B_per <= 0.05 ~ 1,
        X1B_per > 0.05 & X1B_per <= 0.10 ~ 2,
        X1B_per > 0.1 & X1B_per <= 0.125 ~ 3,
        X1B_per > 0.125 & X1B_per <= 0.15 ~ 4,
        X1B_per > 0.15 & X1B_per <= 0.175 ~ 5,
        X1B_per > 0.175 & X1B_per <= 0.2 ~ 6,
        .default = 7
      ),
    group2B =
      case_when(
        X2B_per <= 0.025 ~ 1,
        X2B_per > 0.025 & X2B_per <= 0.04 ~ 2,
        X2B_per > 0.04 & X2B_per <= 0.05 ~ 3,
        X2B_per > 0.05 & X2B_per <= 0.07 ~ 4,
        .default = 5
      ),
    group3B =
      case_when(
        X3B_per <= 0.001 ~ 1,
        X3B_per > 0.001 & X3B_per <= 0.002 ~ 2,
        X3B_per > 0.002 & X3B_per <= 0.003 ~ 3,
        X3B_per > 0.003 & X3B_per <= 0.004 ~ 4,
        X3B_per > 0.004 & X3B_per <= 0.005 ~ 5,
        X3B_per > 0.005 & X3B_per <= 0.006 ~ 6,
        X3B_per > 0.006 & X3B_per <= 0.007 ~ 7,
        .default = 8
      ),
    groupHR =
      case_when(
        HR_per <= 0.01 ~ 1,
        HR_per > 0.01 & HR_per <= 0.015 ~ 2,
        HR_per > 0.015 & HR_per <= 0.03 ~ 3,
        HR_per > 0.03 & HR_per <= 0.045 ~ 4,
        HR_per > 0.045 & HR_per <= 0.07 ~ 5,
        .default = 6
      ),
    groupHBP =
      case_when(
        HBP_per <= 0.005 ~ 1,
        HBP_per > 0.005 & HBP_per <= 0.0075 ~ 2,
        HBP_per > 0.0075 & HBP_per <= 0.01 ~ 3,
        HBP_per > 0.01 & HBP_per <= 0.015 ~ 4,
        HBP_per > 0.015 & HBP_per <= 0.02 ~ 5,
        HBP_per > 0.02 & HBP_per <= 0.04 ~ 6,
        .default = 7
      ),
    groupBABIP =
      case_when(
        babip <= 0.2 ~ 1,
        babip > 0.2 & babip <= 0.25 ~ 2,
        babip > 0.25 & babip <= 0.275 ~ 3,
        babip > 0.275 & babip <= 0.3 ~ 4,
        babip > 0.3 & babip <= 0.325 ~ 5,
        babip > 0.325 & babip <= 0.375 ~ 6,
        .default = 7
      )) %>% 
  group_by(groupBB) %>% 
  mutate(totalBB_per = round(sum(bb)/sum(pa),3)) %>% 
  ungroup() %>% 
  group_by(groupK) %>%
  mutate(totalK_per = round(sum(so)/sum(pa),3)) %>%
  ungroup() %>% 
  group_by(group1B) %>%
  mutate(total1B_per = round(sum(singles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group2B) %>%
  mutate(total2B_per = round(sum(doubles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group3B) %>%
  mutate(total3B_per = round(sum(triples)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHR) %>%
  mutate(totalHR_per = round(sum(hrs)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHBP) %>%
  mutate(totalHBP_per = round(sum(hbp)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupBABIP) %>%
  mutate(avgBABIP = round(mean(babip),3)) %>%
  arrange(desc(pa)) %>% 
  head(500)
x3 = data3 %>% 
  mutate(season = 2023) %>% 
  mutate(player_type = 2) %>% 
  mutate(hbp = pa - bip - so - bb) %>%
  mutate(X1B_per = singles/pa + 0.001,
         X2B_per = doubles/pa + 0.001,
         X3B_per = triples/pa + 0.001,
         HR_per = hrs/pa + 0.001,
         HBP_per = hbp/pa + 0.001,
         K_per = k_percent/100,
         BB_per = bb_percent/100) %>%
  mutate(O_per = 1 - (X1B_per + X2B_per + X3B_per + HR_per + HBP_per + BB_per)) %>% 
  select(player_name, season, player_type, pa:bb_percent, hbp, babip, X1B_per:BB_per, O_per) %>% 
  mutate(
    groupBB =
      case_when(
        BB_per <= 0.05 ~ 1,
        BB_per > 0.05 & BB_per <= 0.075 ~ 2,
        BB_per > 0.075 & BB_per <= 0.1 ~ 3,
        BB_per > 0.1 & BB_per <= 0.125 ~ 4,
        BB_per > 0.125 & BB_per <= 0.15 ~ 5,
        .default = 6
      ),
    groupK =
      case_when(
        K_per <= 0.15 ~ 1,
        K_per > 0.15 & K_per <= 0.20 ~ 2,
        K_per > 0.2 & K_per <= 0.25 ~ 3,
        K_per > 0.25 & K_per <= 0.30 ~ 4,
        K_per > 0.3 & K_per <= 0.35 ~ 5,
        .default = 6
      ),
    group1B =
      case_when(
        X1B_per <= 0.05 ~ 1,
        X1B_per > 0.05 & X1B_per <= 0.10 ~ 2,
        X1B_per > 0.1 & X1B_per <= 0.125 ~ 3,
        X1B_per > 0.125 & X1B_per <= 0.15 ~ 4,
        X1B_per > 0.15 & X1B_per <= 0.175 ~ 5,
        X1B_per > 0.175 & X1B_per <= 0.2 ~ 6,
        .default = 7
      ),
    group2B =
      case_when(
        X2B_per <= 0.025 ~ 1,
        X2B_per > 0.025 & X2B_per <= 0.04 ~ 2,
        X2B_per > 0.04 & X2B_per <= 0.05 ~ 3,
        X2B_per > 0.05 & X2B_per <= 0.07 ~ 4,
        .default = 5
      ),
    group3B =
      case_when(
        X3B_per <= 0.001 ~ 1,
        X3B_per > 0.001 & X3B_per <= 0.002 ~ 2,
        X3B_per > 0.002 & X3B_per <= 0.003 ~ 3,
        X3B_per > 0.003 & X3B_per <= 0.004 ~ 4,
        X3B_per > 0.004 & X3B_per <= 0.005 ~ 5,
        X3B_per > 0.005 & X3B_per <= 0.006 ~ 6,
        X3B_per > 0.006 & X3B_per <= 0.007 ~ 7,
        .default = 8
      ),
    groupHR =
      case_when(
        HR_per <= 0.01 ~ 1,
        HR_per > 0.01 & HR_per <= 0.015 ~ 2,
        HR_per > 0.015 & HR_per <= 0.03 ~ 3,
        HR_per > 0.03 & HR_per <= 0.045 ~ 4,
        HR_per > 0.045 & HR_per <= 0.07 ~ 5,
        .default = 6
      ),
    groupHBP =
      case_when(
        HBP_per <= 0.005 ~ 1,
        HBP_per > 0.005 & HBP_per <= 0.0075 ~ 2,
        HBP_per > 0.0075 & HBP_per <= 0.01 ~ 3,
        HBP_per > 0.01 & HBP_per <= 0.015 ~ 4,
        HBP_per > 0.015 & HBP_per <= 0.02 ~ 5,
        HBP_per > 0.02 & HBP_per <= 0.04 ~ 6,
        .default = 7
      ),
    groupBABIP =
      case_when(
        babip <= 0.2 ~ 1,
        babip > 0.2 & babip <= 0.25 ~ 2,
        babip > 0.25 & babip <= 0.275 ~ 3,
        babip > 0.275 & babip <= 0.3 ~ 4,
        babip > 0.3 & babip <= 0.325 ~ 5,
        babip > 0.325 & babip <= 0.375 ~ 6,
        .default = 7
      )) %>% 
  group_by(groupBB) %>% 
  mutate(totalBB_per = round(sum(bb)/sum(pa),3)) %>% 
  ungroup() %>% 
  group_by(groupK) %>%
  mutate(totalK_per = round(sum(so)/sum(pa),3)) %>%
  ungroup() %>% 
  group_by(group1B) %>%
  mutate(total1B_per = round(sum(singles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group2B) %>%
  mutate(total2B_per = round(sum(doubles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group3B) %>%
  mutate(total3B_per = round(sum(triples)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHR) %>%
  mutate(totalHR_per = round(sum(hrs)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHBP) %>%
  mutate(totalHBP_per = round(sum(hbp)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupBABIP) %>%
  mutate(avgBABIP = round(mean(babip),3)) %>%
  arrange(desc(pa)) %>% 
  head(500)

x4 = data4 %>% 
  mutate(season = 2023) %>% 
  mutate(player_type = 1) %>% 
  mutate(hbp = pa - bip - so - bb) %>%
  mutate(X1B_per = singles/pa + 0.001,
         X2B_per = doubles/pa + 0.001,
         X3B_per = triples/pa + 0.001,
         HR_per = hrs/pa + 0.001,
         HBP_per = hbp/pa + 0.001,
         K_per = k_percent/100,
         BB_per = bb_percent/100) %>%
  mutate(O_per = 1 - (X1B_per + X2B_per + X3B_per + HR_per + HBP_per + BB_per)) %>% 
  select(player_name, season, player_type, pa:bb_percent, hbp, babip, X1B_per:BB_per, O_per) %>% 
  mutate(
    groupBB =
      case_when(
        BB_per <= 0.05 ~ 1,
        BB_per > 0.05 & BB_per <= 0.075 ~ 2,
        BB_per > 0.075 & BB_per <= 0.1 ~ 3,
        BB_per > 0.1 & BB_per <= 0.125 ~ 4,
        BB_per > 0.125 & BB_per <= 0.15 ~ 5,
        .default = 6
      ),
    groupK =
      case_when(
        K_per <= 0.15 ~ 1,
        K_per > 0.15 & K_per <= 0.20 ~ 2,
        K_per > 0.2 & K_per <= 0.25 ~ 3,
        K_per > 0.25 & K_per <= 0.30 ~ 4,
        K_per > 0.3 & K_per <= 0.35 ~ 5,
        .default = 6
      ),
    group1B =
      case_when(
        X1B_per <= 0.05 ~ 1,
        X1B_per > 0.05 & X1B_per <= 0.10 ~ 2,
        X1B_per > 0.1 & X1B_per <= 0.125 ~ 3,
        X1B_per > 0.125 & X1B_per <= 0.15 ~ 4,
        X1B_per > 0.15 & X1B_per <= 0.175 ~ 5,
        X1B_per > 0.175 & X1B_per <= 0.2 ~ 6,
        .default = 7
      ),
    group2B =
      case_when(
        X2B_per <= 0.025 ~ 1,
        X2B_per > 0.025 & X2B_per <= 0.04 ~ 2,
        X2B_per > 0.04 & X2B_per <= 0.05 ~ 3,
        X2B_per > 0.05 & X2B_per <= 0.07 ~ 4,
        .default = 5
      ),
    group3B =
      case_when(
        X3B_per <= 0.001 ~ 1,
        X3B_per > 0.001 & X3B_per <= 0.002 ~ 2,
        X3B_per > 0.002 & X3B_per <= 0.003 ~ 3,
        X3B_per > 0.003 & X3B_per <= 0.004 ~ 4,
        X3B_per > 0.004 & X3B_per <= 0.005 ~ 5,
        X3B_per > 0.005 & X3B_per <= 0.006 ~ 6,
        X3B_per > 0.006 & X3B_per <= 0.007 ~ 7,
        .default = 8
      ),
    groupHR =
      case_when(
        HR_per <= 0.01 ~ 1,
        HR_per > 0.01 & HR_per <= 0.015 ~ 2,
        HR_per > 0.015 & HR_per <= 0.03 ~ 3,
        HR_per > 0.03 & HR_per <= 0.045 ~ 4,
        HR_per > 0.045 & HR_per <= 0.07 ~ 5,
        .default = 6
      ),
    groupHBP =
      case_when(
        HBP_per <= 0.005 ~ 1,
        HBP_per > 0.005 & HBP_per <= 0.0075 ~ 2,
        HBP_per > 0.0075 & HBP_per <= 0.01 ~ 3,
        HBP_per > 0.01 & HBP_per <= 0.015 ~ 4,
        HBP_per > 0.015 & HBP_per <= 0.02 ~ 5,
        HBP_per > 0.02 & HBP_per <= 0.04 ~ 6,
        .default = 7
      ),
    groupBABIP =
      case_when(
        babip <= 0.2 ~ 1,
        babip > 0.2 & babip <= 0.25 ~ 2,
        babip > 0.25 & babip <= 0.275 ~ 3,
        babip > 0.275 & babip <= 0.3 ~ 4,
        babip > 0.3 & babip <= 0.325 ~ 5,
        babip > 0.325 & babip <= 0.375 ~ 6,
        .default = 7
      )) %>% 
  group_by(groupBB) %>% 
  mutate(totalBB_per = round(sum(bb)/sum(pa),3)) %>% 
  ungroup() %>% 
  group_by(groupK) %>%
  mutate(totalK_per = round(sum(so)/sum(pa),3)) %>%
  ungroup() %>% 
  group_by(group1B) %>%
  mutate(total1B_per = round(sum(singles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group2B) %>%
  mutate(total2B_per = round(sum(doubles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group3B) %>%
  mutate(total3B_per = round(sum(triples)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHR) %>%
  mutate(totalHR_per = round(sum(hrs)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHBP) %>%
  mutate(totalHBP_per = round(sum(hbp)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupBABIP) %>%
  mutate(avgBABIP = round(mean(babip),3)) %>%
  arrange(desc(pa)) %>% 
  head(500)

x5 = data5 %>% 
  mutate(season = 2022) %>% 
  mutate(player_type = 2) %>% 
  mutate(hbp = pa - bip - so - bb) %>%
  mutate(X1B_per = singles/pa + 0.001,
         X2B_per = doubles/pa + 0.001,
         X3B_per = triples/pa + 0.001,
         HR_per = hrs/pa + 0.001,
         HBP_per = hbp/pa + 0.001,
         K_per = k_percent/100,
         BB_per = bb_percent/100) %>%
  mutate(O_per = 1 - (X1B_per + X2B_per + X3B_per + HR_per + HBP_per + BB_per)) %>% 
  select(player_name, season, player_type, pa:bb_percent, hbp, babip, X1B_per:BB_per, O_per) %>% 
  mutate(
    groupBB =
      case_when(
        BB_per <= 0.05 ~ 1,
        BB_per > 0.05 & BB_per <= 0.075 ~ 2,
        BB_per > 0.075 & BB_per <= 0.1 ~ 3,
        BB_per > 0.1 & BB_per <= 0.125 ~ 4,
        BB_per > 0.125 & BB_per <= 0.15 ~ 5,
        .default = 6
      ),
    groupK =
      case_when(
        K_per <= 0.15 ~ 1,
        K_per > 0.15 & K_per <= 0.20 ~ 2,
        K_per > 0.2 & K_per <= 0.25 ~ 3,
        K_per > 0.25 & K_per <= 0.30 ~ 4,
        K_per > 0.3 & K_per <= 0.35 ~ 5,
        .default = 6
      ),
    group1B =
      case_when(
        X1B_per <= 0.05 ~ 1,
        X1B_per > 0.05 & X1B_per <= 0.10 ~ 2,
        X1B_per > 0.1 & X1B_per <= 0.125 ~ 3,
        X1B_per > 0.125 & X1B_per <= 0.15 ~ 4,
        X1B_per > 0.15 & X1B_per <= 0.175 ~ 5,
        X1B_per > 0.175 & X1B_per <= 0.2 ~ 6,
        .default = 7
      ),
    group2B =
      case_when(
        X2B_per <= 0.025 ~ 1,
        X2B_per > 0.025 & X2B_per <= 0.04 ~ 2,
        X2B_per > 0.04 & X2B_per <= 0.05 ~ 3,
        X2B_per > 0.05 & X2B_per <= 0.07 ~ 4,
        .default = 5
      ),
    group3B =
      case_when(
        X3B_per <= 0.001 ~ 1,
        X3B_per > 0.001 & X3B_per <= 0.002 ~ 2,
        X3B_per > 0.002 & X3B_per <= 0.003 ~ 3,
        X3B_per > 0.003 & X3B_per <= 0.004 ~ 4,
        X3B_per > 0.004 & X3B_per <= 0.005 ~ 5,
        X3B_per > 0.005 & X3B_per <= 0.006 ~ 6,
        X3B_per > 0.006 & X3B_per <= 0.007 ~ 7,
        .default = 8
      ),
    groupHR =
      case_when(
        HR_per <= 0.01 ~ 1,
        HR_per > 0.01 & HR_per <= 0.015 ~ 2,
        HR_per > 0.015 & HR_per <= 0.03 ~ 3,
        HR_per > 0.03 & HR_per <= 0.045 ~ 4,
        HR_per > 0.045 & HR_per <= 0.07 ~ 5,
        .default = 6
      ),
    groupHBP =
      case_when(
        HBP_per <= 0.005 ~ 1,
        HBP_per > 0.005 & HBP_per <= 0.0075 ~ 2,
        HBP_per > 0.0075 & HBP_per <= 0.01 ~ 3,
        HBP_per > 0.01 & HBP_per <= 0.015 ~ 4,
        HBP_per > 0.015 & HBP_per <= 0.02 ~ 5,
        HBP_per > 0.02 & HBP_per <= 0.04 ~ 6,
        .default = 7
      ),
    groupBABIP =
      case_when(
        babip <= 0.2 ~ 1,
        babip > 0.2 & babip <= 0.25 ~ 2,
        babip > 0.25 & babip <= 0.275 ~ 3,
        babip > 0.275 & babip <= 0.3 ~ 4,
        babip > 0.3 & babip <= 0.325 ~ 5,
        babip > 0.325 & babip <= 0.375 ~ 6,
        .default = 7
      )) %>% 
  group_by(groupBB) %>% 
  mutate(totalBB_per = round(sum(bb)/sum(pa),3)) %>% 
  ungroup() %>% 
  group_by(groupK) %>%
  mutate(totalK_per = round(sum(so)/sum(pa),3)) %>%
  ungroup() %>% 
  group_by(group1B) %>%
  mutate(total1B_per = round(sum(singles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group2B) %>%
  mutate(total2B_per = round(sum(doubles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group3B) %>%
  mutate(total3B_per = round(sum(triples)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHR) %>%
  mutate(totalHR_per = round(sum(hrs)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHBP) %>%
  mutate(totalHBP_per = round(sum(hbp)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupBABIP) %>%
  mutate(avgBABIP = round(mean(babip),3)) %>%
  arrange(desc(pa)) %>% 
  head(500)
x6 = data6 %>% 
  mutate(season = 2022) %>% 
  mutate(player_type = 1) %>% 
  mutate(hbp = pa - bip - so - bb) %>%
  mutate(X1B_per = singles/pa + 0.001,
         X2B_per = doubles/pa + 0.001,
         X3B_per = triples/pa + 0.001,
         HR_per = hrs/pa + 0.001,
         HBP_per = hbp/pa + 0.001,
         K_per = k_percent/100,
         BB_per = bb_percent/100) %>%
  mutate(O_per = 1 - (X1B_per + X2B_per + X3B_per + HR_per + HBP_per + BB_per)) %>% 
  select(player_name, season, player_type, pa:bb_percent, hbp, babip, X1B_per:BB_per, O_per) %>% 
  mutate(
    groupBB =
      case_when(
        BB_per <= 0.05 ~ 1,
        BB_per > 0.05 & BB_per <= 0.075 ~ 2,
        BB_per > 0.075 & BB_per <= 0.1 ~ 3,
        BB_per > 0.1 & BB_per <= 0.125 ~ 4,
        BB_per > 0.125 & BB_per <= 0.15 ~ 5,
        .default = 6
      ),
    groupK =
      case_when(
        K_per <= 0.15 ~ 1,
        K_per > 0.15 & K_per <= 0.20 ~ 2,
        K_per > 0.2 & K_per <= 0.25 ~ 3,
        K_per > 0.25 & K_per <= 0.30 ~ 4,
        K_per > 0.3 & K_per <= 0.35 ~ 5,
        .default = 6
      ),
    group1B =
      case_when(
        X1B_per <= 0.05 ~ 1,
        X1B_per > 0.05 & X1B_per <= 0.10 ~ 2,
        X1B_per > 0.1 & X1B_per <= 0.125 ~ 3,
        X1B_per > 0.125 & X1B_per <= 0.15 ~ 4,
        X1B_per > 0.15 & X1B_per <= 0.175 ~ 5,
        X1B_per > 0.175 & X1B_per <= 0.2 ~ 6,
        .default = 7
      ),
    group2B =
      case_when(
        X2B_per <= 0.025 ~ 1,
        X2B_per > 0.025 & X2B_per <= 0.04 ~ 2,
        X2B_per > 0.04 & X2B_per <= 0.05 ~ 3,
        X2B_per > 0.05 & X2B_per <= 0.07 ~ 4,
        .default = 5
      ),
    group3B =
      case_when(
        X3B_per <= 0.001 ~ 1,
        X3B_per > 0.001 & X3B_per <= 0.002 ~ 2,
        X3B_per > 0.002 & X3B_per <= 0.003 ~ 3,
        X3B_per > 0.003 & X3B_per <= 0.004 ~ 4,
        X3B_per > 0.004 & X3B_per <= 0.005 ~ 5,
        X3B_per > 0.005 & X3B_per <= 0.006 ~ 6,
        X3B_per > 0.006 & X3B_per <= 0.007 ~ 7,
        .default = 8
      ),
    groupHR =
      case_when(
        HR_per <= 0.01 ~ 1,
        HR_per > 0.01 & HR_per <= 0.015 ~ 2,
        HR_per > 0.015 & HR_per <= 0.03 ~ 3,
        HR_per > 0.03 & HR_per <= 0.045 ~ 4,
        HR_per > 0.045 & HR_per <= 0.07 ~ 5,
        .default = 6
      ),
    groupHBP =
      case_when(
        HBP_per <= 0.005 ~ 1,
        HBP_per > 0.005 & HBP_per <= 0.0075 ~ 2,
        HBP_per > 0.0075 & HBP_per <= 0.01 ~ 3,
        HBP_per > 0.01 & HBP_per <= 0.015 ~ 4,
        HBP_per > 0.015 & HBP_per <= 0.02 ~ 5,
        HBP_per > 0.02 & HBP_per <= 0.04 ~ 6,
        .default = 7
      ),
    groupBABIP =
      case_when(
        babip <= 0.2 ~ 1,
        babip > 0.2 & babip <= 0.25 ~ 2,
        babip > 0.25 & babip <= 0.275 ~ 3,
        babip > 0.275 & babip <= 0.3 ~ 4,
        babip > 0.3 & babip <= 0.325 ~ 5,
        babip > 0.325 & babip <= 0.375 ~ 6,
        .default = 7
      )) %>% 
  group_by(groupBB) %>% 
  mutate(totalBB_per = round(sum(bb)/sum(pa),3)) %>% 
  ungroup() %>% 
  group_by(groupK) %>%
  mutate(totalK_per = round(sum(so)/sum(pa),3)) %>%
  ungroup() %>% 
  group_by(group1B) %>%
  mutate(total1B_per = round(sum(singles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group2B) %>%
  mutate(total2B_per = round(sum(doubles)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(group3B) %>%
  mutate(total3B_per = round(sum(triples)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHR) %>%
  mutate(totalHR_per = round(sum(hrs)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupHBP) %>%
  mutate(totalHBP_per = round(sum(hbp)/sum(pa),3) + 0.001) %>%
  ungroup() %>% 
  group_by(groupBABIP) %>%
  mutate(avgBABIP = round(mean(babip),3)) %>%
  arrange(desc(pa)) %>% 
  head(500)

pitchers = rbind(x, x3, x5)
batters = rbind(x2, x4, x6)

#Models for each statistic based on article

modelbb = lm(log(batters$totalBB_per) ~ log(batters$BB_per) + log(pitchers$BB_per))
summary(modelbb)
modelk =  lm(log(batters$totalK_per) ~ log(batters$K_per) + log(pitchers$K_per))
summary(modelk)
model1b = lm(log(batters$total1B_per) ~ log(batters$X1B_per) + log(pitchers$X1B_per))
summary(model1b)
model2b = lm(batters$total2B_per ~ batters$X2B_per + pitchers$X2B_per)
summary(model2b)
model3b = lm(log(batters$total3B_per) ~ log(batters$X3B_per) + log(pitchers$X3B_per))
summary(model3b)
modelhr = lm(log(batters$totalHR_per) ~ log(batters$HR_per) + log(pitchers$HR_per))
summary(modelhr)
modelhbp = lm(log(batters$totalHBP_per) ~ log(batters$HBP_per) + log(pitchers$HBP_per))
summary(modelhbp)
modelbabip = lm(batters$avgBABIP ~ batters$babip + pitchers$babip)
summary(modelbabip)

library(shiny)
Outcomes = data.frame(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0)
colnames(Outcomes) = c("K%", "BB%", "1B%", "2B%", "3B%", "HR%", "HBP%", "BABIP")

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Predicting Baseball Matchups",
  
  tabPanel(
    title = "At Bat Outcomes",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "bYear", label = "Select Batter Year", 
                    choices = c(2022, 2023, 2024)),
        
        selectInput(inputId = "batter", label =  "Select a Batter", 
                    choices = batters$player_name),
        
        selectInput(inputId = "pYear", label = "Select Pitcher Year",
                    choices = c(2022, 2023, 2024)),
        
        selectInput(inputId = "pitcher", label = "Select a Pitcher", 
                    choices = pitchers$player_name)
      ),
      mainPanel(tableOutput("table"))
    )
  ),
  tabPanel(title = "About",
           includeMarkdown("about.Rmd"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  batter_change = reactive({
    batters %>% filter(season == input$bYear)
  })
   
  observeEvent(
    eventExpr = input$bYear,
    handlerExpr = {
      updateSelectInput(inputId = "batter", choices = sort(batter_change()$player_name))
    }
  )
   
  pitcher_change = reactive({
    pitchers %>% filter(season == input$pYear)
  })
   
  observeEvent(
    eventExpr = input$pYear,
    handlerExpr = {
    updateSelectInput(inputId = "pitcher", choices = sort(pitcher_change()$player_name))
    }
  )
  
  output$table = renderTable({
    bat = batters %>% filter(season == input$bYear & player_name == input$batter)
    
    pitch = pitchers %>% filter(season == input$pYear & player_name == input$pitcher)
    
    K_p = round((exp(-0.118988 + (0.912456 * log(bat$K_per)) + 
                (0.006092 * log(pitch$K_per))) * 100), 2)
    BB_p = round((exp(-0.42612 + (0.85323 * log(bat$BB_per)) + 
                        (-0.02054 * log(pitch$BB_per))) * 100), 2)
    B1_p = round((exp(-0.13415 + (0.93351 * log(bat$X1B_per)) + 
                        (-0.00189 * log(pitch$X1B_per))) * 100), 2)
    B2_p = round((0.003617 + (0.910425 * bat$X2B_per) + 
                        (0.004065 * pitch$X2B_per)) * 100, 2)
    B3_p = round((exp(-0.173629 + (0.963592 * log(bat$X3B_per)) + 
                        (0.008845 * log(pitch$X3B_per))) * 100), 2)
    HR_p = round((exp(-0.98431 + (0.70834 * log(bat$HR_per)) + 
                        (0.01573 * log(pitch$HR_per))) * 100), 2)
    HBP_p = round((exp(-0.913345 + (0.793687 * log(bat$HBP_per)) + 
                         (0.001231 * log(pitch$HBP_per))) * 100), 2)
    BABIP_p = round(-0.022466 + (1.065888 * bat$babip) + 
                       (0.006571 * pitch$babip), 3)
    Outcomes[1, ] = c(K_p, BB_p, B1_p, B2_p, B3_p, HR_p, HBP_p, BABIP_p)
    Outcomes
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
