calc_ops = function(data){
  
  data %>% 
    mutate(PA = AB + BB + HBP + SH + SF,
           TB = H + X2B + 2 * X3B + 3 * HR,
           OBP = round((H + BB + HBP) / (PA - SH),3),
           SLG = round(TB / AB, 3),
           OPS = round((OBP + SLG), 3)) %>% 
    select(-PA, -TB, -OBP, -SLG)
}
