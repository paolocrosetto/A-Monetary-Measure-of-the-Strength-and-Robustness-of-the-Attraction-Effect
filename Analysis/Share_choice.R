### computing the share of choices that go to target, competitor, decoy


#table
prop.table(table(df$type_choice)) %>% 
  as.data.frame() %>% 
  rename(choice = Var1, Frequency = Freq) %>% 
  tt()
  
