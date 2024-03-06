
### computing mean payoffs

df %>% 
  select(id, payoff) %>% 
  distinct() %>% 
  summarise(mean(payoff))
