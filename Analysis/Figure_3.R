####
#### Replication package for Crosetto & Gaudeul, Economics Letters 2016
####
####
#### Data cleaning

#### This script

## generates and saves Figure 3



## plotting

ggplot(df, aes(x=profitpremium_abs, y=1))+
  geom_hline(aes(yintercept=1), linetype='dotted', color='light grey')+
  geom_point(size=2.5, shape=124)+
  geom_vline(aes(xintercept=0), linetype='dashed')+
  labs(x = "", y = "")+
  scale_x_continuous(breaks = c(-10,-7.5,-5,-2.5,0,2.5,5,7.5,10),
                     labels = c("-10€","-7.5€","-5€","-2.5€","0","2.5€", "5€","7.5€", "10€"))+
  coord_cartesian(xlim = c(-10,10), ylim =c(0.95,1.05))+
  theme_minimal()+
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(), 
        plot.background = element_rect(fill = "white", color = "white"))

## saving
ggsave("Figures/Figure_3.png" , width=7, height=1.5, units = "in", dpi = "retina")
