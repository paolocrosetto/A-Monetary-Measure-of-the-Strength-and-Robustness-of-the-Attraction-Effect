####
#### Replication package for Crosetto & Gaudeul, Economics Letters 2016
####
####
#### Data cleaning

#### This script

## the data come from a larger campaign, testing menus of length 3 and 6
## they contain also demographics, replies to control tasks, and more
## in this script we
##  - restrict attention to 3-menus
##  - discard unused variables
##  - clean variables to be ready for plotting / regressions


# restrictions/filters
df <- df %>% 
  filter(menulength == 3,  ## only 3-menus
         CS != 0           ## only menus where there is dominance
         )

# creating/modifying variables
df <- df %>% 
  mutate(subject = session*100 + subject,                           ## create a unique subject identifier
         profitpremium_perc = 100*lpcsdiff/optprofit,               ## comput relative profit diff
         type_choice = case_when(LPCSchosen == 1 ~ "Target", 
                                 HPCSchosen == 1 ~ "Decoy", 
                                 ISchosen   == 1 ~ "Competitor",    ## var to track if choice is T,C,D
                                 TRUE ~ NA_character_),
  )

# selecting only needed variables
df <- df %>% 
  select(-breakdown, -menulength, -menulength2, -session, 
         -choice4, -choice5, -choice6,
         -fac, -year, -ma1, -ma2, -ma3, 
         -shtask1, -shtask2, -shtask3, 
         -shtask4, -pr1, -pr2, -pr3, -pr4,
         -demand, -motivation, -sh4, -sh5, -sh6,
         -up4, -up5, -up6, -ar4, -ar5, -ar6,
         -is4, -is5, -is6)

# renaming variables
df <- df %>% 
  rename(profitpremium_abs = lpcsdiff)
