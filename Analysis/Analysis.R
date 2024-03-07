###
### replicate all results in Crosetto & Gaudeul, Economics Letters ADE paper
###



#### 0. Preliminaries ####

### needed libraries

library(tidyverse)      ### R dialect used in this file
     
library(lme4)           ### Library to run the regression
     
library(grid)           ### Used to patch plots together 
library(gridExtra)      ### Used to patch plots together 

library(tinytable)      ### Used to format & output descriptive stats
library(modelsummary)   ### Used to format & output regression tables


#### 1. importing data #####
df <- read_csv("Data/raw_data.csv")


#### 3. Data cleaning #####

source("Analysis/Clean_data.R")

#### Descriptive statistics ####

source("Analysis/Share_choice.R")     ## share of target, competitor, decoy chosen (TODO BETTER FORMAT)

#### 4. Figure 3 ####

source("Analysis/Figure_3.R")

#### 5. Regression: table, plots & tests #####

source("Analysis/Regressions.R")
