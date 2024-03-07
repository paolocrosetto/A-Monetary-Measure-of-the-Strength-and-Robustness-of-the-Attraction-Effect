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




###shares of subjects subject to the different types of ADE
#share of traditional ade
ftable(cut(ade$ADE, breaks=c(-10000,0.5,10000)))/202
#share of monetary ADE
ftable(cut(mcoef$bias, breaks=c(-10000,0,10000)))/202
###

#################################################
###### 2. lpcs vs. is only -- dropping dominated ######
#################################################

#bias different from 0
biastest <- t.test(mcoef$bias)
biastest$p.value
adetest <- t.test(ade$ADE, mu=0.5)
adetest$p.value

###shares of subjects subject to the different types of ADE
#share of traditional ade
ftable(cut(ade$ADE, breaks=c(-10000,0.5,10000)))/164
#share of monetary ADE
ftable(cut(mcoef$bias, breaks=c(-10000,0,10000)))/202
###
