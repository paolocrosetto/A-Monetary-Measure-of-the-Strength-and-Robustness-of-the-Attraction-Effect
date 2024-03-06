####
#### Replication package for Crosetto & Gaudeul, Economics Letters 2016
####
####
#### Data cleaning

#### This script

## runs the regressions of Table 1
## 
## for each regression, it 
##   - saves the ADE monetary measure
##   - saves the BIAS measure
##   - generates the plots of the predicted curves + marginal histograms
##   - runs tests of normality of the regressions
##   - runs tests of difference from zero of the ADE / difference from 50% of the BIAS measures


#### 1. Regressions #####

## Model 1: all choices

mod1 <- glmer(LPCSchosen ~  profitpremium_perc + (1+profitpremium_perc | subject), 
              data = df,
              family = binomial, 
              control = glmerControl(optimizer = "bobyqa"))

## Model 2: dropping decoy choices

mod2 <- glmer(LPCSchosen ~  profitpremium_perc + (1+profitpremium_perc | subject), 
              data = df[df$HPCSchosen==0,], 
              family = binomial, 
              control = glmerControl(optimizer = "bobyqa"))


## output regressions to table

modelsummary(list("Model 1" = mod1, "Model 2" = mod2), 
             estimate = "{estimate}{stars}",
             coef_rename = c("(Intercept)" = "Constant",
                             "profitpremium_perc" = "Profit premium",
                             "SD (Intercept subject)" = "Var(u_i)",
                             "SD (profitpremium_perc subject)" = "Var(v_i)"),
             output = "latex") %>% 
  kableExtra::save_kable("Tables/Table_1.pdf")
