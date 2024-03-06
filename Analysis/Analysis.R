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


#### 2. Needed functions #####
source("Analysis/Functions.R")


#### 3. Data cleaning #####

source("Analysis/Clean_data.R")

#### Descriptive statistics ####

source("Analysis/Share_choice.R")     ## share of target, competitor, decoy chosen (TODO BETTER FORMAT)

#### 4. Figure 3 ####

source("Analysis/Figure_3.R")

#### 5. Regression: table, plots & tests #####

source("Analysis/Regressions.R")

############# mixed logit models #################


#####################################
##### 1. lpcs vs. all other choices ######
#####################################


mod1 <- glmer(LPCSchosen ~  profitpremium_perc + (1+profitpremium_perc | subject), data = df,
           family = binomial, control = glmerControl(optimizer = "bobyqa"))
modA <- mod1
# predict psychometric function
df$predictions <- predict(mod1, newdata = df, type = "response", re.form = NULL, allow.new.levels = TRUE)

#are the RE normally distributed?
r_int<- ranef(mod1)$subject$`(Intercept)`
qqnorm(r_int)
qqline(r_int)
shapiro.test(r_int)

r_slope<- ranef(mod1)$subject$profitpremium
qqnorm(r_slope)
qqline(r_slope)
shapiro.test(r_slope)

# Compute bias
mcoef <-coef(mod1)[[1]]
mcoef$bias <- -mcoef$`(Intercept)`/mcoef$profitpremium

#addign for title-like feature
mcoef$titlestring = "Distribution of the monetary measure of ADE"

#fixed effect
mod1_bias_mean = -fixef(mod1)[[1]]/fixef(mod1)[[2]]

#mean, sd and confint of the bias
mod1_bias <- summarySE(mcoef, measurevar = "bias")

meanlabel <- paste("mean = ", round(mod1_bias_mean,2), "%")

##find and plot intercept
ade <- df %>% 
  select(id, predictions, profitpremium_perc) %>%
  filter(profitpremium_perc>-3 & profitpremium_perc<3) %>%
  group_by(id)%>%
  spread(profitpremium_perc, predictions)
names(ade) <- c("id","neg","pos")
ade <- ade %>% mutate(ADE = neg+((pos-neg)/0.5)*0.3)

#addign for title-like feature
ade$titlestring = "Distribution of the frequency measure of ADE"

mod1_ade <- summarySE(ade, measurevar = "ADE")

meanlabel2 <- paste("mean = ", round(mod1_ade$mean,3)*100, "%", sep="")

###testing: is bias different from 0? is ADE different from 0.5?
#bias different from 0
biastest <- t.test(mcoef$bias)
biastest$p.value
adetest <- t.test(ade$ADE, mu=0.5)
adetest$p.value

#main plot of predicted psychometric function by subject
main <- ggplot(df, aes(x = profitpremium_perc, y = predictions)) +
  geom_smooth(level=0.9999, color="cornflowerblue")+
  theme_minimal()+
  geom_vline(xintercept=0, color='indianred')+geom_hline(aes(yintercept=0.5),color='indianred', linetype="dashed")+
  theme(plot.title = element_text(lineheight=.8, face="bold", size=16))+
  xlab("% utility premium of target vs. competitor")+
  ylab("Predicted probability of choosing Target")+
  geom_line(aes(group=id), alpha=0.1)+
  coord_cartesian(xlim = c(-40,35), ylim = c(0.1,0.9))

#empty plot for the unused corner
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

#plot bias
bias <- ggplot(mcoef, aes(x = bias))+
       #adding histogram
       geom_histogram( color="grey80", fill="grey70", binwidth = 0.8) + 
       annotate("segment", x = mod1_bias_mean, xend = mod1_bias_mean, y=0, yend = 11.7, color="cornflowerblue", size=1)+
       annotate(geom = "rect", xmin= mod1_bias_mean-mod1_bias$se, 
                xmax=mod1_bias_mean+mod1_bias$se, ymin=0, ymax=11.7, fill="cornflowerblue", alpha=.1)+
       geom_vline(xintercept = 0, color='indianred')+
       theme_minimal()+ coord_cartesian(ylim=c(0,11.5),xlim = c(-40,35))+
       ylab("# of subjects")+xlab("")+theme(axis.text.x = element_blank())+
       scale_y_continuous(breaks=c(0,2.5,5,7.5,10,12.5))+
       facet_grid(.~titlestring)


#plot ADE
ADE <- ggplot(ade, aes(x = ADE))+
  geom_histogram(color="grey80", fill="grey70", binwidth = 0.01) + 
  annotate("segment", x = mod1_ade$mean, xend = mod1_ade$mean, y=0, yend = 17, color="cornflowerblue", size=1)+
  annotate(geom = "rect", xmin= mod1_ade$mean-mod1_ade$se, 
           xmax=mod1_ade$mean+mod1_ade$se, ymin=0, ymax=17, fill='cornflowerblue', alpha=.1)+
  geom_vline(xintercept = 0.5, color='indianred', linetype="dashed")+
  #appearance
  coord_flip(xlim=c(0.1,0.9))+
  xlab("")+ylab("# of subjects")+
  theme_minimal()+theme(axis.text.y = element_blank())+
  facet_grid(titlestring~.)

# putting them all together
png("marginalDist_all.png", width = 9, height = 9, units = "in", res = 300)
grid.arrange(bias, empty, main, ADE, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
dev.off()


###shares of subjects subject to the different types of ADE
#share of traditional ade
ftable(cut(ade$ADE, breaks=c(-10000,0.5,10000)))/202
#share of monetary ADE
ftable(cut(mcoef$bias, breaks=c(-10000,0,10000)))/202
###

#################################################
###### 2. lpcs vs. is only -- dropping dominated ######
#################################################

#dropping
df1 <- df1[df1$HPCSchosen==0,]

mod1 <- glmer(LPCSchosen ~  profitpremium + (1+profitpremium| subject), data = df1, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"))
df1$predictions <- predict(mod1, newdata = df1, type = "response", re.form = NULL, allow.new.levels = TRUE)

mod2 <- mod1

#are the RE normally distributed?
r_int<- ranef(mod2)$subject$`(Intercept)`
qqnorm(r_int)
qqline(r_int)
shapiro.test(r_int)

r_slope<- ranef(mod2)$subject$profitpremium
qqnorm(r_slope)
qqline(r_slope)
shapiro.test(r_slope)

#bias different from 0
biastest <- t.test(mcoef$bias)
biastest$p.value
adetest <- t.test(ade$ADE, mu=0.5)
adetest$p.value

## find and plot bias
# 1. average with confidence intervals

mcoef <-coef(mod1)[[1]]
mcoef$bias <- -mcoef$`(Intercept)`/mcoef$profitpremium

#addign for title-like feature
mcoef$titlestring = "Distribution of the monetary measure of ADE"

#fixed effect
mod1_bias_mean = -fixef(mod1)[[1]]/fixef(mod1)[[2]]

#mean, sd and confint of the bias
mod1_bias <- summarySE(mcoef, measurevar = "bias")

meanlabel <- paste("mean = ", round(mod1_bias_mean,2), "%", sep="")



##find and plot intercept
ade <- df1 %>% select(id, predictions, profitpremium) %>%
  filter(profitpremium>-3 & profitpremium<3) %>%
  group_by(id)%>%
  spread(profitpremium, predictions)
names(ade) <- c("id","neg","pos")
ade <- ade %>% mutate(ADE = neg+((pos-neg)/0.5)*0.3)
#clean away NAs
ade <- ade[!is.na(ade$ADE),]

mod1_ade <- summarySE(ade, measurevar = "ADE")
#addign for title-like feature
ade$titlestring = "Distribution of the frequency measure of ADE"


meanlabel <- paste("mean = ", round(mod1_ade$mean,3)*100, "%", sep="")


#main plot of predicted psychometric function by subject
main <- ggplot(df1, aes(x = profitpremium, y = predictions)) +
  geom_smooth(level=0.9999, color="cornflowerblue")+
  theme_minimal()+
  geom_vline(xintercept=0, color='indianred')+geom_hline(aes(yintercept=0.5),color='indianred', linetype="dashed")+
  theme(plot.title = element_text(lineheight=.8, face="bold", size=16))+
  xlab("% utility premium of target vs. competitor")+
  ylab("Predicted probability of choosing Target")+
  geom_line(aes(group=id), alpha=0.1)+
  coord_cartesian(xlim = c(-40,35), ylim = c(0.1,0.9))

#empty plot for the unused corner
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(                    
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

#plot bias
bias <- ggplot(mcoef, aes(x = bias))+
  #adding histogram
  geom_histogram( color="grey80", fill="grey70", binwidth = 0.8) + 
  annotate("segment", x = mod1_bias_mean, xend = mod1_bias_mean, y=0, yend = 15, color="cornflowerblue", size=1)+
  annotate(geom = "rect", xmin= mod1_bias_mean-mod1_bias$se, 
           xmax=mod1_bias_mean+mod1_bias$se, ymin=0, ymax=15, fill="cornflowerblue", alpha=.1)+
  geom_vline(xintercept = 0, color='indianred')+
  theme_minimal()+ coord_cartesian(ylim=c(0,15),xlim = c(-40,35))+
  ylab("# of subjects")+xlab("")+theme(axis.text.x = element_blank())+
  scale_y_continuous(breaks=c(0,2.5,5,7.5,10,12.5,15))+
  facet_grid(.~titlestring)


#plot ADE
ADE <- ggplot(ade, aes(x = ADE))+
  geom_histogram(color="grey80", fill="grey70", binwidth = 0.01) + 
  annotate("segment", x = mod1_ade$mean, xend = mod1_ade$mean, y=0, yend = 22, color="cornflowerblue", size=1)+
  annotate(geom = "rect", xmin= mod1_ade$mean-mod1_ade$se, 
           xmax=mod1_ade$mean+mod1_ade$se, ymin=0, ymax=22, fill='cornflowerblue', alpha=.1)+
  geom_vline(xintercept = 0.5, color='indianred', linetype="dashed")+
  #appearance
  coord_flip(xlim=c(0.1,0.9))+
  xlab("")+ylab("# of subjects")+
  theme_minimal()+theme(axis.text.y = element_blank())+
  facet_grid(titlestring~.)

# putting them all together
png("marginalDist_drop.png", width = 9, height = 9, units = "in", res = 300)
grid.arrange(bias, empty, main, ADE, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
dev.off()

###shares of subjects subject to the different types of ADE
#share of traditional ade
ftable(cut(ade$ADE, breaks=c(-10000,0.5,10000)))/164
#share of monetary ADE
ftable(cut(mcoef$bias, breaks=c(-10000,0,10000)))/202
###

###exporting table with regressions
library(stargazer)
stargazer(modA,mod2)
