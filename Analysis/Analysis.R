###
### replicate all results in Crosetto & Gaudeul, Economics Letters ADE paper
###



#### 0. Preliminaries ####

### needed libraries

library(tidyverse) ### R dialect used in this file

library(lme4)      ### Library to run the regression

library(grid)      ### Used to patch plots together 
library(gridExtra) ### Used to patch plots together 



### needed functions

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  #   # Rename the "mean" column    
  #   datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


#### 1. importing data #####
df <- read_csv("Data/raw_data.csv")


#### 2. Data cleaning #####

source("Clean_data.R")


### The original experiment was run on menus of 3 and 6 options. 
### For this paper, we restrict attention to menus with 3 options

df <- df[df$menulength==3,]


#generating a unique subject number
df$subject <- df$session*100+df$subject

#df1 contains CS menus only
df1 <- df[df$CS!=0,]

#### computing relative profit diff
df1 <- df1 %>% mutate(profitpremium = 100*lpcsdiff/optprofit)


#remove unused levels
df1$breakdown<-factor(df1$breakdown)

##### decoy profit
df1$decoyprofit <- 60 - 100*df1$maxLCS
df1$tdpdiff <- (df1$targetprofit - df1$decoyprofit)/df1$targetprofit

probt <- df1 %>% group_by(tdpdiff)%>%summarise(avgHPCSc = mean(HPCSchosen), avgLPCSc = mean(LPCSchosen))


########### plot 1 : menus by profit premium #######
#plot
png(file = "menus_payoffratio.png" , width=7, height=1.5, units = "in", res = 400)
# svglite::svglite("design_premium_by_menu.svg", width=7, height=2)
ggplot(df1, aes(x=profitpremium, y=1))+
  geom_hline(aes(yintercept=1), linetype='dotted', color='light grey')+
  geom_point(size=2.5, shape=124)+
  geom_vline(aes(xintercept=0), linetype='dashed')+
  ylim(c(0.95,1.05))+
  xlab(label = "")+
  theme_minimal()+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  scale_x_continuous(breaks = c(-40, -30, -20, -10, 0, 10, 20, 30, 40,
                                50,60,70,80),
                     labels = c("-40%", "-30%", "-20%", "-10%", "0",
                                "+10%", "+20%", "+30%", "+40%",
                                "+50%","+60%","+70%","+80%"))+
  coord_cartesian(xlim = c(-40,80))
dev.off()

png(file = "menus_payoffdiff.png" , width=7, height=1.5, units = "in", res = 400)
# svglite::svglite("design_premium_by_menu.svg", width=7, height=2)
ggplot(df1, aes(x=lpcsdiff, y=1))+
  geom_hline(aes(yintercept=1), linetype='dotted', color='light grey')+
  geom_point(size=2.5, shape=124)+
  geom_vline(aes(xintercept=0), linetype='dashed')+
  xlab(label = "")+
  theme_minimal()+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  scale_x_continuous(breaks = c(-10,-7.5,-5,-2.5,0,2.5,5,7.5,10),
                     labels = c("-10€","-7.5€","-5€","-2.5€","0","2.5€", "5€","7.5€", "10€"))+
  coord_cartesian(xlim = c(-10,10), ylim =c(0.95,1.05))
dev.off()
########### tables and summary statistics #######

# more optimal choices when CS? yes
oktable <- aggregate(ok~CS, data=df, FUN=function(x) c(n = length(x), mean =mean(x), st.dev = sd(x)))
oktable
okwt<-wilcox.test(df$ok[df$CS==0],df$ok[df$CS==2])
okwt$p.value
oktt<-t.test(df$ok[df$CS==0],df$ok[df$CS==2])
oktt$p.value

# faster when CS? yes
timetable <- aggregate(time~CS, data=df, FUN=function(x) c(n = length(x), mean =mean(x), st.dev = sd(x)))
timewt<-wilcox.test(df$time[df$CS==0],df$time[df$CS==2])
timewt$p.value
timett<-t.test(df$time[df$CS==0],df$time[df$CS==2])
timett$p.value

#in general, how frequently CS chosen? how frequently IS chosen? how frequently HPCS chosen?
df1$cschoice <- NA
df1$cschoice[df1$LPCSchosen==1]<-"Target"
df1$cschoice[df1$HPCSchosen==1]<-"Decoy"
df1$cschoice[df1$ISchosen==1]<-"Competitor"

#changing order of factor levels
df1$cschoice <- as.factor(df1$cschoice)
df1$cschoice <- factor(df1$cschoice,levels(df1$cschoice)[c(3,2,1)])
#table
prop.table(table(df1$cschoice))

#only the -29cent task
prop.table(table(df1$cschoice[df1$lpcsdiff==-0.2999992]))

#only the +20cent task
prop.table(table(df1$cschoice[df1$lpcsdiff==0.2000036]))

df1$lpd <- as.factor(round(df1$profitpremium,1))


########### plot2: raw data, choice shares as a function of profit premium #######
png("choice_shares_by_profitdiff.png", width=16, height=16, units="in", res=300)
ggplot(df1, aes(x=lpd,fill=cschoice))+geom_bar(position='fill')+
  geom_hline(aes(yintercept=0.5), linetype='dotted')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_fill_grey()+
  xlab("profit difference target - competitor")
dev.off()


############# mixed logit models #################

##saving data for stata mixlogit
write.csv(df1, "df_to_Stata.csv")

### let's try to control for the actual shapes
df1 <- df1 %>% select(-menulength, -menulength2, -session, 
                      -choice4, -choice5, -choice6,
                      -fac, -year, -ma1, -ma2, -ma3, 
                      -shtask1, -shtask2, -shtask3, 
                      -shtask4, -pr1, -pr2, -pr3, -pr4,
                      -demand, -motivation, -sh4, -sh5, -sh6,
                      -up4, -up5, -up6, -ar4, -ar5, -ar6,
                      -is4, -is5, -is6, - X_merge)

df1$targetshape[df1$target == 1] <- df1$sh1[df1$target == 1]
df1$targetshape[df1$target == 2] <- df1$sh2[df1$target == 2]
df1$targetshape[df1$target == 3] <- df1$sh3[df1$target == 3]
df1$targetshape <- factor(df1$targetshape)
levels(df1$targetshape) <- c("cir","sq","tri")

df1$competitorshape[df1$competitor == 1] <- df1$sh1[df1$competitor == 1]
df1$competitorshape[df1$competitor == 2] <- df1$sh2[df1$competitor == 2]
df1$competitorshape[df1$competitor == 3] <- df1$sh3[df1$competitor == 3]
df1$competitorshape <- factor(df1$competitorshape)
levels(df1$competitorshape) <- c("cir","sq","tri")

#add dummies to df
df1 <- df1 %>% mutate(targetcir = targetshape == "cir",
                      targetsq = targetshape == "sq",
                      targettri = targetshape == "tri",
                      compcir = competitorshape == "cir",
                      compsq = competitorshape == "sq",
                      comptri = competitorshape == "tri")


#####################################
##### 1. lpcs vs. all other choices ######
#####################################


# modX <- glmer(LPCSchosen ~  profitpremium + (1+profitpremium| subject) + targetsq + targettri + targetcir + compsq + comptri + compcir, data = df1, 
#               family = binomial, control = glmerControl(optimizer = "bobyqa"))
# 
# mod1 <- modX

mod1 <- glmer(LPCSchosen ~  profitpremium + (1+profitpremium| subject), data = df1,
           family = binomial, control = glmerControl(optimizer = "bobyqa"))
modA <- mod1
# predict psychometric function
df1$predictions <- predict(mod1, newdata = df1, type = "response", re.form = NULL, allow.new.levels = TRUE)

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
ade <- df1 %>% select(id, predictions, profitpremium) %>%
  filter(profitpremium>-3 & profitpremium<3) %>%
  group_by(id)%>%
  spread(profitpremium, predictions)
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
