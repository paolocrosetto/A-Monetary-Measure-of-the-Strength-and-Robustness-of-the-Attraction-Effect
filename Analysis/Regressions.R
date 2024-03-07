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
             coef_map = c("(Intercept)" = "Constant",
                             "profitpremium_perc" = "Profit premium",
                             "SD (Intercept subject)" = "Var(u_i)",
                             "SD (profitpremium_perc subject)" = "Var(v_i)"),
             output = "latex") %>% 
  kableExtra::save_kable("Tables/Table_1.pdf")


### diagnostics: are random effects normally distributed? 

## model 1
ranef(mod1)$subject$`(Intercept)` %>% 
  shapiro.test()

## model 2
ranef(mod2)$subject$`(Intercept)` %>% 
  shapiro.test()


## function to create plots

## the function generates a plot that contains
##   - in the center, the estimated psychometric functions for each subject
##   - on the margins, the plot of the effects (frequency + monetary ADE estimates)
## it takes as inputs three datasets
##   - {original_data} contains the original data
##   - {model_run} is the regression results ran above
##   - {file_name} is the name of the file you want to generate

estimation_plot_and_tests <- function(model_run, original_data, file_name, table_name) {
  
  ### generating the needed data ####
  
  ## 1: adding predictions to the dataset to generate the main plot of the estimated psychometric functions
  main_data <- original_data %>% 
    mutate(predictions = predict(model_run, 
                                 newdata = original_data, 
                                 type = "response", 
                                 re.form = NULL, allow.new.levels = TRUE))
  
  ## 2: data of the monetary ADE measure
  
  # individual estimates
  monetaryADE <- coef(model_run)[[1]] %>% 
    as_tibble() %>% 
    mutate(measure = -`(Intercept)`/profitpremium_perc, 
           titlestring = "Distribution of the monetary measure of ADE")
  
  # mean of the fixed effect
  monetaryADE_mean = -fixef(model_run)[[1]]/fixef(model_run)[[2]]
  
  # mean, sd and confint of the indivdual biases
  monetaryADE_stats <- monetaryADE %>% 
    summarise(mean = mean(measure), 
              sd = sd(measure), 
              se = sd/sqrt(n()))
  
  ## 3: histogram of the frequency ADE measure
  
  ##find and plot intercept
  frequencyADE <- main_data %>% 
    select(id, predictions, profitpremium_perc) %>%
    filter(profitpremium_perc>-3 & profitpremium_perc<3) %>%
    group_by(id)%>%
    spread(profitpremium_perc, predictions) %>% 
    dplyr::rename(neg = "-2.85713523809524", 
           pos = "2.04085243647374") %>% 
    mutate(measure = neg+((pos-neg)/0.5)*0.3,
           titlestring = "Distribution of the frequency measure of ADE")
  
  frequencyADE_stats = frequencyADE %>% ungroup() %>% 
    summarise(mean = mean(measure, na.rm = T), 
              sd = sd(measure, na.rm = T), 
              se = sd/sqrt(n()))
  
  ### TABLE and TESTING ###
  
  monetaryADE_table <- monetaryADE_stats %>% 
    mutate(mean = monetaryADE_mean,
           stat = "Monetary ADE", 
           test = t.test(monetaryADE$measure)$p.value) %>% 
    select(stat, mean, sd, test)
  
  frequencyADE_table <- frequencyADE_stats %>% 
    mutate(stat = "Frequency ADE", 
           test = t.test(frequencyADE$measure)$p.value) %>% 
    select(stat, mean, sd, test)
  
  
  
  # share of subjects affected 
  affected_frequency <- frequencyADE %>% 
    mutate(has_effect = measure > 0.5) %>% 
    group_by(has_effect) %>% 
    tally() %>% 
    mutate(share = 100*n/sum(n)) %>% 
    filter(has_effect== T) %>% 
    pull(share)
    
  frequencyADE_table <- frequencyADE_table %>% 
    mutate(share_affected = affected_frequency)
  
  # packaging and exporting
  filename <- paste0("Tables/effect_sizes_and_tests_",table_name, ".png")
  monetaryADE_table %>% 
    bind_rows(frequencyADE_table) %>% 
    tt(caption = paste0("Descriptive statiscts and tests, ", table_name)) %>% 
    save_tt(filename, overwrite = T)
  
  ### PLOTTING ###
  
  # main plot of predicted psychometric function by subject
  main <- ggplot(main_data, aes(x = profitpremium_perc, y = predictions)) +
    geom_smooth(level=0.9999, color="cornflowerblue")+
    theme_minimal()+
    geom_vline(xintercept=0, color='indianred')+geom_hline(aes(yintercept=0.5),color='indianred', linetype="dashed")+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=16))+
    xlab("% utility premium of target vs. competitor")+
    ylab("Predicted probability of choosing Target")+
    geom_line(aes(group=id), alpha=0.1)+
    coord_cartesian(xlim = c(-40,35), ylim = c(0.1,0.9))
  
  # empty plot for the unused corner
  empty <- ggplot()+geom_point(aes(1,1), colour="white") +
    theme_void()
  
  # marginal histogram #1: monetary ADE measure
  monetary <- ggplot(monetaryADE, aes(x = measure))+
    #adding histogram
    geom_histogram(color="grey80", fill="grey70", binwidth = 0.8) + 
    annotate("segment", 
             x = monetaryADE_mean, 
             xend = monetaryADE_mean, 
             y=0, 
             yend = 11.7, 
             color="cornflowerblue", size=1)+
    annotate(geom = "rect", 
             xmin = monetaryADE_mean - monetaryADE_stats$se, 
             xmax = monetaryADE_mean + monetaryADE_stats$se, 
             ymin=0, ymax=11.7, 
             fill="cornflowerblue", alpha=.1)+
    geom_vline(xintercept = 0, color='indianred')+
    theme_minimal()+ coord_cartesian(ylim=c(0,11.5),xlim = c(-40,35))+
    ylab("# of subjects")+xlab("")+theme(axis.text.x = element_blank())+
    scale_y_continuous(breaks=c(0,2.5,5,7.5,10,12.5))+
    facet_grid(.~titlestring)
  
  
  # marginal histogram #2: frequency ADE measure
  frequency <- ggplot(frequencyADE, aes(x = measure))+
    geom_histogram(color="grey80", fill="grey70", binwidth = 0.01) + 
    annotate("segment", x = frequencyADE_stats$mean, 
             xend = frequencyADE_stats$mean, 
             y=0, yend = 17, 
             color="cornflowerblue", size=1)+
    annotate(geom = "rect", 
             xmin = frequencyADE_stats$mean - frequencyADE_stats$se, 
             xmax = frequencyADE_stats$mean + frequencyADE_stats$se, 
             ymin=0, ymax=17, 
             fill='cornflowerblue', alpha=.1)+
    geom_vline(xintercept = 0.5, color='indianred', linetype="dashed")+
    #appearance
    coord_flip(xlim=c(0.1,0.9))+
    xlab("")+ylab("# of subjects")+
    theme_minimal()+theme(axis.text.y = element_blank())+
    facet_grid(titlestring~.)
  
  # putting them all together and saving to file
  png(paste0("Figures/",file_name,".png"), width = 9, height = 9, units = "in", res = 300)
  grid.arrange(monetary, empty, main, frequency, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  dev.off()
  
}


## running plot and tests for each model.
# model 1: all choices
estimation_plot_and_tests(mod1,df,"Figure_5","full_model")

# model 2: no decoy choices
estimation_plot_and_tests(mod2,df[df$HPCSchosen==0,],"Figure_6", "dropping_decoy_choices")
