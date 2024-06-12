# plot models predicting allogrooming using first contacts

# clear work space
rm(list=ls())

# working directory
setwd("/Users/gerry/Dropbox/Dropbox/_working/_ACTIVE/2019_Panama/2019 first contacts in vampire cage/data/results")

# packages
library(tidyverse) # data tidying
library(brms) # bayesian models
library(bayesplot) # bayesian plots
library(patchwork) # combining plots

# get models of first contacts predicting allogrooming
d1 <- 
  read.csv('model_2_contact_coefficients.csv') %>% 
  mutate(allogrooming.sample= "all")
d2 <- 
  read.csv('model_3_contact_coefficients.csv') %>% 
  mutate(allogrooming.sample= "pre-treatment")

# get models of first contacts predicting pre-treatment allogrooming 

# compile results
results <- rbind(d1, d2)

# save table of results
results %>% 
  filter(term == 'scalecontact') %>% 
  mutate(Estimate = round(Estimate,2)) %>% 
  mutate(Est.Error = round(Est.Error,2)) %>% 
  mutate(l.95..CI= round(l.95..CI, 2)) %>% 
  mutate(u.95..CI= round(u.95..CI, 2)) %>% 
  mutate(Rhat= round(Rhat,4)) %>% 
  rename(
    `Hours sampled` = first_contact_period,
         `lower 95%` = l.95..CI,
         `upper 95%` = u.95..CI,
         `Bulk Effective Sample Size`= Bulk_ESS,
         `Tail Effective Sample Size`= Tail_ESS) %>% 
  mutate(perc.increase= ifelse(Estimate >= 0, 100*(exp(Estimate)- 1), 100*(1-exp(Estimate)))) %>% 
  write.csv("allogrooming_model_results.csv", row.names = F)

# get reference result (min and max)
reference <- 
  read.csv('model_2_reference_coefficients.csv') %>% 
  pull(Estimate) %>% 
  range()

# plot model results
(plot <- 
  results %>% 
  ggplot(aes(y=Estimate, x= first_contact_period, color= allogrooming.sample, shape= allogrooming.sample))+
  xlab("sampled hours at first contact")+
  ylab("standardized coefficient for first contact")+
  geom_hline(yintercept= reference[1], linetype= 'dashed', color= 'darkred')+
  geom_hline(yintercept= reference[2], linetype= 'dashed', color= 'darkred')+
  geom_text(aes(x=4, y=(reference[2]+0.05), label="reference effect (one week of forced proximity)"), hjust= "left", color= "darkred")+
  geom_hline(yintercept= 0, color = "darkred")+
    geom_point(size=3, position=position_dodge(width=1.5))+
    #geom_line(position=position_dodge(width=1))+
    geom_errorbar(aes(ymin=l.95..CI, ymax= u.95..CI, width=1), size=1, position=position_dodge(width=1.5))+  
  theme_bw()+
  theme(legend.position= "none", axis.text=element_text(size=11), strip.text = element_text(size=12))+
  scale_color_manual(values= c('darkblue', "darkgreen"))+  
  scale_x_continuous(breaks= c(4,8,12,16,20,24)))

# save plot
ggsave(
  filename= 'plots/first-contact_allogrooming.pdf',
  plot = plot,
  scale = 1,
  width = 4.5,
  height = 4.5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300)

# plot proportion of variance explained
(plot2 <- 
    results %>% 
    select(first_contact_period, allogrooming.sample, r_squared_null, r_squared_diff) %>% 
    mutate(allogrooming.sample= ifelse(allogrooming.sample== "all", 
                                       "allogrooming over all 15 weeks", 
                                       "allogrooming over first 6 weeks")) %>% 
    pivot_longer(r_squared_null: r_squared_diff) %>% 
    mutate(name= ifelse(name== "r_squared_diff", "first contact time", "other predictors")) %>% 
    ggplot(aes(x= first_contact_period, y= value, group= name, fill= name))+
    facet_wrap(~allogrooming.sample, nrow=1)+
    xlab("sampled hours at first contact")+
    ylab("proportion of variance explained")+
    geom_col(color= 'black')+
    coord_cartesian(ylim= c(0,1))+
    theme_bw()+
    theme(legend.position= c(0.15, 0.8), 
          legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"),
          legend.box.background = element_rect(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text=element_text(size=11), 
          strip.text = element_text(size=11))+
    scale_fill_grey(start=0.1, end=0.8)+
    scale_x_continuous(breaks= c(4,8,12,16,20,24)))

# save plot
ggsave(
  filename= 'plots/r-squared.pdf',
  plot = plot2,
  scale = 1,
  width = 6,
  height = 3.5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300)
