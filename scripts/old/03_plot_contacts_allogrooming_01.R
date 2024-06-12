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

# compile models of first contacts predicting allogrooming

# put all models in list
models <- list()

load('4hour_model2.Rdata')
models[[1]] <- fit2
rm(fit2)

load('8hour_model2.Rdata')
models[[2]] <- fit2
rm(fit2)

load('12hour_model2.Rdata')
models[[3]] <- fit2
rm(fit2)

load('16hour_model2.Rdata')
models[[4]] <- fit2
rm(fit2)

load('20hour_model2.Rdata')
models[[5]] <- fit2
rm(fit2)

load('24hour_model2.Rdata')
models[[6]] <- fit2
rm(fit2)

# compile models of first contacts predicting pre-treatment allogrooming 
# put all models in list
models2 <- list()

load('4hour_model3.Rdata')
models2[[1]] <- fit3
rm(fit3)

load('8hour_model3.Rdata')
models2[[2]] <- fit3
rm(fit3)

load('12hour_model3.Rdata')
models2[[3]] <- fit3
rm(fit3)

load('16hour_model3.Rdata')
models2[[4]] <- fit3
rm(fit3)

load('20hour_model3.Rdata')
models2[[5]] <- fit3
rm(fit3)

load('24hour_model3.Rdata')
models2[[6]] <- fit3
rm(fit3)


# get list of hours.sampled
period <- c(4,8,12,16,20,24)

# make empty list to store model results
res <- list()
res2 <- list()

# fill list
for (i in 1:length(models)) {
  # get model results
  res[[i]] <- 
    summary(models[[i]])$fixed %>%
    rownames_to_column(var= 'term') %>% 
    mutate(period= period[i]) %>% 
    filter(term == "scalecontact" | term == "forced.proximityTRUE:phase3")
}

for (i in 1:length(models2)) {
  # get model results
  res2[[i]] <- 
    summary(models2[[i]])$fixed %>%
    rownames_to_column(var= 'term') %>% 
    mutate(period= period[i]) %>% 
    filter(term == "scalecontact" | term == "forced.proximityTRUE:phase3")
}

# compile results
results1 <- 
  bind_rows(res) %>% 
  mutate(allogrooming.sample= "all")
results2 <- 
  bind_rows(res2) %>% 
  mutate(allogrooming.sample= "pre-treatment")

results <- rbind(results1, results2)

# save table of results
results %>% 
  filter(term == 'scalecontact') %>% 
  mutate(Estimate = round(Estimate,2)) %>% 
  mutate(Est.Error = round(Est.Error,2)) %>% 
  mutate(`l-95% CI`= round(`l-95% CI`, 2)) %>% 
  mutate(`u-95% CI`= round(`u-95% CI`, 2)) %>% 
  mutate(Rhat= round(Rhat,4)) %>% 
  select(
    allogrooming.sample,
    `Hours sampled` = period,
         Estimate, 
         `lower 95%` = `l-95% CI`,
         `upper 95%` = `u-95% CI`,
         Rhat,
         `Bulk Effective Sample Size`= Bulk_ESS,
         `Tail Effective Sample Size`= Tail_ESS) %>% 
  mutate(perc.increase= ifelse(Estimate >= 0, 100*(exp(Estimate)- 1), 100*(1-exp(Estimate)))) %>% 
  write.csv("allogrooming_model_results.csv", row.names = F)

# get reference result (min and max)
reference <- 
  results %>% 
  filter(term== "forced.proximityTRUE:phase3") %>% 
  pull(Estimate) %>% 
  range()

# plot model results
(plot <- 
  results %>% 
  filter(term!= "forced.proximityTRUE:phase3") %>% 
  mutate(low= ifelse(term=="scalecontact", `l-95% CI`, NA)) %>% 
  mutate(high= ifelse(term=="scalecontact", `u-95% CI`, NA)) %>% 
  ggplot(aes(y=Estimate, x= period, color= allogrooming.sample, shape= allogrooming.sample))+
  xlab("sampled hours at first contact")+
  ylab("standardized coefficient for first contact")+
  geom_hline(yintercept= reference[1], linetype= 'dashed', color= 'darkgreen')+
  geom_hline(yintercept= reference[2], linetype= 'dashed', color= 'darkgreen')+
  geom_text(aes(x=4, y=(reference[2]+0.05), label="reference effect (one week of forced proximity)"), hjust= "left", color= "darkgreen")+
  geom_hline(yintercept= 0, color = "darkred")+
  #geom_text(aes(x=4, y=(-0.05), label="no effect"), hjust= "left", color= "darkred")+  
    geom_point(size=3, position=position_dodge(width=1))+
    geom_line(position=position_dodge(width=1))+
    geom_errorbar(aes(ymin=`l-95% CI`, ymax=`u-95% CI`, width=1), size=1, position=position_dodge(width=1))+  
  theme_bw()+
  theme(legend.position= "none", axis.text=element_text(size=11), strip.text = element_text(size=12))+
  scale_color_manual(values= c('darkblue', "lightblue"))+  
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
