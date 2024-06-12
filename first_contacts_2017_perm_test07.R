# analyze 2017 first interactions as predictors of long-term social bonds in vampire bats
# Gerald Carter

# clear workspace
rm(list=ls())

# set working directory
# set directory
setwd("/Users/gerry/Dropbox/Dropbox/_working/_ACTIVE/2017_Panama/2017 first contacts")

# packages
library(tidyverse)
library(patchwork)
library(see)

# set number of permutations for permutation test
perms <- 5000

# import data----

# get first contact interaction rates
d <- 
  read.csv('first_contact_rates.csv', stringsAsFactors = F) %>% 
  mutate(dyad= ifelse(actor<receiver, paste(actor,receiver,sep="_"), paste(receiver,actor, sep="_")))

# get introduced bats
intros <- c('una', 'dos', 'tes', 'cat', 'ivy', 'six', 'eve', 'ola')

# Do first impressions matter?

# how many introductions with long-term data?
d %>% 
  filter(!is.na(grooming)) %>% 
  pull(dyad) %>% 
  n_distinct()
# how many bats
n_distinct(c(d$actor, d$receiver))

# what behaviors happen at first contact?
d %>% group_by(dyad) %>% summarize(aff= mean(aff)) %>% summarize(mean(aff>0)) # 86% undirected dyads affiliative
d %>% group_by(dyad) %>% summarize(agg= mean(agg)) %>% summarize(mean(agg>0)) # 8.6 % undirected dyads aggressive
mean(d$aff>0) # 74% directed dyads affiliative
mean(d$agg>0) # 4% directed dyads aggressive

# Do first contact interactions predict long-term cooperation?

# fit effects and get coefficients
obs1 <- glm(cbind(grooming,g.sampling-grooming) ~ scale(aff), family= 'binomial', data =d)$coefficients[2]
obs2 <- glm(cbind(grooming,g.sampling-grooming) ~ scale(agg), family= 'binomial', data =d)$coefficients[2]
obs3 <- glm(cbind(sharing,g.sampling-sharing) ~ scale(aff), family= 'binomial', data =d)$coefficients[2]
obs4 <- glm(cbind(sharing,g.sampling-sharing) ~ scale(agg), family= 'binomial', data =d)$coefficients[2]

# create place to store expected coefficients
exp1 <- rep(NA, perms)
exp2 <- rep(NA, perms)
exp3 <- rep(NA, perms)
exp4 <- rep(NA, perms)

# create resample function to replace the sample function so that it works with n=1
resample <- function(x, ...) x[sample.int(length(x), ...)]

# permutation test
set.seed(123)
for (i in 1:perms){
  # randomize data
  rand <- 
    d %>% 
    separate(dir.dyad, into= c('actor', 'receiver'), sep= "_") %>% 
    mutate(introduced.bat= if_else(actor %in% intros, actor, receiver)) %>% 
    mutate(direction= actor==introduced.bat) %>% 
    
    # permute possible interactions given and received by each introduced bat
    group_by(introduced.bat, direction) %>% 
    mutate(aff = aff[resample(row_number())],
           agg = agg[resample(row_number())]) %>%  
    ungroup()
  
  # get expected correlations
  exp1[i] <- glm(cbind(grooming,g.sampling-grooming) ~ scale(aff), family= 'binomial', data = rand)$coefficients[2]
  exp2[i] <- glm(cbind(grooming,g.sampling-grooming) ~ scale(agg), family= 'binomial', data = rand)$coefficients[2]
  exp3[i] <- glm(cbind(sharing,g.sampling-sharing) ~ scale(aff), family= 'binomial', data = rand)$coefficients[2]
  exp4[i] <- glm(cbind(sharing,g.sampling-sharing) ~ scale(agg), family= 'binomial', data = rand)$coefficients[2]
  
  # show progress
  print(paste(i,"of","perms"))
  
}

# delete NA values from expected
exp1 <- exp1[which(!is.na(exp1))]
exp2 <- exp2[which(!is.na(exp2))]
exp3 <- exp3[which(!is.na(exp3))]
exp4 <- exp4[which(!is.na(exp4))]

# get one-tailed p-values (affiliative should be higher, agggressive should be lower)
p1 <- mean(exp1>=obs1)
p2 <- mean(exp2<=obs2)
p3 <- mean(exp3>=obs3)
p4 <- mean(exp4<=obs4)

# get quantiles of expected values
results <- 
  tibble(predictor= 
           c('affiliative', 'aggressive', 'affiliative', 'aggressive'),
         relationship= 
           c('allogrooming', 'allogrooming', 'food sharing', 'food sharing'),
         observed= 
           c(obs1, obs2, obs3, obs4),
         expected= 
           c(mean(exp1), mean(exp2), mean(exp3), mean(exp4)),
         expected.low= 
                        c(quantile(exp1, probs= 0.025),
                         quantile(exp2, probs= 0.025),
                         quantile(exp3, probs= 0.025),
                         quantile(exp4, probs= 0.025)),
         expected.high= 
           c(quantile(exp1, probs= 0.975),
             quantile(exp2, probs= 0.975),
             quantile(exp3, probs= 0.975),
             quantile(exp4, probs= 0.975)),
         n.exp= c(length(exp1), length(exp2), length(exp3), length(exp4)),
         one.tailed.p= c(p1,p2,p3,p4)) %>% 
  mutate(predictor= paste(predictor,"first contacts")) 

# save results
results %>% 
  write.csv("perm_test_results.csv", row.names = F)
  

# get raw expected values for plotting distributions
results2 <- rbind(tibble(expected= exp1, relationship= 'allogrooming', predictor= "affiliative"),
                  tibble(expected= exp2, relationship= 'allogrooming', predictor= "aggressive"),
                  tibble(expected= exp3, relationship= 'food sharing', predictor= "affiliative"),
                  tibble(expected= exp4, relationship= 'food sharing', predictor= "aggressive")) %>% 
  mutate(predictor= paste(predictor,"first contacts")) 
  

# plot results
(plot <- 
  results %>% 
  ggplot(aes(x=relationship, y=observed))+
  facet_wrap(~predictor, scales= "free", ncol=1)+
  geom_violinhalf(data= results2, aes(x=relationship, y=expected), scale= 'width', width=1, color="darkgrey", fill= "lightgrey")+
  geom_errorbar(aes(ymin=expected.low, ymax=expected.high, width=.2), color= 'black')+
  geom_point(size=2)+
  ylab("standardized coefficient")+
  xlab("long-term relationship")+
  theme_bw()+
  coord_flip()+
    theme(axis.text.y=element_text(size=10), 
          axis.text.x=element_text(size=10), 
          axis.title.y=element_text(size=10), 
          axis.title.x=element_text(size=10),
          strip.text = element_text(size=10)))

# save plot
ggsave(
  "perm_test_plot.pdf",
  plot = plot,
  scale = 1,
  width = 4,
  height = 3,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)

plot
