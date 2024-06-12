# posterior predictive plots

# clear work space
rm(list=ls())

# working directory
setwd("/Users/gerry/Dropbox/Dropbox/_working/_ACTIVE/2019_Panama/2019 first contacts in vampire cage/data/results")

# packages
library(tidyverse) # data tidying
library(brms) # bayesian models
library(bayesplot) # bayesian plots
library(patchwork) # combining plots

# put all models in list
models <- list()

# compile models on effect of unfamiliarity 
load('4hour_model1.Rdata')
models[[1]] <- fit1
rm(fit1)
  
load('8hour_model1.Rdata')
models[[2]] <- fit1
rm(fit1)

load('12hour_model1.Rdata')
models[[3]] <- fit1
rm(fit1)

load('16hour_model1.Rdata')
models[[4]] <- fit1
rm(fit1)

load('20hour_model1.Rdata')
models[[5]] <- fit1
rm(fit1)

load('24hour_model1.Rdata')
models[[6]] <- fit1
rm(fit1)

# compile models of first contacts predicting allogrooming
load('4hour_model2.Rdata')
models[[7]] <- fit2
rm(fit2)

load('8hour_model2.Rdata')
models[[8]] <- fit2
rm(fit2)

load('12hour_model2.Rdata')
models[[9]] <- fit2
rm(fit2)

load('16hour_model2.Rdata')
models[[10]] <- fit2
rm(fit2)

load('20hour_model2.Rdata')
models[[11]] <- fit2
rm(fit2)

load('24hour_model2.Rdata')
models[[12]] <- fit2
rm(fit2)

load('4hour_model3.Rdata')
models[[13]] <- fit3
rm(fit3)

load('8hour_model3.Rdata')
models[[14]] <- fit3
rm(fit3)

load('12hour_model3.Rdata')
models[[15]] <- fit3
rm(fit3)

load('16hour_model3.Rdata')
models[[16]] <- fit3
rm(fit3)

load('20hour_model3.Rdata')
models[[17]] <- fit3
rm(fit3)

load('24hour_model3.Rdata')
models[[18]] <- fit3
rm(fit3)

# make list of titles
titles <- c(
  "effect of unfamiliarity on first 4 hours of contact",
  "effect of unfamiliarity on first 8 hours of contact",
  "effect of unfamiliarity on first 12 hours of contact",
  "effect of unfamiliarity on first 16 hours of contact",
  "effect of unfamiliarity on first 20 hours of contact",
  "effect of unfamiliarity on first 24 hours of contact",
  "first 4 hours of contact as predictor of long-term allogrooming",
  "first 8 hours of contact as predictor of long-term allogrooming",
  "first 12 hours of contact as predictor of long-term allogrooming",
  "first 16 hours of contact as predictor of long-term allogrooming",
  "first 20 hours of contact as predictor of long-term allogrooming",
  "first 24 hours of contact as predictor of long-term allogrooming",
  "first 4 hours of contact as predictor of pre-treatment allogrooming",
  "first 8 hours of contact as predictor of pre-treatment allogrooming",
  "first 12 hours of contact as predictor of pre-treatment allogrooming",
  "first 16 hours of contact as predictor of pre-treatment allogrooming",
  "first 20 hours of contact as predictor of pre-treatment allogrooming",
  "first 24 hours of contact as predictor of pre-treatment allogrooming")

# set x-axis
t <- c(rep(500,6), rep(100,12))

# make list of posterior predictive check
ppc.list <- list()

# fill list with plots
for (i in 1:length(models)) {
  ppc.list[[i]] <- 
     pp_check(models[[i]], ndraws=200)+
     coord_cartesian(xlim= c(0,t[i]))+
    xlab("observed (black) and expected (blue) response (y) data")+
     theme(legend.position= 'none')+
     ggtitle(paste(titles[i]))
}

# combined plot
cp <- ppc.list[[1]]+ppc.list[[2]]+ppc.list[[3]]+ppc.list[[4]]+ppc.list[[5]]+ppc.list[[6]]+ 
  ppc.list[[7]]+ppc.list[[8]]+ppc.list[[9]]+ppc.list[[10]]+ppc.list[[11]]+ppc.list[[12]] + 
  ppc.list[[13]]+ppc.list[[14]]+ppc.list[[15]]+ppc.list[[16]]+ppc.list[[17]]+ppc.list[[18]]+ 
  plot_layout(ncol=3, nrow=6)

# save as PDF (this can take awhile)
ggsave(
  filename= 'plots/ppc_plots.pdf',
  plot = cp,
  scale = 1,
  width = 20,
  height = 20,
  units = c("in", "cm", "mm", "px"),
  dpi = 300)
