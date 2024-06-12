# plot familiar vs unfamiliar contact times

# clear work space
rm(list=ls())

# working directory
setwd("/Users/gerry/Dropbox/Dropbox/_working/_ACTIVE/2019_Panama/2019 first contacts in vampire cage/data/results")

# packages
library(boot) # bootstrapping
library(tidyverse) # data tidying
library(igraph) # plotting networks
library(brms) # bayesian models
library(bayesplot) # bayesian plots
library(patchwork) # combining plots

# get mean and 95% CI of values x via bootstrapping
boot_ci <- function(x, perms=5000, bca=F) {
  get_mean <- function(x, d) {
    return(mean(x[d]))
  }
  x <- as.vector(na.omit(x))
  mean <- mean(x)
  if(bca){
    boot <- boot.ci(boot(data=x, 
                         statistic=get_mean, 
                         R=perms, 
                         parallel = "multicore", 
                         ncpus = 4), 
                    type="bca")
    low <- boot$bca[1,4]
    high <- boot$bca[1,5] 
  }else{
    boot <- boot.ci(boot(data=x, 
                         statistic=get_mean, 
                         R=perms, 
                         parallel = "multicore", 
                         ncpus = 4), 
                    type="perc")
    low <- boot$perc[1,4]
    high <- boot$perc[1,5] 
  }
  c(low=low,mean=mean,high=high, N=round(length(x)))
}

# get mean and 95% CI via bootstrapping of values y within grouping variable x
boot_ci2 <- function(d=d, y=d$y, x=d$x, perms=5000, bca=F){
  df <- data.frame(effect=unique(x))
  df$low <- NA
  df$mean <- NA
  df$high <- NA
  df$n.obs <- NA
  for (i in 1:nrow(df)) {
    ys <- y[which(x==df$effect[i])]
    if (length(ys)>1 & var(ys)>0 ){
      b <- boot_ci(y[which(x==df$effect[i])], perms=perms, bca=bca) 
      df$low[i] <- b[1]
      df$mean[i] <- b[2]
      df$high[i] <- b[3]
      df$n.obs[i] <- b[4]
    }else{
      df$low[i] <- min(ys)
      df$mean[i] <- mean(ys)
      df$high[i] <- max(ys)
      df$n.obs[i] <- length(ys)
    }
  }
  df
}



# compile descriptions of contact duration
(d1 <- 
  list(
  read.csv("4hour_contact_stats.csv"),
  read.csv("8hour_contact_stats.csv"),
  read.csv("12hour_contact_stats.csv"),
  read.csv("16hour_contact_stats.csv"),
  read.csv("20hour_contact_stats.csv"),
  read.csv("24hour_contact_stats.csv")) %>% 
  bind_rows() %>% 
  mutate(density= n.contacts/n.dyads))

# plot density over time
(p1 <- 
  d1 %>% 
  ggplot(aes(x=period, y=density))+
  geom_point(size=3)+
  geom_line()+
  ylab("proportion of new dyads with contact")+
  xlab("hours sampled")+
  scale_color_manual(values= c("darkblue", "lightblue"))+
  scale_x_continuous(breaks= c(4,8,12,16,20,24))+
  coord_cartesian(ylim= c(0,1))+  
  theme_classic()+
  theme(legend.position = c(0.2,0.9), legend.title = element_blank()))

# get new vs familiar contacts
(d2 <- 
    list(
      read.csv("4hour_contacts.csv"),
      read.csv("8hour_contacts.csv"),
      read.csv("12hour_contacts.csv"),
      read.csv("16hour_contacts.csv"),
      read.csv("20hour_contacts.csv"),
      read.csv("24hour_contacts.csv")) %>% 
    bind_rows() %>% 
    mutate(new= new.dyad) %>% 
    as_tibble())

# get means and 95% CI
means <- 
  d2 %>% 
  mutate(group= paste(new.dyad, period, sep="_")) %>% 
  boot_ci2(y=.$duration, x=.$group) %>% 
  separate(effect, into=c("new", "period"), sep= "_") %>% 
  mutate(new= paste(new,"pairs")) %>% 
  mutate(period= as.numeric(period))

# plot
(p2 <- 
  means %>% 
  ggplot(aes(x=period, y=mean, color=new, shape = new))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=low, ymax=high, width=1), linewidth=1)+
  ylab("minutes of proximity")+
  xlab("hours sampled")+
  scale_color_manual(values= c("darkblue", "lightblue"))+
  scale_x_continuous(breaks= c(4,8,12,16,20,24))+
  theme_classic()+
  theme(legend.position = c(0.2,0.9), legend.title = element_blank()))

# get new vs familiar model
(d3 <- 
    read.csv('model_1_summary.csv') %>% 
    filter(term == "unfamiliarity") %>% 
    mutate(Estimate = round(Estimate,2)) %>% 
    mutate(Est.Error = round(Est.Error,2)) %>% 
    mutate(l.95..CI= round(l.95..CI, 2)) %>% 
    mutate(u.95..CI= round(u.95..CI, 2)) %>% 
    mutate(Rhat= round(Rhat,4)) %>% 
    select(`No. Hours Sampled`= first_contact_period,
           Estimate, Est.Error,
           `lower 95%` = l.95..CI,
           `upper 95%` = u.95..CI,
           Rhat,
           `Bulk Effective Sample Size`= Bulk_ESS,
           `Tail Effective Sample Size`= Tail_ESS)) 

# save model summary
d3 %>% 
  write.csv("new_vs_familiar_contacts.csv", row.names = F)

# plot model
(p3 <- 
    d3 %>% 
    ggplot(aes(x=`No. Hours Sampled`, y=Estimate))+
    geom_point(size=3)+
    geom_line()+
    geom_errorbar(aes(ymin=`lower 95%`, ymax=`upper 95%`, width=1), linewidth=1)+
    ylab("effect of unfamiliarity on contact time")+
    xlab("hours sampled")+
    geom_hline(yintercept=0)+
    scale_color_manual(values= c("darkblue", "lightblue"))+
    scale_x_continuous(breaks= c(4,8,12,16,20,24))+
    theme_classic()+
    theme(legend.position = c(0.2,0.9), legend.title = element_blank()))

# combine plots
p4 <- p1/p2/p3 + plot_annotation(tag_levels = "A")

# save as PDF
ggsave(
  filename= 'plots/new_vs_familiar_contacts.pdf',
  plot = p4,
  scale = 1,
  width = 5,
  height = 10,
  units = c("in", "cm", "mm", "px"),
  dpi = 300)

