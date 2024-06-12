# do first contacts from proximity tags predict long-term social grooming rates in vampire bats?
# fit models

# clear work space
rm(list=ls())

# get start time
start <- Sys.time()

# load packages
library(tidyverse) # data tidying
library(igraph) # plotting networks
library(brms) # bayesian models

# get social data---------
# get first contacts data from proximity loggers
setwd("/Users/maydixon/GitHub/First_Contacts")
meetings <- read.csv("20210313_21_bats_revised_meeting_data.csv", sep=",", header=T)

# load grooming data
load('rates2019_2021-05-18.RData')

# get date of introduction (June 14, 2019) 
# get first day of allogrooming data (June 23, 2019)
rates2019 %>% pull(date) %>% min() 

# change working directory to save results
setwd("/Users/maydixon/GitHub/First_Contacts/may_results")

# functions-------------
# convert a list of dyadic interactions to a network ------
a_b_edgelist_to_matrix <- function(el=el, symbol="_", directed= T, make.NA.zero=T){
  a <- str_split(as.data.frame(el)[,1],symbol, simplify = TRUE)[,1]
  r <- str_split(as.data.frame(el)[,1],symbol, simplify = TRUE)[,2]
  y <- as.data.frame(el)[,2]
  e <- data.frame(a,r,y, stringsAsFactors = F)
  require(igraph)
  if (make.NA.zero){
    g <- graph_from_data_frame(e, directed=directed)
    m <- get.adjacency(g, attr='y', sparse=FALSE)
    m
  }else{
    e$y <- e$y+1 # temporarily add one to distinguish between 0 and NA
    g <- graph_from_data_frame(e, directed=directed)
    m <- get.adjacency(g, attr='y', sparse=FALSE)
    m[m==0] <- NA # relabel missing values as NA
    m <- m-1 # subtract one to adjust values back
    m
  }
}


# clean data------------
d <- 
  meetings %>% 
  # text from email from Simon March 13, 2021
  # meetingStartPanama: this is when the meetings started in Panama.
  # We released the bats 2:15 AM (use this to filter for startOfMeeting  Panama time)
  filter(MeetingStartPanama > "2019-06-14 02:15:00") %>% 
  # convert bat ID codes from June 13, 2019 to final IDs in rates2019
  # first letter is group (a = Lake Bayano, b = Tole, c = La Chorrera)
  # other letters indicate type and location of bat bands
  mutate(bat1= case_when(
    SenderID == 11 ~ "box_L",
    SenderID == 12 ~ "box_D",
    SenderID == 13 ~ "box_R",
    SenderID == 30 ~ "box_L-2",
    SenderID == 31 ~ "addldd", # irma
    SenderID == 33 ~ "adld", # kelly
    SenderID == 34 ~ "add", # lillith
    SenderID == 35 ~ "adldd", # tiffany
    SenderID == 36 ~ "addld", # queen
    SenderID == 38 ~ "aldd", # shania
    SenderID == 39 ~ "ax", # jupiter, originally 'ad', later 'ax'
    SenderID == 40 ~ "bes", # esther, originally bse, later bes
    SenderID == 41 ~ "bey", # betty, originally bde, later bey
    SenderID == 42 ~ "bxdlx", # fran, originally bdlee, later bxdlx
    SenderID == 43 ~ "bdlx", # cindy, original bdle, later bdlx
    SenderID == 45 ~ "blx", # gayle, originally be, later blx
    SenderID == 46 ~ "bs", # hera
    SenderID == 47 ~ "bxx", # mabel, originally blee, later bxx
    SenderID == 50 ~ "box_R-2",
    SenderID == 51 ~ "clw", # rina, originally cf, later clw
    SenderID == 52 ~ "cldw", # winona, originally cff, later cldw
    SenderID == 53 ~ "cdw", # vixen, originally cddd, later cdw
    SenderID == 54 ~ "cnone", # piper, originally clf, later cnone
    SenderID == 57 ~ "cww", # opal, originally clddd, later cww
    SenderID == 58 ~ "cfd", # zelda, originally clff, later cfd
    SenderID == 59 ~ "cwd", # yzma, originally cflf, later cwd
    TRUE ~ "label_missing")) %>% 
  mutate(bat2= case_when(
    EncounteredID == 11 ~ "box_L",
    EncounteredID == 12 ~ "box_D",
    EncounteredID == 13 ~ "box_R",
    EncounteredID == 30 ~ "box_L-2",
    EncounteredID == 31 ~ "addldd", # irma
    EncounteredID == 33 ~ "adld", # kelly
    EncounteredID == 34 ~ "add", # lillith
    EncounteredID == 35 ~ "adldd", # tiffany
    EncounteredID == 36 ~ "addld", # queen
    EncounteredID == 38 ~ "aldd", # shania
    EncounteredID == 39 ~ "ax", # jupiter, originally 'ad', later 'ax'
    EncounteredID == 40 ~ "bes", # esther, originally bse, later bes
    EncounteredID == 41 ~ "bey", # betty, originally bde, later bey
    EncounteredID == 42 ~ "bxdlx", # fran, originally bdlee, later bxdlx
    EncounteredID == 43 ~ "bdlx", # cindy, original bdle, later bdlx
    EncounteredID == 45 ~ "blx", # gayle, originally be, later blx
    EncounteredID == 46 ~ "bs", # hera
    EncounteredID == 47 ~ "bxx", # mabel, originally blee, later bxx
    EncounteredID == 50 ~ "box_R-2",
    EncounteredID == 51 ~ "clw", # rina, originally cf, later clw
    EncounteredID == 52 ~ "cldw", # winona, originally cff, later cldw
    EncounteredID == 53 ~ "cdw", # vixen, originally cddd, later cdw
    EncounteredID == 54 ~ "cnone", # piper, originally clf, later cnone
    EncounteredID == 57 ~ "cww", # opal, originally clddd, later cww
    EncounteredID == 58 ~ "cfd", # zelda, originally clff, later cfd
    EncounteredID == 59 ~ "cwd", # yzma, originally cflf, later cwd
    TRUE ~ "label_missing")) %>% 
  rename(duration= MeetingDuration, 
         rssi= RSSI, 
         bs_rssi= BS_RSSI,
         time= MeetingStartPanama) %>% 
  # email from Simon March 13, 2021
  # Tag 40 restarted itself in the cage at least once and only ran for 6h. 
  # "My suggestion would be not using meetings at all where ID 40 was the sender. I would only use meetings where ID 40 was encountered. Those should be less biased." 
  filter(SenderID!=40 & EncounteredID !=40) %>% 
  # label meeting type
  mutate(meeting.type= case_when(
    substring(bat1, 1,3)!= "box" & substring(bat2, 1,3)!= "box" ~ "bat-bat",
    substring(bat1, 1,3)== "box" & substring(bat2, 1,3)== "box" ~ "box-box",
    substring(bat1, 1,3)!= "box" & substring(bat2, 1,3)== "box" ~ "bat-box",
    substring(bat1, 1,3)== "box" & substring(bat2, 1,3)!= "box" ~ "bat-box")) %>% 
  select(meeting.type, bat1,bat2, time, rssi, bs_rssi, duration) %>% 
  filter(meeting.type!= "box-box") %>% 
  as_tibble()

# get all bat-bat contacts
d2 <- 
  d %>% 
  filter(meeting.type== 'bat-bat') %>% 
  mutate(new.dyad = ifelse(substr(bat1,1,1) != substr(bat2,1,1), "unfamiliar", "familiar"))

# choose RSSI threshold
rssi.threshold <- as.numeric(quantile(d2$rssi, probs= c(0.95)))
d2 %>% 
  ggplot(aes(x=rssi, color=g))+
  geom_histogram(fill="light blue", color="black")+
  geom_vline(xintercept= rssi.threshold, color= 'red')

# get empty list for model results
model1.list <- list()
model2.list <- list()
model3.list <- list()

# loop through first contact periods
fcp <- c(4,8,12,16,20,24)

# run this code for each first contact period
for (i in 1:length(fcp)) {
  
# set first contact period
# run this script using 4, 8, 12, 16, 20, and 24 hours
first.contact.period <-  fcp[i]

# get first contact data (defined by first contact period)
if(first.contact.period== 24){
  t <- 
    d2 %>% 
    # get contacts for meetings that started in the first 24 hours
    filter(time < "2019-06-15 02:15:00")
}

if(first.contact.period== 20){
  t <- 
    d2 %>% 
    # get contacts for meetings that started in the first 24 hours
    filter(time < "2019-06-14 22:15:00")
}

if(first.contact.period== 16){
  t <- 
    d2 %>% 
    # get contacts for meetings that started in the first 24 hours
    filter(time < "2019-06-14 18:15:00")
}

if(first.contact.period== 12){
  t <- 
    d2 %>% 
    # get contacts for meetings that started in the first 12 hours
    filter(time < "2019-06-14 14:15:00")
}
if(first.contact.period== 8){
  t <- 
    d2 %>% 
    # get contacts for meetings that started in the first 8 hours
    filter(time < "2019-06-14 10:15:00")
}

if(first.contact.period== 4){
  t <- 
    d2 %>% 
    # get contacts for meetings that started in the first 6 hours
    filter(time < "2019-06-14 06:15:00")
}


# get proximity network during first contact period---------
d3 <- 
  t %>% 
  # get only close contacts
  filter(rssi > rssi.threshold) %>% 
  # label undirected dyads
  mutate(dyad= ifelse(bat1<bat2, paste(bat1,bat2, sep="_"), paste(bat2,bat1, sep="_"))) %>% 
  group_by(new.dyad, dyad) %>% 
  # sum duration (minutes) for the first contact period
  summarize(duration = sum(duration)/60) %>% 
  ungroup() %>% 
  mutate(obs.time=60*first.contact.period) %>% 
  # get association rate
  mutate(assoc= duration/obs.time) %>% 
  # label actor and receiver
  separate(dyad, into=c("bat1", "bat2"), remove= F) %>% 
  # round duration to nearest minute
  mutate(duration = round(duration))

# plot proximity network-----  
net <- 
  d3 %>% 
  select(dyad, assoc) %>% 
  a_b_edgelist_to_matrix(directed=F) %>% 
  graph_from_adjacency_matrix(mode="undirected", weighted=T)
  
ids <- 
  data.frame(id=unique(c(d2$bat1, d2$bat2))) %>% 
  mutate(group= substr(id, 1,1))
  
V(net)$group <- factor(ids[match(V(net)$name, ids$id), "group"])
colors <- c('#1b9e77','#fdae61','#7570b3')
V(net)$color=colors[as.numeric(V(net)$group)]

# save plot as pdf
set.seed(123)
plot.name <- paste0(first.contact.period, "hour_network.pdf")
pdf(plot.name) 
plot(net, 
     edge.width=E(net)$weight*5,
     layout=layout_with_fr(net),
     vertex.label.family="Helvetica",                   
     vertex.label.cex=c(0.6))
dev.off() 

# save association times by dyad------------
d3 %>% 
  mutate(period= first.contact.period) %>% 
  write.csv(file= paste0(first.contact.period,"hour_contacts.csv"))

# choose number of chains and chain length
nchains <- 6
chain_length <- 6000
warmup_length <- 3000

# model effect of familiarity on contact time----
fit1 <-
  brm(duration ~ 
        new.dyad + 
        (1|mm(bat1,bat2)),
      data = d3, 
      family = "negbinomial",
      cores = nchains,
      chains = nchains,
      iter = chain_length,
      warmup = warmup_length)

(ppc1 <- 
    pp_check(fit1, ndraws=200)+
    coord_cartesian(xlim= c(0,1000))+
    theme(legend.position= 'none')+
    ggtitle("posterior predictive check:", 
            subtitle= paste(first.contact.period, "sampled hours of initial contact predicted by familiarity")))

# check model
summary(fit1)

# save model
save(fit1, file = paste0(first.contact.period,"hour_model1.Rdata"))

# save model results
model1.list[[i]] <- 
  summary(fit1)$fixed %>%
  mutate(first_contact_period= first.contact.period)

# main model---------
# does first-day close contact network between new bats predict long-term social bonds?
# get mean grooming rates per directed dyad
# get bats present on first day
bats <- unique(c(d2$bat1, d2$bat2))

# get grooming seconds and observation time
g <- 
  rates2019 %>% 
  filter(cage== "big_cage") %>% 
  filter(phase== '3'| phase== '1') %>% 
  filter(behav=='g') %>% 
  filter(actor %in% bats) %>% 
  filter(receiver %in% bats) %>% 
  # get total seconds of grooming and hours of sampling (observation)
  group_by(actor, receiver, phase) %>% 
  summarize(count = sum(rate, na.rm=T), sampling=n()) %>% 
  ungroup() %>% 
  mutate(dyad= ifelse(actor<receiver, paste(actor,receiver, sep="_"), paste(receiver,actor, sep="_"))) %>% 
  mutate(new.dyad = ifelse(substr(actor,1,1) != substr(receiver,1,1), "unfamiliar", "familiar")) %>% 
  # convert grooming seconds to minutes
  mutate(count= (count/60)) %>% 
  mutate(rate= count/sampling)

# add first contact duration to grooming
g$contact <- d3$duration[match(g$dyad,d3$dyad)]

# convert missing first contacts to zero
g$contact[which(is.na(g$contact))] <- 0

# get forced proximity (cages)
small_cages <- 
  rates2019 %>% 
  filter(phase == 2) %>% 
  group_by(actor) %>% 
  summarize(cage= first(cage))

# get bats with population and forced proximity cages
bats2 <- 
  rates2019 %>% 
  group_by(actor) %>% 
  summarize(population= first(actor.pop)) %>% 
  filter(population != "captive-born") %>% 
  mutate(cage = small_cages$cage[match(.$actor, small_cages$actor)]) %>% 
  arrange(cage, actor) %>% 
  ungroup() %>% 
  mutate(pop.cage = paste(population, cage, sep = ".")) 

# get data for model (new dyads only)
d4 <- 
  g %>% 
  filter(new.dyad== "unfamiliar") %>% 
  # label dyads from forced proximity phase (phase2)
  mutate(a.cage= bats2$cage[match(.$actor, bats2$actor)],
         r.cage= bats2$cage[match(.$receiver, bats2$actor)]) %>% 
  mutate(forced.proximity= a.cage == r.cage) %>% 
  # get undirected dyads
  mutate(dyad= ifelse(actor<receiver, paste(actor,receiver, sep="_"), paste(receiver,actor, sep="_"))) %>% 
  arrange(dyad) %>% 
  group_by(dyad, phase, forced.proximity) %>% 
  summarize(count= round(mean(count, na.rm=T)), 
            sampling= round(mean(sampling, na.rm=T)),
            contact = round(mean(contact, na.rm=T))) %>% 
  ungroup() %>% 
  separate(dyad, into= c('bat1', 'bat2'), remove = F)
  
# how many new bonds
n_distinct(d4$dyad)

# get first contact durations by dyad
contacts <- 
  d4 %>% 
  group_by(dyad) %>% 
  summarize(contact=sum(contact)) %>% 
  pull(contact) 

# save descriptive stats for contacts---------
tibble(
  n.dyads= length(contacts),
  n.contacts= sum(contacts>0),
  mean.duration= mean(contacts[which(contacts>0)]),
  median.duration= median(contacts[which(contacts>0)]),
  min.duration= min(contacts[which(contacts>0)]),
  q25= quantile(contacts[which(contacts>0)])[2],
  q75= quantile(contacts[which(contacts>0)])[4],
  max.duration= max(contacts),
  sd.duration= min(contacts[which(contacts>0)]),
  period= first.contact.period) %>% 
  write.csv(file= paste0(first.contact.period,"hour_contact_stats.csv"))

# fit overdispersed count model for predicting grooming including other effects (Bayesian)
fit2 <-
  brm(count ~ 
        scale(contact)+
        forced.proximity*phase + 
        offset(log(sampling)) + 
        (1|mm(bat1,bat2)),
      data = d4, 
      family = negbinomial(),
      cores = nchains,
      chains = nchains,
      iter = chain_length,
      warmup = warmup_length)

# save model
save(fit2, file = paste0(first.contact.period,"hour_model2.Rdata"))

# get Bayes R-squared with full model
rsq <- 
  bayes_R2(
  fit2,
  resp = NULL,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975))

# get Bayes R-squared without first contact
nullfit <-
  brm(count ~ 
        forced.proximity*phase + 
        offset(log(sampling)) + 
        (1|mm(bat1,bat2)),
      data = d4, 
      family = negbinomial(),
      cores = nchains,
      chains = nchains,
      iter = chain_length,
      warmup = warmup_length)

rsq.null <- 
  bayes_R2(
    nullfit,
    resp = NULL,
    summary = TRUE,
    robust = FALSE,
    probs = c(0.025, 0.975))

# save model results
model2.list[[i]] <- 
  summary(fit2)$fixed %>%
  mutate(r_squared = rsq[1], r_squared_null= rsq.null[1]) %>% 
  mutate(r_squared_diff= r_squared - r_squared_null) %>% 
  mutate(first_contact_period= first.contact.period) 


# fit overdispersed count model for predicting grooming before forced proximity
# get data for model (new dyads only)
# get grooming seconds and observation time before treatment
rm(g)
g2 <- 
  rates2019 %>% 
  filter(cage== "big_cage") %>% 
  filter(phase== '1') %>% 
  filter(behav=='g') %>% 
  filter(actor %in% bats) %>% 
  filter(receiver %in% bats) %>% 
  # get total seconds of grooming and hours of sampling (observation)
  group_by(actor, receiver, phase) %>% 
  summarize(count = sum(rate, na.rm=T), sampling=n()) %>% 
  ungroup() %>% 
  mutate(dyad= ifelse(actor<receiver, paste(actor,receiver, sep="_"), paste(receiver,actor, sep="_"))) %>% 
  mutate(new.dyad = ifelse(substr(actor,1,1) != substr(receiver,1,1), "unfamiliar", "familiar")) %>% 
  # convert grooming seconds to minutes
  mutate(count= (count/60)) %>% 
  mutate(rate= count/sampling)

# add first contact duration to grooming
g2$contact <- d3$duration[match(g2$dyad,d3$dyad)]

# convert missing first contacts to zero
g2$contact[which(is.na(g2$contact))] <- 0

# get data for model (new dyads only)
d5 <- 
  g2 %>% 
  filter(new.dyad== "unfamiliar") %>% 
  # get undirected dyads
  mutate(dyad= ifelse(actor<receiver, paste(actor,receiver, sep="_"), paste(receiver,actor, sep="_"))) %>% 
  arrange(dyad) %>% 
  group_by(dyad) %>% 
  summarize(count= round(mean(count, na.rm=T)), 
            sampling= round(mean(sampling, na.rm=T)),
            contact = round(mean(contact, na.rm=T))) %>% 
  ungroup() %>% 
  separate(dyad, into= c('bat1', 'bat2'), remove = F)

# fit model
fit3 <-
  brm(count ~ 
        scale(contact)+
        offset(log(sampling)) + 
        (1|mm(bat1,bat2)),
      data = d5, 
      family = negbinomial(),
      cores = nchains,
      chains = nchains,
      iter = chain_length,
      warmup = warmup_length)

# save model
save(fit3, file = paste0(first.contact.period,"hour_model3.Rdata"))

# get Bayes R-squared with full model
rsq <- 
  bayes_R2(
    fit3,
    resp = NULL,
    summary = TRUE,
    robust = FALSE,
    probs = c(0.025, 0.975))

# get Bayes R-squared without first contact
nullfit <-
  brm(count ~ 
        offset(log(sampling)) + 
        (1|mm(bat1,bat2)),
      data = d5, 
      family = negbinomial(),
      cores = nchains,
      chains = nchains,
      iter = chain_length,
      warmup = warmup_length)

rsq.null <- 
  bayes_R2(
    nullfit,
    resp = NULL,
    summary = TRUE,
    robust = FALSE,
    probs = c(0.025, 0.975))

# save model results
model3.list[[i]] <- 
  summary(fit3)$fixed %>%
  mutate(r_squared = rsq[1], r_squared_null= rsq.null[1]) %>% 
  mutate(r_squared_diff= r_squared - r_squared_null) %>% 
  mutate(first_contact_period= first.contact.period) 

# progress bar
print(paste('-----------------',i, "of", length(fcp), '--------------------------'))

}

# compile model results
bind_rows(model1.list) %>% 
  rownames_to_column('term') %>% 
  mutate(term= substr(term, 1,9)) %>% 
  mutate(term = ifelse(term== "new.dyadu", "unfamiliarity", term)) %>% 
  write.csv("model_1_summary.csv")

bind_rows(model2.list) %>% 
  rownames_to_column('term') %>% 
  mutate(term= substr(term, 1, 21)) %>% 
  filter(term== "forced.proximityTRUE:") %>% 
  mutate(term= "forced.proximity") %>% 
  write.csv("model_2_reference_coefficients.csv")

bind_rows(model2.list) %>% 
  rownames_to_column('term') %>% 
  mutate(term= substr(term, 1, 12)) %>% 
  filter(term == "scalecontact") %>% 
  write.csv("model_2_contact_coefficients.csv")

bind_rows(model3.list) %>% 
  rownames_to_column('term') %>% 
  mutate(term= substr(term, 1, 12)) %>% 
  filter(term == "scalecontact") %>% 
  write.csv("model_3_contact_coefficients.csv")

# get end time
end <- Sys.time()

# get runtime
end-start

# estimate = 30 min

