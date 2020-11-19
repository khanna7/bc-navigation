# Analyze calibration data

# Libraries  ----------

rm(list=ls())

library(dplyr)
library(ggplot2)
library(network)
library(networkDynamic)

# Read data and set meta-parameters ----------
setwd('/project2/khanna7/bryanb/bc-navigation/intervention_testing')
getwd()
N <- 5000
n.instances <- 20

control_list <- as.list(1:n.instances)
intervention_list <- as.list(1:n.instances)
noSocial_intervention_list <- as.list(1:n.instances)

dt_columns <- c(
  #see `write.table` in https://github.com/khanna7/bc-navigation/blob/master/demography-reset.R for col names
  "time",
  "nintros", #deaths
  "number.of.positive.bc.agents",
  "number.of.hpos.agents",
  "number.of.hneg.agents",
  "number.of.diagnosed.cases",
  
  "number.of.diagnostic.referrals",
  "number.of.screening.referrals",
  "number.of.screen.completed",
  "number.of.dt.completed",
  "number.of.symptomatic",
  "number.of.navigated.agents",
  
  "time.with.cancer",
  "time.until.diagnosis",
  "time.until.diagnosis.navigated",
  "time.until.diagnosis.unnavigated",
  "time.until.diagnosis.neigbor.navigated",
  "number.of.diagnostic.referrals.at.t",
  
  "number.of.screening.visits.at.t",#19
  
  "number.of.ss0.diagnosed",#20
  "number.of.ss1.diagnosed",#21
  "number.of.ss2.diagnosed",#22
  "number.of.ss3.diagnosed",#23
  
  "number.of.ss0.diagnosed.navigated",#24
  "number.of.ss1.diagnosed.navigated",#25
  "number.of.ss2.diagnosed.navigated",#26
  "number.of.ss3.diagnosed.navigated",#27
  
  "number.of.ss0.diagnosed.unnavigated",#28
  "number.of.ss1.diagnosed.unnavigated",#29
  "number.of.ss2.diagnosed.unnavigated",#30
  "number.of.ss3.diagnosed.unnavigated",#31
  
  "number.of.ss0.diagnosed.neighbor_navigated", #32
  "number.of.ss1.diagnosed.neighbor_navigated", #33
  "number.of.ss2.diagnosed.neighbor_navigated", #34
  "number.of.ss3.diagnosed.neighbor_navigated", #35
  "number.of.bc.onsets", #36
  "screening_referral_checker" #37
)

#create lists
for (i in 1:n.instances){
  control_list[[i]] <- read.table(paste0("nov3_control/data/", i,".data"))
  }
for (i in 1:n.instances){
  intervention_list[[i]] <- read.table(paste0("10nov_socialFix/data/", i,".data"))
  }
for (i in 1:n.instances){
  noSocial_intervention_list[[i]] <- read.table(paste0("nov3_nosocial/data/", i,".data"))
  }


which(unlist(lapply(control_list, nrow) != 360)) #number of months of the simulation
which(unlist(lapply(intervention_list, nrow) != 360)) 
which(unlist(lapply(noSocial_intervention_list, nrow) != 360)) 


control.df <- bind_rows(control_list[1:n.instances])
intervention.df <- bind_rows(intervention_list[1:n.instances])
noSocial_intervention.df <- bind_rows(noSocial_intervention_list[1:n.instances])

colnames(control.df) <- dt_columns
colnames(intervention.df) <- dt_columns
colnames(noSocial_intervention.df) <- dt_columns

control.df$source <- rep(1:n.instances, each=360)
intervention.df$source <- rep(1:n.instances, each=360)
noSocial_intervention.df$source <- rep(1:n.instances, each=360)


# Compute incidence rate ----------

control.df <- 
  control.df %>%
  mutate(number.of.bc.neg = N - number.of.positive.bc.agents)  
intervention.df <- 
  intervention.df %>%
  mutate(number.of.bc.neg = N - number.of.positive.bc.agents)  
noSocial_intervention.df <- 
  noSocial_intervention.df %>%
  mutate(number.of.bc.neg = N - number.of.positive.bc.agents)  

# Compute means across variables at given time ----------

control.df_mean_at_time <- 
  control.df %>% 
  group_by(time) %>%
  summarise(m_time.until.diagnosis = median(time.until.diagnosis),
            m_number.of.navigated.agents = mean(number.of.navigated.agents),
	          m_number.of.screening.visits.at.t = mean(number.of.screening.visits.at.t),
            m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
            m_number.of.diagnostic.referrals.at.t = mean(number.of.diagnostic.referrals.at.t),
            m_number.of.screening.referrals = mean(number.of.screening.referrals),
            m_number.of.dt.completed = mean(number.of.dt.completed),
            m_number.of.screen.completed = mean(number.of.screen.completed),
            m_number.of.hpos.agents = mean(number.of.hpos.agents),
            m_number.of.hneg.agents = mean(number.of.hneg.agents),
            m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
            m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
            m_nintros = mean(nintros)
  )
intervention.df_mean_at_time <- 
  intervention.df %>% 
  group_by(time) %>%
  summarise(m_time.until.diagnosis = median(time.until.diagnosis),
            m_number.of.navigated.agents = mean(number.of.navigated.agents),
	          m_number.of.screening.visits.at.t = mean(number.of.screening.visits.at.t),
            m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
            m_number.of.diagnostic.referrals.at.t = mean(number.of.diagnostic.referrals.at.t),
            m_number.of.screening.referrals = mean(number.of.screening.referrals),
            m_number.of.dt.completed = mean(number.of.dt.completed),
            m_number.of.screen.completed = mean(number.of.screen.completed),
            m_number.of.hpos.agents = mean(number.of.hpos.agents),
            m_number.of.hneg.agents = mean(number.of.hneg.agents),
            m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
            m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
            m_nintros = mean(nintros)
  )
noSocial_intervention.df_mean_at_time <- 
  noSocial_intervention.df %>% 
  group_by(time) %>%
  summarise(m_time.until.diagnosis = median(time.until.diagnosis),
            m_number.of.navigated.agents = mean(number.of.navigated.agents),
  	    m_number.of.screening.visits.at.t = mean(number.of.screening.visits.at.t),
            m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
            m_number.of.diagnostic.referrals.at.t = mean(number.of.diagnostic.referrals.at.t),
            m_number.of.screening.referrals = mean(number.of.screening.referrals),
            m_number.of.dt.completed = mean(number.of.dt.completed),
            m_number.of.screen.completed = mean(number.of.screen.completed),
            m_number.of.hpos.agents = mean(number.of.hpos.agents),
            m_number.of.hneg.agents = mean(number.of.hneg.agents),
            m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
            m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
            m_nintros = mean(nintros)
  )
  
# Plots ----------

#may be used to color legend
colors <- c("control" = "green", "full intervention" = "blue", "no social" = "red")

#Time to diagnosis
ggplot()+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_time.until.diagnosis,color='green'))+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=time.until.diagnosis,color='green'))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_time.until.diagnosis,color='red'))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=time.until.diagnosis, color='red'))+
  geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_time.until.diagnosis, color='blue'))+
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=time.until.diagnosis, color='blue'))#+
  #scale_color_manual(
  #  values = c('blue', 'red', 'green'),
  #  labels = c("control", "Institutional with social", "Institutional without social")
  #)
      
#Time from referral to testing (THIS WILL REQUIRE SUBSTANTIAL WORK: MODEL NEEDS TO RECORD TIME OF REFERRAL)
            
            

#Total number of navigated
ggplot()+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.navigated.agents, color='green'))+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.navigated.agents, color='green'))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.navigated.agents, color='red'))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.navigated.agents, color='red'))+
  geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.navigated.agents, color='blue'))+
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.navigated.agents,color='blue'))#+
 # scale_color_manual(
  #  values = c("green", "blue", "red"),
   # labels = c("control", "Institutional with social", "Institutional without social")
  #)
#number.of.positive.bc.agents
ggplot()+
  theme_bw()+
  geom_line(color='green', data = control.df_mean_at_time, aes(x=time, y=m_number.of.positive.bc.agents))+
  geom_line(data=control.df, color='green', alpha=0.1, aes(x=time, y=number.of.positive.bc.agents))+
  geom_line(color='red',data = noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.positive.bc.agents))+
  geom_line(data=noSocial_intervention.df, color='red', alpha=0.1, aes(x=time, y=number.of.positive.bc.agents))+
  geom_line(color='blue',data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.positive.bc.agents))+
  geom_line(data=intervention.df, color='blue', alpha=0.1, aes(x=time, y=number.of.positive.bc.agents))

#(number.of.positive.bc.agents/N)*100
#Breast Cancer Prevalence (%)
ggplot()+
  theme_bw()+
  geom_line(data=control.df, color='green', alpha=0.1,aes(x=time, y=((number.of.positive.bc.agents/N)*100)))+
  geom_line(data=control.df_mean_at_time, color='green', aes(x=time, y=((m_number.of.positive.bc.agents/N)*100)))+
  geom_line(data=noSocial_intervention.df, color='red', alpha=0.1,aes(x=time, y=((number.of.positive.bc.agents/N)*100)))+
  geom_line(data=noSocial_intervention.df_mean_at_time, color='red', aes(x=time, y=((m_number.of.positive.bc.agents/N)*100)))+
  geom_line(data=intervention.df, color='blue', alpha=0.1,aes(x=time, y=((number.of.positive.bc.agents/N)*100)))+
  geom_line(data=intervention.df_mean_at_time, color='blue', aes(x=time, y=((m_number.of.positive.bc.agents/N)*100)))+
    ylim(c(0,5))+
  labs(y="Breast Cancer Prevalence (%)")

#number.of.screening.visits.at.t
ggplot()+  
  theme_bw()+
  geom_line(data=control.df, color='green',alpha=0.1, aes(x=time, y=number.of.screening.visits.at.t))+
  geom_line(data=control.df_mean_at_time,color='green', aes(x=time, y=m_number.of.screening.visits.at.t))+
  geom_line(data=noSocial_intervention.df, color='red',alpha=0.1, aes(x=time, y=number.of.screening.visits.at.t))+
  geom_line(data=noSocial_intervention.df_mean_at_time,color='red', aes(x=time, y=m_number.of.screening.visits.at.t))+  
  geom_line(data=intervention.df, color='blue',alpha=0.1, aes(x=time, y=number.of.screening.visits.at.t))+
  geom_line(data=intervention.df_mean_at_time,color='blue', aes(x=time, y=m_number.of.screening.visits.at.t))

#number.of.diagnostic.referrals.at.t
ggplot()+
  theme_bw()+
  geom_line(data=control.df, color='green',alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t))+
  geom_line(data=control.df_mean_at_time,color='green', aes(x=time, y=m_number.of.diagnostic.referrals.at.t))+
  geom_line(data=noSocial_intervention.df, color='red',alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t))+
  geom_line(data=noSocial_intervention.df_mean_at_time,color='red', aes(x=time, y=m_number.of.diagnostic.referrals.at.t))+  
  geom_line(data=intervention.df, color='blue',alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t))+
  geom_line(data=intervention.df_mean_at_time,color='blue', aes(x=time, y=m_number.of.diagnostic.referrals.at.t))

#number.of.screening.referrals
ggplot()+
  theme_bw()+
  geom_line(data=control.df, color='green',alpha=0.1, aes(x=time, y=number.of.screening.referrals))+
  geom_line(data=control.df_mean_at_time,color='green', aes(x=time, y=m_number.of.screening.referrals))+
  geom_line(data=noSocial_intervention.df, color='red',alpha=0.1, aes(x=time, y=number.of.screening.referrals))+
  geom_line(data=noSocial_intervention.df_mean_at_time,color='red', aes(x=time, y=m_number.of.screening.referrals))+  
  geom_line(data=intervention.df, color='blue',alpha=0.1, aes(x=time, y=number.of.screening.referrals))+
  geom_line(data=intervention.df_mean_at_time,color='blue', aes(x=time, y=m_number.of.screening.referrals))

#number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t
ggplot()+
  theme_bw()+
  geom_line(data=control.df, color='green',alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t))+
  geom_line(data=control.df_mean_at_time,color='green', aes(x=time, y=m_number.of.diagnostic.referrals.at.t/m_number.of.screening.visits.at.t))+
  geom_line(data=noSocial_intervention.df, color='red',alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t))+
  geom_line(data=noSocial_intervention.df_mean_at_time,color='red', aes(x=time, y=m_number.of.diagnostic.referrals.at.t/m_number.of.screening.visits.at.t))+  
  geom_line(data=intervention.df, color='blue',alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t))+
  geom_line(data=intervention.df_mean_at_time,color='blue', aes(x=time, y=m_number.of.diagnostic.referrals.at.t/m_number.of.screening.visits.at.t))+
    ylim(c(0,0.5))

#number.of.dt.completed/number.of.screen.completed
ggplot()+
  theme_bw()+
  geom_line(data=control.df, color='green',alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.screen.completed))+
  geom_line(data=control.df_mean_at_time,color='green', aes(x=time, y=m_number.of.dt.completed/m_number.of.screen.completed))+
  geom_line(data=noSocial_intervention.df, color='red',alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.screen.completed))+
  geom_line(data=noSocial_intervention.df_mean_at_time,color='red', aes(x=time, y=m_number.of.dt.completed/m_number.of.screen.completed))+  
  geom_line(data=intervention.df, color='blue',alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.screen.completed))+
  geom_line(data=intervention.df_mean_at_time,color='blue', aes(x=time, y=m_number.of.dt.completed/m_number.of.screen.completed))
  
#number.of.screen.completed
ggplot()+
  theme_bw()+
  geom_line(data=control.df, color='green',alpha=0.1, aes(x=time, y=number.of.screen.completed))+
  geom_line(data=control.df_mean_at_time,color='green', aes(x=time, y=m_number.of.screen.completed))+
  geom_line(data=noSocial_intervention.df, color='red',alpha=0.1, aes(x=time, y=number.of.screen.completed))+
  geom_line(data=noSocial_intervention.df_mean_at_time,color='red', aes(x=time, y=m_number.of.screen.completed))+  
  geom_line(data=intervention.df, color='blue',alpha=0.1, aes(x=time, y=number.of.screen.completed))+
  geom_line(data=intervention.df_mean_at_time,color='blue', aes(x=time, y=m_number.of.screen.completed))

#number.of.dt.completed
ggplot()+
  theme_bw()+
  geom_line(data=control.df, color='green',alpha=0.1, aes(x=time, y=number.of.dt.completed))+
  geom_line(data=control.df_mean_at_time,color='green', aes(x=time, y=m_number.of.dt.completed))+
  geom_line(data=noSocial_intervention.df, color='red',alpha=0.1, aes(x=time, y=number.of.dt.completed))+
  geom_line(data=noSocial_intervention.df_mean_at_time,color='red', aes(x=time, y=m_number.of.dt.completed))+  
  geom_line(data=intervention.df, color='blue',alpha=0.1, aes(x=time, y=number.of.dt.completed))+
  geom_line(data=intervention.df_mean_at_time,color='blue', aes(x=time, y=m_number.of.dt.completed))

##Skipped for now
ggplot(control.df, aes(x=time))+
  geom_line(aes(y=number.of.hpos.agents), alpha=0.1)+
  geom_line(aes(y=number.of.hneg.agents), alpha=0.1)+
  geom_line(aes(y=number.of.positive.bc.agents), alpha=0.1)+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.hpos.agents))+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.hneg.agents))+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.positive.bc.agents))+
  ylim(c(0, 150))+
  annotate(geom="text", x=625, y=112, label="All cases", col="black")+
  annotate(geom="text", x=625, y=90, label="Hormone-positive", col="black")+
  annotate(geom="text", x=625, y=30, label="Hormone-negative", col="black")+
  labs(y="number")

#number.of.hneg.agents/number.of.hpos.agents
ggplot()+
  theme_bw()+
  geom_line(data=control.df, color='green',alpha=0.1, aes(x=time, y=number.of.hneg.agents/number.of.hpos.agents))+
  geom_line(data=control.df_mean_at_time,color='green', aes(x=time, y=m_number.of.hneg.agents/m_number.of.hpos.agents))+
  geom_line(data=noSocial_intervention.df, color='red',alpha=0.1, aes(x=time, y=number.of.hneg.agents/number.of.hpos.agents))+
  geom_line(data=noSocial_intervention.df_mean_at_time,color='red', aes(x=time, y=m_number.of.hneg.agents/m_number.of.hpos.agents))+  
  geom_line(data=intervention.df, color='blue',alpha=0.1, aes(x=time, y=number.of.hneg.agents/number.of.hpos.agents))+
  geom_line(data=intervention.df_mean_at_time,color='blue', aes(x=time, y=m_number.of.hneg.agents/m_number.of.hpos.agents))+
    ylim(c(0,1))

#number.of.diagnosed.cases
ggplot()+
  theme_bw()+
  geom_line(data=control.df, color='green',alpha=0.1, aes(x=time, y=number.of.diagnosed.cases))+
  geom_line(data=control.df_mean_at_time,color='green', aes(x=time, y=m_number.of.diagnosed.cases))+
  geom_line(data=noSocial_intervention.df, color='red',alpha=0.1, aes(x=time, y=number.of.diagnosed.cases))+
  geom_line(data=noSocial_intervention.df_mean_at_time,color='red', aes(x=time, y=m_number.of.diagnosed.cases))+  
  geom_line(data=intervention.df, color='blue',alpha=0.1, aes(x=time, y=number.of.diagnosed.cases))+
  geom_line(data=intervention.df_mean_at_time,color='blue', aes(x=time, y=m_number.of.diagnosed.cases))

#number.of.positive.bc.agents
ggplot()+
  theme_bw()+
  geom_line(data=control.df, color='green',alpha=0.1, aes(x=time, y=number.of.positive.bc.agents))+
  geom_line(data=control.df_mean_at_time,color='green', aes(x=time, y=m_number.of.positive.bc.agents))+
  geom_line(data=noSocial_intervention.df, color='red',alpha=0.1, aes(x=time, y=number.of.positive.bc.agents))+
  geom_line(data=noSocial_intervention.df_mean_at_time,color='red', aes(x=time, y=m_number.of.positive.bc.agents))+  
  geom_line(data=intervention.df, color='blue',alpha=0.1, aes(x=time, y=number.of.positive.bc.agents))+
  geom_line(data=intervention.df_mean_at_time,color='blue', aes(x=time, y=m_number.of.positive.bc.agents))

#nintros
ggplot()+
  theme_bw()+
  geom_line(data=control.df, color='green',alpha=0.1, aes(x=time, y=nintros))+
  geom_line(data=control.df_mean_at_time,color='green', aes(x=time, y=m_nintros))+
  geom_line(data=noSocial_intervention.df, color='red',alpha=0.1, aes(x=time, y=nintros))+
  geom_line(data=noSocial_intervention.df_mean_at_time,color='red', aes(x=time, y=m_nintros))+  
  geom_line(data=intervention.df, color='blue',alpha=0.1, aes(x=time, y=nintros))+
  geom_line(data=intervention.df_mean_at_time,color='blue', aes(x=time, y=m_nintros))

