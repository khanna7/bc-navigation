  #Analyze calibration data
#Pulls data from .data files (written by demography-reset.R)
#Loads data into dataframes and generates plots 

##Setup
# Libraries  ----------
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(network)
library(networkDynamic)

## Add names of data directories here
#Directory name format is {date}_full_run
bc_navigation_root <- '/project2/ahotton/bryanb/bc-navigation/dec9_navlength/bc-navigation/'
#date<-"20:53:45_2021-06-08" 
date <- "15:54:31_2021-07-08"
full_run_name <- paste0(date, '_full_run)/')

# Read data and set meta-parameters ----------
N<-5000 #number of agents
n.instances<-30 #number of runs
run_length<-360 #number of time steps in run

control_list <- as.list(1:n.instances)
intervention_list <- as.list(1:n.instances)
noSocial_intervention_list <- as.list(1:n.instances)

control_dt_list <- as.list(1:n.instances)
intervention_dt_list <- as.list(1:n.instances)
noSocial_intervention_dt_list <- as.list(1:n.instances)

control_sc_list <- as.list(1:n.instances)
intervention_sc_list <- as.list(1:n.instances)
noSocial_intervention_sc_list <- as.list(1:n.instances)

dt_columns <- c(
  #see `write.table` in https://github.com/khanna7/bc-navigation/blob/master/demography-reset.R for col names
  "time", #TODO set to "time-step"
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
  "number.of.screening.referrals.at.t",#37
  "number.of.total.expired.diagnostic.referrals",#38
  "number.of.total.expired.screening.referrals", #39
  
  "n_ss0",
  "n_ss1",
  "n_ss2",
  "n_ss3"
)

diagnostic_event_columns <- c(
  "time",
  "agent",
  "diagnostic_referral_length",
  "expired",
  "within_2_months",
  "symptom_severity",
  "navigated",
  "screening_referral_length",
  "total_care_length",
  "instance",
  "cancer_status",
  "neighbor_navigated"
)

screening_event_columns <- c(
  "time",
  "agent",
  "screening_referral_length",
  "expired",
  "within_2_months",
  "symptom_severity",
  "navigated",
  "slurm_instance",
  "cancer_status",
  "neighbor_navigated"
)

#create lists
for (i in 1:n.instances){
  control_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date, '_control/data/', i,".data"))
}
for (i in 1:n.instances){
  intervention_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date,'_intervention/data/', i,".data"))
}
for (i in 1:n.instances){
  noSocial_intervention_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date, '_interventionNoSocial/data/', i,".data"))
}
#Diagnostic event data handling
for (i in 1:n.instances){
  control_dt_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date, '_control/diagnostic_event_logs/', i,"_diagnostic.events"))
}
for (i in 1:n.instances){
  intervention_dt_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date,'_intervention/diagnostic_event_logs/', i,"_diagnostic.events"))
}
for (i in 1:n.instances){
  noSocial_intervention_dt_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date, '_interventionNoSocial/diagnostic_event_logs/', i,"_diagnostic.events"))
}
#Screening event data handling
for (i in 1:n.instances){
  control_sc_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date, '_control/diagnostic_event_logs/', i,"_screening.events"))
}
for (i in 1:n.instances){
  intervention_sc_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date,'_intervention/diagnostic_event_logs/', i,"_screening.events"))
}
for (i in 1:n.instances){
  noSocial_intervention_sc_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date, '_interventionNoSocial/diagnostic_event_logs/', i,"_screening.events"))
}

#check whether the listed data is of the right length. Returns the instances that are missing data.
which(unlist(lapply(control_list, nrow) != run_length)) #number of months of the simulation
which(unlist(lapply(intervention_list, nrow) != run_length)) 
which(unlist(lapply(noSocial_intervention_list, nrow) != run_length)) 

control.df <- bind_rows(control_list[1:n.instances])
intervention.df <- bind_rows(intervention_list[1:n.instances])
noSocial_intervention.df <- bind_rows(noSocial_intervention_list[1:n.instances])

control_dt.df <- bind_rows(control_dt_list[1:n.instances])
intervention_dt.df <- bind_rows(intervention_dt_list[1:n.instances])
noSocial_intervention_dt.df <- bind_rows(noSocial_intervention_dt_list[1:n.instances])

# Not Sure What this is all about
control_sc.df <- bind_rows(control_sc_list[1:n.instances])
intervention_sc.df <- bind_rows(intervention_sc_list[1:n.instances])
noSocial_intervention_sc.df <- bind_rows(noSocial_intervention_sc_list[1:n.instances])

colnames(control.df) <- dt_columns
colnames(intervention.df) <- dt_columns
colnames(noSocial_intervention.df) <- dt_columns

colnames(control_dt.df) <- diagnostic_event_columns
colnames(intervention_dt.df) <- diagnostic_event_columns
colnames(noSocial_intervention_dt.df) <- diagnostic_event_columns

colnames(control_sc.df) <- screening_event_columns
colnames(intervention_sc.df) <- screening_event_columns
colnames(noSocial_intervention_sc.df) <- screening_event_columns

control.df$source <- rep(1:n.instances, each=run_length)
intervention.df$source <- rep(1:n.instances, each=run_length)
noSocial_intervention.df$source <- rep(1:n.instances, each=run_length)



# confidence interval calculation functions
#Confidence intervals | Switched to t-dist from normal dist
upper_ci <- function(data){
  x_bar <- mean(data)
  sigma <- sd(data)
  n.sim <- length(data)
    A = qt(0.975, df=n.sim-1)*sigma/sqrt(n.sim)
  left <- x_bar - A
  right <- x_bar + A
  ci_interval <- c(round(left,digits=2),round(right,digits=2))
  return(right)
}
lower_ci <- function(data){
  x_bar <- mean(data)
  sigma <- sd(data)
  n.sim <- length(data)
  A = qt(0.975, df=n.sim-1)*sigma/sqrt(n.sim)
  left <- x_bar - A
  right <- x_bar + A
  ci_interval <- c(round(left,digits=2),round(right,digits=2))
  return(left)
}

#Local/Regional/Distant version
#CONTROL
control_grouped_ss<- control_dt.df %>%
  filter(cancer_status == 1) %>%
  select(symptom_severity, instance) %>%
  mutate(scenario = "control") %>%
  group_by(instance)

ctemp <- c()
for(i in c(1:30)){
  #temp <- append(temp,i)
  for (j in 0:3){
    ctemp <- append(ctemp,length(filter(control_grouped_ss, instance == i & symptom_severity == j)$symptom_severity)/length(filter(control_grouped_ss, instance == i)$symptom_severity)*100)
  }
}
control_ss1 <- ctemp[seq(1,length(ctemp),4)]
control_ss2 <- ctemp[seq(2,length(ctemp),4)]
control_ss3 <- ctemp[seq(3,length(ctemp),4)]
control_ss4 <- ctemp[seq(4,length(ctemp),4)]

#Localized
cat(round(mean(control_ss1+control_ss2), digits=2),"%(",round(lower_ci(control_ss1+control_ss2), digits=2),",",round(upper_ci(control_ss1+control_ss2), digits=2),")")
#Regional
cat(round(mean(control_ss3), digits=2),"%(",round(lower_ci(control_ss3), digits=2),",",round(upper_ci(control_ss3), digits=2),")")
#Distant
cat(round(mean(control_ss4), digits=2),"%(",round(lower_ci(control_ss4), digits=2),",",round(upper_ci(control_ss4), digits=2),")")

#INSTITUTIONAL total SS at diagnosis breakdown
institutional_grouped_ss<- noSocial_intervention_dt.df %>%
  filter(cancer_status == 1) %>%
  select(symptom_severity, instance) %>%
  mutate(scenario = "control") %>%
  group_by(instance)

itemp <- c()
for(i in c(1:30)){
  for (j in 0:3){
    itemp <- append(itemp,length(filter(institutional_grouped_ss, instance == i & symptom_severity == j)$symptom_severity)/length(filter(institutional_grouped_ss, instance == i)$symptom_severity)*100)
  }
}
institutional_ss1 <- itemp[seq(1,length(itemp),4)]
institutional_ss2 <- itemp[seq(2,length(itemp),4)]
institutional_ss3 <- itemp[seq(3,length(itemp),4)]
institutional_ss4 <- itemp[seq(4,length(itemp),4)]

#Localized
cat(round(mean(institutional_ss1+institutional_ss2), digits=2),"%(",round(lower_ci(institutional_ss1+institutional_ss2), digits=2),",",round(upper_ci(institutional_ss1+institutional_ss2), digits=2),")")
#Regional
cat(round(mean(institutional_ss3), digits=2),"%(",round(lower_ci(institutional_ss3), digits=2),",",round(upper_ci(institutional_ss3), digits=2),")")
#Distant
cat(round(mean(institutional_ss4), digits=2),"%(",round(lower_ci(institutional_ss4), digits=2),",",round(upper_ci(institutional_ss4), digits=2),")")

#SOCIAL
social_grouped_ss <- intervention_dt.df %>%
  filter(cancer_status == 1) %>%
  select(symptom_severity, instance) %>%
  mutate(scenario = "control") %>%
  group_by(instance)

stemp <- c()
for(i in c(1:30)){
  for (j in 0:3){
    stemp <- append(stemp,length(filter(social_grouped_ss, instance == i & symptom_severity == j)$symptom_severity)/length(filter(social_grouped_ss, instance == i)$symptom_severity)*100)
  }
}
social_ss1 <- stemp[seq(1,length(stemp),4)]
social_ss2 <- stemp[seq(2,length(stemp),4)]
social_ss3 <- stemp[seq(3,length(stemp),4)]
social_ss4 <- stemp[seq(4,length(stemp),4)]

#Localized
cat(round(mean(social_ss1+social_ss2), digits=2),"%(",round(lower_ci(social_ss1+social_ss2), digits=2),",",round(upper_ci(social_ss1+social_ss2), digits=2),")")
#Regional
cat(round(mean(social_ss3), digits=2),"%(",round(lower_ci(social_ss3), digits=2),",",round(upper_ci(social_ss3), digits=2),")")
#Distant
cat(round(mean(social_ss4), digits=2),"%(",round(lower_ci(social_ss4), digits=2),",",round(upper_ci(social_ss4), digits=2),")")

