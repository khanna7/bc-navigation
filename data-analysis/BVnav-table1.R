#Analyze calibration data
#Pulls data from .data files (written by demography-reset.R)
#Loads data into dataframes and generates plots 

##Setup
# Libraries  ----------
rm(list=ls())

library(dplyr)
library(ggplot2)
library(network)
library(networkDynamic)

## Add names of data directories here
#Directory name format is {date}_full_run
bc_navigation_root <- '/project2/khanna7/bryanb/bc-navigation/dec9_navlength/bc-navigation/'
date<-"13:50:15_2021-05-13" #3rd round (reset neighbor_navigation_roll at each navigation end point)
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
  "number.of.total.expired.screening.referrals" #39
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
  "cancer_status"
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
  "cancer_status"
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
  summarise(#m_navigation.length = mean(navigation.length),
    m_time.until.diagnosis = median(time.until.diagnosis),
    m_number.of.navigated.agents = mean(number.of.navigated.agents),
    m_number.of.screening.visits.at.t = mean(number.of.screening.visits.at.t),
    m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
    
    m_number.of.diagnostic.referrals.at.t = mean(number.of.diagnostic.referrals.at.t),
    m_number.of.screening.referrals.at.t = mean(number.of.screening.referrals.at.t),
    m_number.of.screening.referrals = mean(number.of.screening.referrals),
    m_number.of.diagnostic.referrals = mean(number.of.diagnostic.referrals),
    m_number.of.dt.completed = mean(number.of.dt.completed),
    m_number.of.screen.completed = mean(number.of.screen.completed),
    
    m_number.of.hpos.agents = mean(number.of.hpos.agents),
    m_number.of.hneg.agents = mean(number.of.hneg.agents),
    m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
    m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
    m_nintros = mean(nintros),
    
    m_early.diagnosed.ratio = mean((number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases),
    m_late.diagnosed.ratio = mean((number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases),
    m_number.of.expired.screening.referrals = mean(number.of.total.expired.screening.referrals),
    m_number.of.expired.diagnostic.referrals = mean(number.of.total.expired.diagnostic.referrals)
  )

intervention.df_mean_at_time <- 
  intervention.df %>% 
  group_by(time) %>%
  summarise(#m_navigation.length = mean(navigation.length),
    m_time.until.diagnosis = median(time.until.diagnosis),
    m_number.of.navigated.agents = mean(number.of.navigated.agents),
    m_number.of.screening.visits.at.t = mean(number.of.screening.visits.at.t),
    m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
    m_number.of.diagnostic.referrals.at.t = mean(number.of.diagnostic.referrals.at.t),
    m_number.of.screening.referrals.at.t = mean(number.of.screening.referrals.at.t),
    m_number.of.screening.referrals = mean(number.of.screening.referrals),
    m_number.of.diagnostic.referrals = mean(number.of.diagnostic.referrals),
    m_number.of.dt.completed = mean(number.of.dt.completed),
    m_number.of.screen.completed = mean(number.of.screen.completed),
    m_number.of.hpos.agents = mean(number.of.hpos.agents),
    m_number.of.hneg.agents = mean(number.of.hneg.agents),
    m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
    m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
    m_nintros = mean(nintros),
    m_early.diagnosed.ratio = mean((number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases),
    m_late.diagnosed.ratio = mean((number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases),
    m_number.of.expired.screening.referrals = mean(number.of.total.expired.screening.referrals),
    m_number.of.expired.diagnostic.referrals = mean(number.of.total.expired.diagnostic.referrals)
  )
noSocial_intervention.df_mean_at_time <- 
  noSocial_intervention.df %>% 
  group_by(time) %>%
  summarise(#m_navigation.length = mean(navigation.length),
    m_time.until.diagnosis = median(time.until.diagnosis),
    m_number.of.navigated.agents = mean(number.of.navigated.agents),
    m_number.of.screening.visits.at.t = mean(number.of.screening.visits.at.t),
    m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
    m_number.of.diagnostic.referrals.at.t = mean(number.of.diagnostic.referrals.at.t),
    m_number.of.screening.referrals.at.t = mean(number.of.screening.referrals.at.t),
    m_number.of.screening.referrals = mean(number.of.screening.referrals),
    m_number.of.diagnostic.referrals = mean(number.of.diagnostic.referrals),
    m_number.of.dt.completed = mean(number.of.dt.completed),
    m_number.of.screen.completed = mean(number.of.screen.completed),
    m_number.of.hpos.agents = mean(number.of.hpos.agents),
    m_number.of.hneg.agents = mean(number.of.hneg.agents),
    m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
    m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
    m_nintros = mean(nintros),
    m_early.diagnosed.ratio = mean((number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases),
    m_late.diagnosed.ratio = mean((number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases),
    m_number.of.expired.screening.referrals = mean(number.of.total.expired.screening.referrals),
    m_number.of.expired.diagnostic.referrals = mean(number.of.total.expired.diagnostic.referrals)
  )

##End Setup


##Stage at Diagnosis Table
####################################Early vs late diagnosis
#Plotting stage at logging by percent
n_total_referral_ends_control <- length(control_dt.df$time)
n_total_referral_ends_institutional <- length(noSocial_intervention_dt.df$time)
n_total_referral_ends_social <- length(intervention_dt.df$time)

bc_pos_n_total_referral_ends_control <- length(filter(control_dt.df, cancer_status==1)$time)
bc_pos_n_total_referral_ends_institutional <- length(filter(noSocial_intervention_dt.df, cancer_status==1)$time)
bc_pos_n_total_referral_ends_social <- length(filter(intervention_dt.df, cancer_status==1)$time)

nav_referral_ends_control <- filter(control_dt.df, navigated==1) 
nav_referral_ends_institutional <- filter(noSocial_intervention_dt.df, navigated==1)
nav_referral_ends_social <- filter(intervention_dt.df, navigated==1)

bc_pos_nav_referral_ends_control <- filter(nav_referral_ends_control, cancer_status==1) 
bc_pos_nav_referral_ends_institutional <- filter(nav_referral_ends_institutional, cancer_status==1)
bc_pos_nav_referral_ends_social <- filter(nav_referral_ends_social, cancer_status==1)
bc_neg_nav_referral_ends_control <- filter(nav_referral_ends_control, cancer_status==0) 
bc_neg_nav_referral_ends_institutional <- filter(nav_referral_ends_institutional, cancer_status==0)
bc_neg_nav_referral_ends_social <- filter(nav_referral_ends_social, cancer_status==0)

unnav_referral_ends_control <- filter(control_dt.df, navigated==0)
unnav_referral_ends_institutional <- filter(noSocial_intervention_dt.df, navigated==0)
unnav_referral_ends_social <- filter(intervention_dt.df, navigated==0)

bc_pos_unnav_referral_ends_control <- filter(unnav_referral_ends_control, cancer_status==1)
bc_pos_unnav_referral_ends_institutional <- filter(unnav_referral_ends_institutional, cancer_status==1)
bc_pos_unnav_referral_ends_social <- filter(unnav_referral_ends_social, cancer_status==1)
bc_neg_unnav_referral_ends_control <- filter(unnav_referral_ends_control, cancer_status==0)
bc_neg_unnav_referral_ends_institutional <- filter(unnav_referral_ends_institutional, cancer_status==0)
bc_neg_unnav_referral_ends_social <- filter(unnav_referral_ends_social, cancer_status==0)

#SS Breakdowns
control_SS_nav  <-table(nav_referral_ends_control$symptom_severity)
control_SS_unnav<-table(unnav_referral_ends_control$symptom_severity)
control_SS_total<-table(control_dt.df$symptom_severity)
#control_SS_bc_pos_nav<- table(bc_pos_nav_referral_ends_control$symptom_severity)
control_SS_bc_pos_unnav<- table(bc_pos_unnav_referral_ends_control$symptom_severity)
#control_SS_bc_neg_nav<- table(bc_neg_nav_referral_ends_control$symptom_severity)
control_SS_bc_neg_unnav<- table(bc_neg_unnav_referral_ends_control$symptom_severity)

institutional_SS_nav  <-table(nav_referral_ends_institutional$symptom_severity)
institutional_SS_unnav<-table(unnav_referral_ends_institutional$symptom_severity)
institutional_SS_total<-table(noSocial_intervention_dt.df$symptom_severity)
institutional_SS_bc_pos_nav  <- table(bc_pos_nav_referral_ends_institutional$symptom_severity)
institutional_SS_bc_pos_unnav<- table(bc_pos_unnav_referral_ends_institutional$symptom_severity)
institutional_SS_bc_neg_nav  <- table(bc_neg_nav_referral_ends_institutional$symptom_severity)
institutional_SS_bc_neg_unnav<- table(bc_neg_unnav_referral_ends_institutional$symptom_severity)

social_SS_nav  <-table(nav_referral_ends_social$symptom_severity)
social_SS_unnav<-table(unnav_referral_ends_social$symptom_severity)
social_SS_total<-table(intervention_dt.df$symptom_severity)
social_SS_bc_pos_nav  <- table(bc_pos_nav_referral_ends_social$symptom_severity)
social_SS_bc_pos_unnav<- table(bc_pos_unnav_referral_ends_social$symptom_severity)
social_SS_bc_neg_nav  <- table(bc_neg_nav_referral_ends_social$symptom_severity)
social_SS_bc_neg_unnav<- table(bc_neg_unnav_referral_ends_social$symptom_severity)

#SS Breakdown by percent
p_control_SS_nav<-control_SS_nav/sum(control_SS_nav)*100
p_control_SS_unnav<-control_SS_unnav/sum(control_SS_unnav)*100
p_control_SS_total<-control_SS_total/sum(control_SS_total)*100

p_institutional_SS_nav  <-institutional_SS_nav/sum(institutional_SS_nav)*100
p_institutional_SS_unnav<-institutional_SS_unnav/sum(institutional_SS_unnav)*100
p_institutional_SS_total<-institutional_SS_total/sum(institutional_SS_total)*100
p_institutional_SS_bc_pos_nav  <-institutional_SS_bc_pos_nav/sum(institutional_SS_bc_pos_nav)*100
p_institutional_SS_bc_pos_unnav<-institutional_SS_bc_pos_unnav/sum(institutional_SS_bc_pos_unnav)*100
p_institutional_SS_bc_neg_nav  <-institutional_SS_bc_neg_nav/sum(institutional_SS_bc_neg_nav)*100
p_institutional_SS_bc_neg_unnav<-institutional_SS_bc_neg_unnav/sum(institutional_SS_bc_neg_unnav)*100

p_social_SS_nav  <-social_SS_nav/sum(social_SS_nav)*100
p_social_SS_unnav<-social_SS_unnav/sum(social_SS_unnav)*100
p_social_SS_total<- social_SS_total/sum(social_SS_total)*100
p_social_SS_bc_pos_nav  <-social_SS_bc_pos_nav/sum(social_SS_bc_pos_nav)*100
p_social_SS_bc_pos_unnav<-social_SS_bc_pos_unnav/sum(social_SS_bc_pos_unnav)*100
p_social_SS_bc_neg_nav  <-social_SS_bc_neg_nav/sum(social_SS_bc_neg_nav)*100
p_social_SS_bc_neg_unnav<-social_SS_bc_neg_unnav/sum(social_SS_bc_neg_unnav)*100

p_institutional_SS_nav_early  <- sum(institutional_SS_nav[1:2])/sum(institutional_SS_nav)*100
p_institutional_SS_unnav_early<- sum(institutional_SS_unnav[1:2])/sum(institutional_SS_unnav)*100
p_institutional_SS_total_early<- sum(institutional_SS_total[1:2])/sum(institutional_SS_total)*100
p_institutional_SS_bc_pos_nav_early  <-sum(institutional_SS_bc_pos_nav[1:2])/sum(institutional_SS_bc_pos_nav)*100
p_institutional_SS_bc_pos_unnav_early<-sum(institutional_SS_bc_pos_unnav[1:2])/sum(institutional_SS_bc_pos_unnav)*100
p_institutional_SS_bc_neg_nav_early  <-sum(institutional_SS_bc_neg_nav[1:2])/sum(institutional_SS_bc_neg_nav)*100
p_institutional_SS_bc_neg_unnav_early<-sum(institutional_SS_bc_neg_unnav[1:2])/sum(institutional_SS_bc_neg_unnav)*100

p_social_SS_nav_early  <-social_SS_nav/sum(social_SS_nav)*100
p_social_SS_unnav_early<-social_SS_unnav/sum(social_SS_unnav)*100
p_social_SS_total_early<- social_SS_total/sum(social_SS_total)*100
p_social_SS_bc_pos_nav_early  <-sum(social_SS_bc_pos_nav[1:2])/sum(social_SS_bc_pos_nav)*100
p_social_SS_bc_pos_unnav_early<-sum(social_SS_bc_pos_unnav[1:2])/sum(social_SS_bc_pos_unnav)*100
p_social_SS_bc_neg_nav_early  <-sum(social_SS_bc_neg_nav[1:2])/sum(social_SS_bc_neg_nav)*100
p_social_SS_bc_neg_unnav_early<-sum(social_SS_bc_neg_unnav[1:2])/sum(social_SS_bc_neg_unnav)*100

p_institutional_SS_nav_late  <- sum(institutional_SS_nav[3:4])/sum(institutional_SS_nav)*100
p_institutional_SS_unnav_late<- sum(institutional_SS_unnav[3:4])/sum(institutional_SS_unnav)*100
p_institutional_SS_total_late<- sum(institutional_SS_total[3:4])/sum(institutional_SS_total)*100
p_institutional_SS_bc_pos_nav_late <-sum(institutional_SS_bc_pos_nav[3:4])/sum(institutional_SS_bc_pos_nav)*100
p_institutional_SS_bc_pos_unnav_late<-sum(institutional_SS_bc_pos_unnav[3:4])/sum(institutional_SS_bc_pos_unnav)*100
p_institutional_SS_bc_neg_nav_late <-sum(institutional_SS_bc_neg_nav[3:4])/sum(institutional_SS_bc_neg_nav)*100
p_institutional_SS_bc_neg_unnav_late<-sum(institutional_SS_bc_neg_unnav[3:4])/sum(institutional_SS_bc_neg_unnav)*100

p_social_SS_nav_late  <-sum(social_SS_nav[3:4])/sum(social_SS_nav)*100
p_social_SS_unnav_late<-sum(social_SS_unnav[3:4])/sum(social_SS_unnav)*100
p_social_SS_total_late<- sum(social_SS_total[3:4])/sum(social_SS_total)*100
p_social_SS_bc_pos_nav_late  <-sum(social_SS_bc_pos_nav[3:4])/sum(social_SS_bc_pos_nav)*100
p_social_SS_bc_pos_unnav_late<-sum(social_SS_bc_pos_unnav[3:4])/sum(social_SS_bc_pos_unnav)*100
p_social_SS_bc_neg_nav_late  <-sum(social_SS_bc_neg_nav[3:4])/sum(social_SS_bc_neg_nav)*100
p_social_SS_bc_neg_unnav_late<-sum(social_SS_bc_neg_unnav[3:4])/sum(social_SS_bc_neg_unnav)*100

#Generate table
p_control_SS_total
p_control_SS_unnav

p_institutional_SS_total
p_institutional_SS_nav
p_institutional_SS_unnav

cro(p_social_SS_total,
p_social_SS_nav,
p_social_SS_unnav
)

cro(mtcars$am,mtcars$vs)
