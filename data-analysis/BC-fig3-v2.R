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
library(cowplot)

## Add names of data directories here
#Directory name format is {date}_full_run
bc_navigation_root <- '/project2/khanna7/bryanb/bc-navigation/dec9_navlength/bc-navigation/'
date<-"15:54:31_2021-07-08" #3rd round (reset neighbor_navigation_roll at each navigation end point)
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
####BEGIN PLOTS#############################################################################
con<-"Control"
ins<-"Institutional without social"
soc<-"Institutional with social"

##DIAGNOSTIC
#Cumulative Completion Percentage by Length
control_expirations <- control_dt.df %>% 
                       group_by(instance) %>% 
                       summarize("RL1" =length(which(diagnostic_referral_length==1))/length(agent)*100,#length of agent column gives us total # of dt referrals
                                 "RL2" =length(which(diagnostic_referral_length==2))/length(agent)*100,
                                 "RL3" =length(which(diagnostic_referral_length==3))/length(agent)*100,
                                 "RL4" =length(which(diagnostic_referral_length==4))/length(agent)*100,
                                 "RL5" =length(which(diagnostic_referral_length==5))/length(agent)*100,
                                 "RL6" =length(which(diagnostic_referral_length==6))/length(agent)*100,
                                 "RL7" =length(which(diagnostic_referral_length==7))/length(agent)*100,
                                 "RL8" =length(which(diagnostic_referral_length==8))/length(agent)*100,
                                 "RL9" =length(which(diagnostic_referral_length==9))/length(agent)*100,
                                 "RL10"=length(which(diagnostic_referral_length==10))/length(agent)*100,
                                 "RL11"=length(which(diagnostic_referral_length==11))/length(agent)*100,
                                 "RL12"=length(which(diagnostic_referral_length==12))/length(agent)*100
                                 )

noSocial_expirations <- noSocial_intervention_dt.df %>% 
                        group_by(instance) %>% 
                        summarize("RL1" =length(which(diagnostic_referral_length==1))/length(agent)*100,#length of agent column gives us total # of dt referrals
                                  "RL2" =length(which(diagnostic_referral_length==2))/length(agent)*100,
                                  "RL3" =length(which(diagnostic_referral_length==3))/length(agent)*100,
                                  "RL4" =length(which(diagnostic_referral_length==4))/length(agent)*100,
                                  "RL5" =length(which(diagnostic_referral_length==5))/length(agent)*100,
                                  "RL6" =length(which(diagnostic_referral_length==6))/length(agent)*100,
                                  "RL7" =length(which(diagnostic_referral_length==7))/length(agent)*100,
                                  "RL8" =length(which(diagnostic_referral_length==8))/length(agent)*100,
                                  "RL9" =length(which(diagnostic_referral_length==9))/length(agent)*100,
                                  "RL10"=length(which(diagnostic_referral_length==10))/length(agent)*100,
                                  "RL11"=length(which(diagnostic_referral_length==11))/length(agent)*100,
                                  "RL12"=length(which(diagnostic_referral_length==12))/length(agent)*100
                        )

social_expirations <- intervention_dt.df %>% 
                      group_by(instance) %>% 
                      summarize("RL1" =length(which(diagnostic_referral_length==1))/length(agent)*100,#length of agent column gives us total # of dt referrals
                                "RL2" =length(which(diagnostic_referral_length==2))/length(agent)*100,
                                "RL3" =length(which(diagnostic_referral_length==3))/length(agent)*100,
                                "RL4" =length(which(diagnostic_referral_length==4))/length(agent)*100,
                                "RL5" =length(which(diagnostic_referral_length==5))/length(agent)*100,
                                "RL6" =length(which(diagnostic_referral_length==6))/length(agent)*100,
                                "RL7" =length(which(diagnostic_referral_length==7))/length(agent)*100,
                                "RL8" =length(which(diagnostic_referral_length==8))/length(agent)*100,
                                "RL9" =length(which(diagnostic_referral_length==9))/length(agent)*100,
                                "RL10"=length(which(diagnostic_referral_length==10))/length(agent)*100,
                                "RL11"=length(which(diagnostic_referral_length==11))/length(agent)*100,
                                "RL12"=length(which(diagnostic_referral_length==12))/length(agent)*100
                                )

control_expirations <-cbind(control_expirations, "scenario"= "Control")
noSocial_expirations <- cbind(noSocial_expirations, "scenario"= "Institutional Only")
social_expirations <- cbind(social_expirations, "scenario"= "Institutional and Social")

colnames(control_expirations)  <- c("Instance", "RL1", "RL2", "RL3", "RL4", "RL5", "RL6", "RL7", "RL8", "RL9", "RL10", "RL11", "RL12", "Scenario")
colnames(noSocial_expirations) <- c("Instance", "RL1", "RL2", "RL3", "RL4", "RL5", "RL6", "RL7", "RL8", "RL9", "RL10", "RL11", "RL12", "Scenario")
colnames(social_expirations)   <- c("Instance", "RL1", "RL2", "RL3", "RL4", "RL5", "RL6", "RL7", "RL8", "RL9", "RL10", "RL11", "RL12", "Scenario")

total_expirations <- rbind(control_expirations, noSocial_expirations, social_expirations)

control_cumu <- control_expirations %>% summarize(mean(RL1), #this is ugly, but I can't figure out how to get a plottable cumulative sum otherwise
                                                  mean(RL2),
                                                  mean(RL3),
                                                  mean(RL4),
                                                  mean(RL5),
                                                  mean(RL6),
                                                  mean(RL7),
                                                  mean(RL8),
                                                  mean(RL9),
                                                  mean(RL10),
                                                  mean(RL11),
                                                  mean(RL12),
                                                  Scenario = 1
                                                  )

noSocial_cumu <- noSocial_expirations %>% summarize(mean(RL1), #this is ugly, but I can't figure out how to get a plottable cumulative sum otherwise
                                                    mean(RL2),
                                                    mean(RL3),
                                                    mean(RL4),
                                                    mean(RL5),
                                                    mean(RL6),
                                                    mean(RL7),
                                                    mean(RL8),
                                                    mean(RL9),
                                                    mean(RL10),
                                                    mean(RL11),
                                                    mean(RL12),
                                                    Scenario = 1
                                                    )
                                                  
social_cumu <-social_expirations %>% summarize(mean(RL1), #this is ugly, but I can't figure out how to get a plottable cumulative sum otherwise
                                               mean(RL2),
                                               mean(RL3),
                                               mean(RL4),
                                               mean(RL5),
                                               mean(RL6),
                                               mean(RL7),
                                               mean(RL8),
                                               mean(RL9),
                                               mean(RL10),
                                               mean(RL11),
                                               mean(RL12),
                                               Scenario = 1
                                               )

colnames(control_cumu) <- c("1","2","3","4","5","6","7","8","9","10","11","12", "Scenario")
colnames(noSocial_cumu)<- c("1","2","3","4","5","6","7","8","9","10","11","12", "Scenario")
colnames(social_cumu)  <- c("1","2","3","4","5","6","7","8","9","10","11","12", "Scenario")

cumu_total_expirations <- tibble(rbind(control_cumu, noSocial_cumu, social_cumu))

#Control Data Processing
c_plot <- tibble(cumsum(t(control_cumu[1:12]))) 
colnames(c_plot) <- c("cumsum")

c_conf_upper <- tibble(rows=c(t.test(control_expirations$RL1)$conf.int[2],
                              t.test(control_expirations$RL1+control_expirations$RL2)$conf.int[2],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3)$conf.int[2],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4)$conf.int[2],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5)$conf.int[2],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6)$conf.int[2],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7)$conf.int[2],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7+control_expirations$RL8)$conf.int[2],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7+control_expirations$RL8+control_expirations$RL9)$conf.int[2],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7+control_expirations$RL8+control_expirations$RL9+control_expirations$RL10)$conf.int[2],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7+control_expirations$RL8+control_expirations$RL9+control_expirations$RL10+control_expirations$RL11)$conf.int[2],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7+control_expirations$RL8+control_expirations$RL9+control_expirations$RL10+control_expirations$RL11+control_expirations$RL12)$conf.int[2]
                              )
                      )
colnames(c_conf_upper) <- c("upper")

c_conf_lower <- tibble(rows=c(t.test(control_expirations$RL1)$conf.int[1],
                              t.test(control_expirations$RL1+control_expirations$RL2)$conf.int[1],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3)$conf.int[1],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4)$conf.int[1],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5)$conf.int[1],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6)$conf.int[1],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7)$conf.int[1],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7+control_expirations$RL8)$conf.int[1],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7+control_expirations$RL8+control_expirations$RL9)$conf.int[1],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7+control_expirations$RL8+control_expirations$RL9+control_expirations$RL10)$conf.int[1],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7+control_expirations$RL8+control_expirations$RL9+control_expirations$RL10+control_expirations$RL11)$conf.int[1],
                              t.test(control_expirations$RL1+control_expirations$RL2+control_expirations$RL3+control_expirations$RL4+control_expirations$RL5+control_expirations$RL6+control_expirations$RL7+control_expirations$RL8+control_expirations$RL9+control_expirations$RL10+control_expirations$RL11+control_expirations$RL12)$conf.int[1]
                              )
                      )
colnames(c_conf_lower) <- c("lower")

c_plot <- cbind(c_plot, c_conf_upper, c_conf_lower)
colnames(c_plot) <- c("cumsum","upper","lower")

#noSocial Data Processing
n_plot <- tibble(cumsum(t(noSocial_cumu[1:12]))) 
colnames(n_plot) <- c("cumsum")

n_conf_upper <- tibble(rows=c(t.test(noSocial_expirations$RL1)$conf.int[2],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2)$conf.int[2],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3)$conf.int[2],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4)$conf.int[2],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5)$conf.int[2],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6)$conf.int[2],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7)$conf.int[2],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7+noSocial_expirations$RL8)$conf.int[2],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7+noSocial_expirations$RL8+noSocial_expirations$RL9)$conf.int[2],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7+noSocial_expirations$RL8+noSocial_expirations$RL9+noSocial_expirations$RL10)$conf.int[2],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7+noSocial_expirations$RL8+noSocial_expirations$RL9+noSocial_expirations$RL10+noSocial_expirations$RL11)$conf.int[2],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7+noSocial_expirations$RL8+noSocial_expirations$RL9+noSocial_expirations$RL10+noSocial_expirations$RL11+noSocial_expirations$RL12)$conf.int[2]
                              )
                        )
colnames(n_conf_upper) <- c("upper")
n_conf_lower <- tibble(rows=c(t.test(noSocial_expirations$RL1)$conf.int[1],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2)$conf.int[1],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3)$conf.int[1],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4)$conf.int[1],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5)$conf.int[1],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6)$conf.int[1],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7)$conf.int[1],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7+noSocial_expirations$RL8)$conf.int[1],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7+noSocial_expirations$RL8+noSocial_expirations$RL9)$conf.int[1],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7+noSocial_expirations$RL8+noSocial_expirations$RL9+noSocial_expirations$RL10)$conf.int[1],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7+noSocial_expirations$RL8+noSocial_expirations$RL9+noSocial_expirations$RL10+noSocial_expirations$RL11)$conf.int[1],
                              t.test(noSocial_expirations$RL1+noSocial_expirations$RL2+noSocial_expirations$RL3+noSocial_expirations$RL4+noSocial_expirations$RL5+noSocial_expirations$RL6+noSocial_expirations$RL7+noSocial_expirations$RL8+noSocial_expirations$RL9+noSocial_expirations$RL10+noSocial_expirations$RL11+noSocial_expirations$RL12)$conf.int[1]
                              )
                      )
colnames(n_conf_lower) <- c("lower")

n_plot <- cbind(n_plot, n_conf_upper, n_conf_lower)
colnames(n_plot) <- c("cumsum","upper","lower")

#social Data Processing
s_plot <- tibble(cumsum(t(social_cumu[1:12]))) 
colnames(s_plot) <- c("cumsum")

s_conf_upper <- tibble(rows=c(t.test(social_expirations$RL1)$conf.int[2],
                              t.test(social_expirations$RL1+social_expirations$RL2)$conf.int[2],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3)$conf.int[2],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4)$conf.int[2],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5)$conf.int[2],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6)$conf.int[2],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7)$conf.int[2],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7+social_expirations$RL8)$conf.int[2],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7+social_expirations$RL8+social_expirations$RL9)$conf.int[2],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7+social_expirations$RL8+social_expirations$RL9+social_expirations$RL10)$conf.int[2],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7+social_expirations$RL8+social_expirations$RL9+social_expirations$RL10+social_expirations$RL11)$conf.int[2],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7+social_expirations$RL8+social_expirations$RL9+social_expirations$RL10+social_expirations$RL11+social_expirations$RL12)$conf.int[2]
                              )
                      )
colnames(s_conf_upper) <- c("upper")
s_conf_lower <- tibble(rows=c(t.test(social_expirations$RL1)$conf.int[1],
                              t.test(social_expirations$RL1+social_expirations$RL2)$conf.int[1],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3)$conf.int[1],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4)$conf.int[1],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5)$conf.int[1],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6)$conf.int[1],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7)$conf.int[1],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7+social_expirations$RL8)$conf.int[1],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7+social_expirations$RL8+social_expirations$RL9)$conf.int[1],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7+social_expirations$RL8+social_expirations$RL9+social_expirations$RL10)$conf.int[1],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7+social_expirations$RL8+social_expirations$RL9+social_expirations$RL10+social_expirations$RL11)$conf.int[1],
                              t.test(social_expirations$RL1+social_expirations$RL2+social_expirations$RL3+social_expirations$RL4+social_expirations$RL5+social_expirations$RL6+social_expirations$RL7+social_expirations$RL8+social_expirations$RL9+social_expirations$RL10+social_expirations$RL11+social_expirations$RL12)$conf.int[1]
                              )
                       )
colnames(s_conf_lower) <- c("lower")

s_plot <- cbind(s_plot, s_conf_upper, s_conf_lower)
colnames(s_plot) <- c("cumsum","upper","lower")

#FINALLY plot the data with CI
ggplot()+ 
  geom_ribbon(aes(ymax=c_plot$upper, ymin=c_plot$lower, x=1:nrow(c_plot), alpha=0.003, fill=con))+
  geom_line(c_plot, mapping=aes(x=1:nrow(c_plot),y=cumsum, color=con))+
  geom_ribbon(aes(ymax=n_plot$upper, ymin=n_plot$lower, x=1:nrow(n_plot), alpha=0.003, fill=ins))+
  geom_line(n_plot, mapping=aes(x=1:nrow(n_plot),y=cumsum, color=ins))+
  geom_ribbon(aes(ymax=s_plot$upper, ymin=s_plot$lower, x=1:nrow(s_plot), alpha=0.003, fill=soc))+
  geom_line(s_plot, mapping=aes(x=1:nrow(s_plot),y=cumsum, color=soc))+
  ylab("Referrals Completed by Month x (%)")+
  xlab("Dignostic Referral Length (months)")+
  scale_x_continuous(breaks=1:12)+
  theme_bw()+
  ylim(c(40,90))+
  scale_color_manual("Scenario", 
                     breaks = c(con, ins, soc),
                     values = c('green','red','blue')
                   # limits = c(con, ins, soc)
                     )+
  scale_fill_manual(breaks = c(con, ins, soc),
                    values = c('green','red','blue'),
                    )+
  guides(alpha="none", fill="none")

#60-day completion rate means
c_plot[2,]$cumsum
n_plot[2,]$cumsum
s_plot[2,]$cumsum
#absolute increase over control
n_plot[2,]$cumsum-c_plot[2,]$cumsum
s_plot[2,]$cumsum-c_plot[2,]$cumsum
#relative increase over control
(n_plot[2,]$cumsum-c_plot[2,]$cumsum)/c_plot[2,]$cumsum *100
(s_plot[2,]$cumsum-c_plot[2,]$cumsum)/c_plot[2,]$cumsum *100

#Completion rates
c_plot[12,]$cumsum
n_plot[12,]$cumsum
s_plot[12,]$cumsum
#absolute increase over control
n_plot[12,]$cumsum-c_plot[12,]$cumsum
s_plot[12,]$cumsum-c_plot[12,]$cumsum
#relative increase over control
(n_plot[12,]$cumsum-c_plot[12,]$cumsum)/c_plot[12,]$cumsum *100
(s_plot[12,]$cumsum-c_plot[12,]$cumsum)/c_plot[12,]$cumsum *100


###############Navigated vs unnavigated
nun_con_expirations <- control_dt.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total_screening_referrals"=length(expired),
            "Navigated with NA"=0,#length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1& neighbor_navigated == 1))*100,
            "Navigated no NA"=0,#length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1 & neighbor_navigated == 0))*100,
            "Unnavigated with NA"=0,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0& neighbor_navigated == 1))*100,
            "Unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100
  )

nun_noSocial_expirations <- noSocial_intervention_dt.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total_screening_referrals"=length(expired),
            "Navigated with NA"=0,#length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1& neighbor_navigated == 1))*100,
            "Navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1 & neighbor_navigated == 0))*100,
            "Unnavigated with NA"=0,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0& neighbor_navigated == 1))*100,
            "Unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100
  )

nun_social_expirations <- intervention_dt.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total_screening_referrals"=length(expired),
            "Navigated with NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1& neighbor_navigated == 1))*100,
            "Navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1 & neighbor_navigated == 0))*100,
            "Unnavigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0& neighbor_navigated == 1))*100,
            "Unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100
  )

nun_con_expirations <-cbind(nun_con_expirations, "scenario"= "Control")
nun_noSocial_expirations <- cbind(nun_noSocial_expirations, "scenario"= "Institutional Only")
nun_social_expirations <- cbind(nun_social_expirations, "scenario"= "Institutional and Social")

# colnames(nun_con_expirations)     <- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_completion_rate", "unnavigated_completion_rate","Scenario")
# colnames(nun_noSocial_expirations)<- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_completion_rate", "unnavigated_completion_rate","Scenario")
# colnames(nun_social_expirations)  <- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_completion_rate", "unnavigated_completion_rate","Scenario")
colnames(nun_con_expirations)     <- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate","Scenario")
colnames(nun_noSocial_expirations)<- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate","Scenario")
colnames(nun_social_expirations)  <- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate","Scenario")


nun_total_expirations <- rbind(nun_con_expirations, nun_noSocial_expirations, nun_social_expirations)

#Unnavigated
d<-ggplot()+ 
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_completion_rate, fill = Scenario, color="Navigated"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_completion_rate, fill = Scenario, color="Unnavigated"))+
  scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
  ylab("Diagnostic Referral Completion Rate (%)")+
  theme_bw()+
  scale_fill_manual("Scenario", values = c("#999999", "#E69F00", "#56B4E9")
  )+
  scale_color_manual("Navigation Status",breaks = c("Navigated","Unnavigated"), values=c("black","brown3"))

d100 <- a<-ggplot()+ 
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_completion_rate, fill = Scenario, color="Navigated"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_completion_rate, fill = Scenario, color="Unnavigated"))+
  scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
  ylab("Diagnostic Referral Completion Rate (%)")+
  theme_bw()+
  scale_fill_manual("Scenario", values = c("#999999", "#E69F00", "#56B4E9")
  )+
  scale_color_manual("Navigation Status",breaks = c("Navigated","Unnavigated"), values=c("black","brown3"))


d <- ggplot()+ #TODO stratify
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_NA_completion_rate, fill = Scenario, color="Navigated with NA"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_no_NA_completion_rate, fill = Scenario, color="Navigated no NA"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_NA_completion_rate, fill = Scenario, color="Unnavigated with NA"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_no_NA_completion_rate, fill = Scenario, color="Unnavigated no NA"))+
  scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
  ylab("Diagnostic Referral Completion Rate (%)")+
  theme_bw()













plot_grid(ncol=2,nrow=1, d+ylim(0,100)+theme(legend.position="none"),d+ylab(""))

mean(nun_total_expirations[1:30,]$unnavigated_completion_rate)#control
mean(nun_total_expirations[31:60,]$unnavigated_completion_rate)#institutional
mean(nun_total_expirations[61:90,]$unnavigated_completion_rate)#social
#relative increases
(mean(nun_total_expirations[31:60,]$unnavigated_completion_rate)-mean(nun_total_expirations[1:30,]$unnavigated_completion_rate))/mean(nun_total_expirations[1:30,]$unnavigated_completion_rate)*100#institutional
(mean(nun_total_expirations[61:90,]$unnavigated_completion_rate)-mean(nun_total_expirations[1:30,]$unnavigated_completion_rate))/mean(nun_total_expirations[1:30,]$unnavigated_completion_rate)*100 #social


#Navigated
#Notch gives us an approximate 95% CI
ggplot(nun_total_expirations, aes(group=Scenario, y=navigated_completion_rate, fill = Scenario))+ 
  geom_boxplot()+
  geom_boxplot(aes(group=Scenario, y=unnavigated_completion_rate, fill = Scenario))+
  scale_x_continuous(breaks = c(-.18555, .18555),labels=c("Institutional Only", "Institutional and Social"))+
  ylab("Diagnostic Referral Completion Rate (%)")+
  theme_bw()+
  scale_fill_manual("Scenario", values = c("#E69F00", "#56B4E9"),
  )


mean(nun_total_expirations[1:30,]$navigated_completion_rate)#control
mean(nun_total_expirations[31:60,]$navigated_completion_rate)#institutional
mean(nun_total_expirations[61:90,]$navigated_completion_rate)#social

#relative increases
(mean(nun_total_expirations[31:60,]$navigated_completion_rate)-mean(nun_total_expirations[31:60,]$unnavigated_completion_rate))/mean(nun_total_expirations[31:60,]$unnavigated_completion_rate)*100#institutional
(mean(nun_total_expirations[61:90,]$navigated_completion_rate)-mean(nun_total_expirations[61:90,]$unnavigated_completion_rate))/mean(nun_total_expirations[61:90,]$unnavigated_completion_rate)*100 #social
