# Analyze calibration data
#Pulls data from .data files (written by demography-reset.R)
#Loads data into dataframes and generates plots 

# Libraries  ----------
rm(list=ls())

library(dplyr)
library(ggplot2)
library(network)
library(networkDynamic)

old_plots <- FALSE

## Add names of data directories here
#Directory name format is {date}_full_run
bc_navigation_root <- '/project2/khanna7/bryanb/bc-navigation/dec9_navlength/bc-navigation/'
date<-"11:29:57_2021-03-15"
#date <- '14:55:41_2021-03-12'
full_run_name <- paste0(date, '_full_run)/')

# Read data and set meta-parameters ----------
#setwd('/project2/khanna7/bryanb/bc-navigation/nov*/bc*')
getwd()
N <- 5000 #number of agents
n.instances <- 30 #number of runs
run_length=360 #number of time steps in run

control_list <- as.list(1:n.instances)
intervention_list <- as.list(1:n.instances)
noSocial_intervention_list <- as.list(1:n.instances)
control_dt_list <- as.list(1:n.instances)
intervention_dt_list <- as.list(1:n.instances)
noSocial_intervention_dt_list <- as.list(1:n.instances)

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
  "number.of.expired.diagnostic.referrals.at.t",#38
  "number.of.expired.screening.referrals.at.t" #39
)

dtest_columns <- c(
  "time",
  "diagnostic_referral_length",
  "expired",
  "within_2_months",
  "symptom_severity",
  "navigated",
  "screening_referral_length",
  "total_care_length",
  "instance"#,
  #"cancer_status"
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

#dtestdata handling
for (i in 1:n.instances){
  control_dt_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date, '_control/dtestdata/', i,".dtestdata"))
}
for (i in 1:n.instances){
  intervention_dt_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date,'_intervention/dtestdata/', i,".dtestdata"))
}
for (i in 1:n.instances){
  noSocial_intervention_dt_list[[i]] <- read.table(paste0(bc_navigation_root, date, '_full_run/', date, '_interventionNoSocial/dtestdata/', i,".dtestdata"))
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

colnames(control.df) <- dt_columns
colnames(intervention.df) <- dt_columns
colnames(noSocial_intervention.df) <- dt_columns

colnames(control_dt.df) <- dtest_columns
colnames(intervention_dt.df) <- dtest_columns
colnames(noSocial_intervention_dt.df) <- dtest_columns

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
            m_number.of.screening.referrals = mean(number.of.screening.referrals),
            m_number.of.dt.completed = mean(number.of.dt.completed),
            m_number.of.screen.completed = mean(number.of.screen.completed),
            m_number.of.hpos.agents = mean(number.of.hpos.agents),
            m_number.of.hneg.agents = mean(number.of.hneg.agents),
            m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
            m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
            m_nintros = mean(nintros),
            m_early.diagnosed.ratio = mean((number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases),
            m_late.diagnosed.ratio = mean((number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases)
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
            m_number.of.screening.referrals = mean(number.of.screening.referrals),
            m_number.of.dt.completed = mean(number.of.dt.completed),
            m_number.of.screen.completed = mean(number.of.screen.completed),
            m_number.of.hpos.agents = mean(number.of.hpos.agents),
            m_number.of.hneg.agents = mean(number.of.hneg.agents),
            m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
            m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
            m_nintros = mean(nintros),
            m_early.diagnosed.ratio = mean((number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases),
            m_late.diagnosed.ratio = mean((number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases)
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
            m_number.of.screening.referrals = mean(number.of.screening.referrals),
            m_number.of.dt.completed = mean(number.of.dt.completed),
            m_number.of.screen.completed = mean(number.of.screen.completed),
            m_number.of.hpos.agents = mean(number.of.hpos.agents),
            m_number.of.hneg.agents = mean(number.of.hneg.agents),
            m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
            m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
            m_nintros = mean(nintros),
            m_early.diagnosed.ratio = mean((number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases),
            m_late.diagnosed.ratio = mean((number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases)
  )

##Not sure how this mean_at_time thing will work
# control_dt.df_mean_at_time <- 
#   control_dt.df %>% 
#   group_by(time) %>%
#   summarise(
#     m_late.diagnosed.ratio = mean((number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases)
#   )
# 
# intervention_dt.df_mean_at_time <- 
#   intervention_dt.df %>% 
#   group_by(time) %>%
#   summarise(#m_navigation.length = mean(navigation.length),
#     m_time.until.diagnosis = median(time.until.diagnosis),
#  m_late.diagnosed.ratio = mean((number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases)
#   )
# 
# noSocial_intervention_dt.df_mean_at_time <- 
#   noSocial_intervention_dt.df %>% 
#   group_by(time) %>%
#   summarise(#m_navigation.length = mean(navigation.length),
#     m_time.until.diagnosis = median(time.until.diagnosis),
# )
  
# Plots ----------

#begin capturing output if you dont want to knit
#pdf(file='multiplot_out.pdf')


  #Length of navigation (FAILED because demography-reset.R will break if I output nav.length because it is a vector of length 5000 and everything else is a single number)
  #Analysis has been done manually collecting nav lengths from net.f and putting that through hist()

#Time from referral to testing (THIS WILL REQUIRE SUBSTANTIAL WORK: MODEL NEEDS TO RECORD TIME OF REFERRAL)
#Notes: color = "" must be inside aes(). Colors assigned in scale_color_manual use LIMITS not LABELS.
con<-"control"
ins<-"Institutional without social"
soc<-"Institutional with social"


##New .dtestdata Plots 
#Compare navigated vs non-navigated time of care
navigated_completions <- filter(intervention_dt.df, navigated == 1)
unnavigated_completions <- filter(intervention_dt.df, navigated == 0)


ggplot()+
  geom_histogram(data=unnavigated_completions, aes(x=total_care_length), bins = 13)

ggplot()+
  geom_histogram(data=navigated_completions, aes(x=total_care_length), bins = 13)

#Compare navigated vs. non-navigated diagnostic test referral to completion time

ggplot()+
  geom_histogram(data=control_dt.df, aes(x=diagnostic_referral_length, color = con), fill = 'none')+
  geom_histogram(data=noSocial_intervention_dt.df, aes(x=diagnostic_referral_length, color = ins), fill = 'none')+
  geom_histogram(data=intervention_dt.df, aes(x=diagnostic_referral_length, color = soc), fill = 'none')+
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                   values = c('green','red','blue'))

#By Percent of completions
control_tbl<-control_dt.df %>% group_by(diagnostic_referral_length)
noSocial_tbl<-noSocial_intervention_dt.df %>% group_by(diagnostic_referral_length)
social_tbl<-intervention_dt.df %>% group_by(diagnostic_referral_length)

control_pct <- table(group_indices(control_tbl))/length(control_tbl$time)
noSocial_pct <- table(group_indices(noSocial_tbl))/length(noSocial_tbl$time)
social_pct <- table(group_indices(social_tbl))/length(social_tbl$time)

#Cumulative sum
plot(cumsum(control_pct), type='b')
plot(cumsum(noSocial_pct), type='b')
plot(cumsum(social_pct), type='b')

#ggplot version of cumulative percent
ggplot()+
  geom_line(data=data.frame(cumsum(control_pct)), aes(x=1:13,y=cumsum.control_pct.), color='green')+
  ylab("Cumulative Percent")+
  xlab("Diagnostic Referral Length")+
  scale_x_continuous(breaks=1:13)+
  theme_bw()+
  ylim(c(0,1))

ggplot()+
  geom_line(data=data.frame(cumsum(noSocial_pct)), aes(x=1:13,y=cumsum.noSocial_pct.), color='red')+
  ylab("Cumulative Percent")+
  xlab("Diagnostic Referral Length")+
  scale_x_continuous(breaks=1:13)+
  theme_bw()+
  ylim(c(0,1))

ggplot()+
  geom_line(data=data.frame(cumsum(social_pct)), aes(x=1:13,y=cumsum.social_pct.),color='blue')+
  ylab("Cumulative Percent")+
  xlab("Diagnostic Referral Length")+
  scale_x_continuous(breaks=1:13)+
  theme_bw()+
  ylim(c(0,1))

ggplot()+
  geom_line(data=data.frame(cumsum(social_pct)), aes(x=1:13,y=cumsum.social_pct.),color='blue')+
  geom_line(data=data.frame(cumsum(noSocial_pct)), aes(x=1:13,y=cumsum.noSocial_pct.), color='red')+
  geom_line(data=data.frame(cumsum(control_pct)), aes(x=1:13,y=cumsum.control_pct.), color='green')+
  ylab("Cumulative Percent")+
  xlab("Diagnostic Referral Length")+
  scale_x_continuous(breaks=1:13)+
  theme_bw()+
  ylim(c(0,1))

#Histogram of completions by diagnostic referral length
ggplot()+
  geom_histogram(data=control_dt.df, aes(x=diagnostic_referral_length, color = con), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=noSocial_intervention_dt.df, aes(x=diagnostic_referral_length, color = ins), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=intervention_dt.df, aes(x=diagnostic_referral_length, color = soc), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  ylab("Frequency")+
  theme_bw()+
  scale_x_continuous(name="Referral Length", breaks=-1:13, labels=-1:13)+ #No X-axis name or Labels for some reason??
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))

##Compare navigated vs. Non-navigated

#Length of diagnostic referral
ggplot()+
  #geom_histogram(data=control_dt.df, aes(x=diagnostic_referral_length, color = con), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=filter(intervention_dt.df, navigated==1), aes(x=diagnostic_referral_length, color = 'navigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=filter(intervention_dt.df, navigated==0), aes(x=diagnostic_referral_length, color = 'unnavigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  ylab("Frequency")+
  #ylim(0,31000)+
  theme_bw()+
  scale_x_continuous(name="Referral Length", breaks=-1:13, labels=-1:13)+ #No X-axis name or Labels for some reason??
  scale_color_manual("Social + Institutional", limits = c("navigated", "unnavigated"),
                     values = c('blue', 'green'))
ggplot()+
  #geom_histogram(data=control_dt.df, aes(x=diagnostic_referral_length, color = con), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=filter(noSocial_intervention_dt.df, navigated==1), aes(x=diagnostic_referral_length, color = 'navigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=filter(noSocial_intervention_dt.df, navigated==0), aes(x=diagnostic_referral_length, color = 'unnavigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  ylab("Frequency")+
  ylim(0,31000)+
  theme_bw()+
  scale_x_continuous(name="Referral Length", breaks=-1:13, labels=-1:13)+ #No X-axis name or Labels for some reason??
  scale_color_manual("Institutional Only", limits = c("navigated", "unnavigated"),
                     values = c('blue', 'green'))
#Plotting diagnostic length as by percent




#Symptom Severity at Logging
#SS social nav/nonnav
ggplot()+
  #geom_histogram(data=control_dt.df, aes(x=diagnostic_referral_length, color = con), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=filter(intervention_dt.df, navigated==1 ), aes(x=symptom_severity, color = 'navigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=filter(intervention_dt.df, navigated==0 ), aes(x=symptom_severity, color = 'unnavigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  ylab("Frequency")+
  theme_bw()+
  scale_x_continuous(name="Symptom Severity at Logging", breaks=-1:13, labels=-1:13)+ #No X-axis name or Labels for some reason??
  scale_color_manual("Social + Institutional", limits = c("navigated", "unnavigated"),
                     values = c('blue', 'green'))
#SS institutional nav/nonnav
ggplot()+
  #geom_histogram(data=control_dt.df, aes(x=diagnostic_referral_length, color = con), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=filter(noSocial_intervention_dt.df, navigated==1 ), aes(x=symptom_severity, color = 'navigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=filter(noSocial_intervention_dt.df, navigated==0 ), aes(x=symptom_severity, color = 'unnavigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  ylab("Frequency")+
  theme_bw()+
  scale_x_continuous(name="Symptom Severity at Logging", breaks=-1:13, labels=-1:13)+ #No X-axis name or Labels for some reason??
  scale_color_manual("Institutional Only", limits = c("navigated", "unnavigated"),
                     values = c('blue', 'green'))

#Plotting stage at logging by percent
n_total_referral_ends_control <- length(control_dt.df$time)
n_total_referral_ends_institutional <- length(noSocial_intervention_dt.df$time)
n_total_referral_ends_social <- length(intervention_dt.df$time)

nav_referral_ends_control <- filter(control_dt.df, navigated==1) #Should be 0 but isnt???
nav_referral_ends_institutional <- filter(noSocial_intervention_dt.df, navigated==1)
nav_referral_ends_social <- filter(intervention_dt.df, navigated==1)

unnav_referral_ends_control <- filter(control_dt.df, navigated==0)
unnav_referral_ends_institutional <- filter(noSocial_intervention_dt.df, navigated==0)
unnav_referral_ends_social <- filter(intervention_dt.df, navigated==0)

#SS Breakdowns
control_SS_nav  <-table(nav_referral_ends_control$symptom_severity)
control_SS_unnav<-table(unnav_referral_ends_control$symptom_severity)
control_SS_total<-table(control_dt.df$symptom_severity)

institutional_SS_nav  <-table(nav_referral_ends_institutional$symptom_severity)
institutional_SS_unnav<-table(unnav_referral_ends_institutional$symptom_severity)
institutional_SS_total<-table(noSocial_intervention_dt.df$symptom_severity)

social_SS_nav  <-table(nav_referral_ends_social$symptom_severity)
social_SS_unnav<-table(unnav_referral_ends_social$symptom_severity)
social_SS_total<-table(intervention_dt.df$symptom_severity)

#SS Breakdown by percent
p_control_SS_nav<-control_SS_nav/sum(control_SS_nav)*100
p_control_SS_unnav<-control_SS_unnav/sum(control_SS_unnav)*100
p_control_SS_total<-control_SS_total/sum(control_SS_total)*100

p_institutional_SS_nav  <-institutional_SS_nav/sum(institutional_SS_nav)*100
p_institutional_SS_unnav<-institutional_SS_unnav/sum(institutional_SS_unnav)*100
p_institutional_SS_total<-institutional_SS_total/sum(institutional_SS_total)*100

p_social_SS_nav  <-social_SS_nav/sum(social_SS_nav)*100
p_social_SS_unnav<-social_SS_unnav/sum(social_SS_unnav)*100
p_social_SS_total<- social_SS_total/sum(social_SS_total)*100

#Plotting symptom severity percentage breakdown

ggplot()+
  theme_bw()+
  geom_bar(stat="identity",data=data.frame(p_control_SS_nav), aes(x=Var1,y=Freq), color="blue", fill='none')+
  geom_bar(stat="identity",data=data.frame(p_control_SS_unnav), aes(x=Var1,y=Freq), color="green", fill='none')+
  ylab("Percent")+
  xlab("Symptom Severity Level/Cancer Stage")

ggplot()+
  theme_bw()+
  geom_bar(stat="identity",data=data.frame(p_institutional_SS_nav), aes(x=Var1,y=Freq), color="blue", fill='none')+
  geom_bar(stat="identity",data=data.frame(p_institutional_SS_unnav), aes(x=Var1,y=Freq), color="green", fill='none')+
  ylab("Percent")+
  xlab("Symptom Severity Level/Cancer Stage")

ggplot()+
  theme_bw()+
  geom_bar(stat="identity",data=data.frame(p_social_SS_nav), aes(x=Var1,y=Freq), color="blue", fill='none')+
  geom_bar(stat="identity",data=data.frame(p_social_SS_unnav), aes(x=Var1,y=Freq), color="green", fill='none')+
  ylab("Percent")+
  xlab("Symptom Severity Level/Cancer Stage")

#Output for symptom severity table
p_control_SS_nav
p_control_SS_unnav
p_control_SS_total

p_institutional_SS_nav
p_institutional_SS_unnav
p_institutional_SS_total

p_social_SS_nav
p_social_SS_unnav
p_social_SS_total

#Difference:
p_control_SS_nav[1:4] - p_control_SS_unnav[1:4] #Doesn't work because the navigated array is shorter
p_institutional_SS_nav - p_institutional_SS_unnav
p_social_SS_nav - p_social_SS_unnav


#length(which(nav_referral_ends_control$navigated==1))
#length(filter(intervention_dt.df, navigated==1 & symptom_severity==0)$time)/length(filter(intervention_dt.df,navigated==1)$time)

#Managing data: Dividing event log into instances

#Cumulative percent (nav vs. unnav)

#control_tbl<-control_dt.df %>% group_by(diagnostic_referral_length)
#noSocial_tbl<-noSocial_intervention_dt.df %>% group_by(diagnostic_referral_length)
social_nav_tbl<-filter(intervention_dt.df, navigated==1) %>% group_by(diagnostic_referral_length)
social_unnav_tbl<-filter(intervention_dt.df, navigated==1) %>% group_by(diagnostic_referral_length)


#control_pct <- table(group_indices(control_tbl))/length(control_tbl$time)
#noSocial_pct <- table(group_indices(noSocial_tbl))/length(noSocial_tbl$time)
social_nav_pct <- table(group_indices(social_nav_tbl))/length(social_nav_tbl$time)
social_unnav_pct <- table(group_indices(social_unnav_tbl))/length(social_unnav_tbl$time)

#Cumulative sum
plot(cumsum(control_pct), type='b')
plot(cumsum(noSocial_pct), type='b')
plot(cumsum(social_pct), type='b')



ggplot()+
  geom_line(data=data.frame(cumsum(social_nav_pct)), aes(x=1:13,y=cumsum.social_nav_pct.),color='blue')+
  geom_line(data=data.frame(cumsum(social_unnav_pct)), aes(x=1:13,y=cumsum.social_unnav_pct.),color='green')+
  ylab("Cumulative Percent")+
  xlab("Diagnostic Referral Length")+
  scale_x_continuous(breaks=1:13)+
  theme_bw()+
  ylim(c(0,1))


##Old .data plots
if(old_plots == TRUE){
#GOAL: Percent dt referrals completed/ total number of completions. i.e. "73% of completions were in month 1-2"

ggplot()+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.screening.referrals,color=con))+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.screening.referrals,color=con))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.screen.completed/m_number.of.screening.referrals,color=ins))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.screening.referrals, color=ins))+
  geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.screening.referrals, color=soc))+
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.screening.referrals, color=soc))+
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue')
  )#+
  #ylim(c(0,2))
  
  #Screening completions/dt completions
  ggplot()+
    theme_bw()+
    geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.dt.completed,color=con))+
    geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.dt.completed,color=con))+
    geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.screen.completed/m_number.of.dt.completed,color=ins))+
    geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.dt.completed, color=ins))+
    geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.dt.completed, color=soc))+
    geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.dt.completed, color=soc))+
    scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                       values = c('green','red','blue')
    )

  #Screening completion rate: screen complete/screening referral
  ggplot()+
    theme_bw()+
    geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.screening.referrals,color=con))+
    #geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.screening.referrals,color=con))+
    geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.screen.completed/m_number.of.screening.referrals,color=ins))+
    #geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.dt.completed, color=ins))+
    geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.screening.referrals, color=soc))+
    #geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.dt.completed, color=soc))+
    scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                       values = c('green','red','blue')
    )
  

#Screening completions/Screening Referrals (screening referral completion rate)
ggplot()+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.screening.referrals,color=con))+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.screening.referrals,color=con))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.screen.completed/m_number.of.screening.referrals,color=ins))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.screening.referrals, color=ins))+
  geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.screening.referrals, color=soc))+
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.screen.completed/number.of.screening.referrals, color=soc))+
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue')
  )#+
  #ylim(c(0,1))
#1 instance
ggplot()+
  theme_bw()+
  #geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.screening.referrals,color=con))+
  geom_line(data=control.df[1:360,], aes(x=time, y=number.of.screen.completed/number.of.screening.referrals,color=con))+
  #geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.screen.completed/m_number.of.screening.referrals,color=ins))+
  geom_line(data=noSocial_intervention.df[1:360,], aes(x=time, y=number.of.screen.completed/number.of.screening.referrals, color=ins))+
  #geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.screening.referrals, color=soc))+
  geom_line(data=intervention.df[1:360,], aes(x=time, y=number.of.screen.completed/number.of.screening.referrals, color=soc))+
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))
#2 instance
ggplot()+
  theme_bw()+
  #geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.screening.referrals,color=con))+
  geom_line(data=control.df[360:720,], aes(x=time, y=number.of.screen.completed/number.of.screening.referrals,color=con))+
  #geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.screen.completed/m_number.of.screening.referrals,color=ins))+
  geom_line(data=noSocial_intervention.df[360:720,], aes(x=time, y=number.of.screen.completed/number.of.screening.referrals, color=ins))+
  #geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed/m_number.of.screening.referrals, color=soc))+
  geom_line(data=intervention.df[360:720,], aes(x=time, y=number.of.screen.completed/number.of.screening.referrals, color=soc))+
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))

#Diagnostic completions/diagnostic referrals (diagnostic referral completion rate)
ggplot()+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.dt.completed/m_number.of.diagnostic.referrals.at.t,color=con))+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.diagnostic.referrals.at.t,color=con))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.dt.completed/m_number.of.diagnostic.referrals.at.t,color=ins))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.diagnostic.referrals.at.t, color=ins))+
  geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.dt.completed/m_number.of.diagnostic.referrals.at.t, color=soc))+
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.diagnostic.referrals.at.t, color=soc))+
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue')
  )#+
 # ylim(c(0,2))
ggplot()+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.dt.completed/m_number.of.diagnostic.referrals.at.t,color=con))+
  #geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.diagnostic.referrals.at.t,color=con))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.dt.completed/m_number.of.diagnostic.referrals.at.t,color=ins))+
  #geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.diagnostic.referrals.at.t, color=ins))+
  geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.dt.completed/m_number.of.diagnostic.referrals.at.t, color=soc))+
  #geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.diagnostic.referrals.at.t, color=soc))+
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue')
  )+
  ylim(c(0,2)) #ylim cuts off huge spike around t=1

#Agents diagnosed/Agents with cancer (Diagnosis rate)
ggplot()+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.diagnosed.cases/m_number.of.positive.bc.agents,color=con))+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.diagnosed.cases/number.of.positive.bc.agents,color=con))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.diagnosed.cases/m_number.of.positive.bc.agents,color=ins))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.diagnosed.cases/number.of.positive.bc.agents, color=ins))+
  geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.diagnosed.cases/m_number.of.positive.bc.agents, color=soc))+
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.diagnosed.cases/number.of.positive.bc.agents, color=soc))+
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue')
  )
  mean(control.df_mean_at_time$m_number.of.diagnosed.cases/control.df_mean_at_time$m_number.of.positive.bc.agents)
  mean(noSocial_intervention.df_mean_at_time$m_number.of.diagnosed.cases/noSocial_intervention.df_mean_at_time$m_number.of.positive.bc.agents)
  mean(intervention.df_mean_at_time$m_number.of.diagnosed.cases/intervention.df_mean_at_time$m_number.of.positive.bc.agents)
  
    
#Stage at diagnosis (Early = 0-1, late = 2-3) (navigated vs unnavigated?)
  #Early
  ggplot()+
    theme_bw()+
    geom_line(data = control.df_mean_at_time, aes(x=time, y=m_early.diagnosed.ratio,color=con))+
    geom_line(data=control.df, alpha=0.1, aes(x=time, y=(number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases,color=con))+
    geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_early.diagnosed.ratio,color=ins))+
    geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=(number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases, color=ins))+
    geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_early.diagnosed.ratio, color=soc))+
    geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=(number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases, color=soc))+
    scale_color_manual("Scenario", values = c('green','red','blue'),
                       limits= c("control", "Institutional without social", "Institutional with social")
    )+
    ylim(c(0,1))
  #Late
  ggplot()+
    theme_bw()+
    geom_line(data = control.df_mean_at_time, aes(x=time, y=m_late.diagnosed.ratio,color=con))+
    geom_line(data=control.df, alpha=0.1, aes(x=time, y=(number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases,color=con))+
    geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_late.diagnosed.ratio,color=ins))+
    geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=(number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases, color=ins))+
    geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_late.diagnosed.ratio, color=soc))+
    geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=(number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases, color=soc))+
    scale_color_manual("Scenario", values = c('green','red','blue'),
                       limits= c("control", "Institutional without social", "Institutional with social")
    )+
    ylim(c(0,1))
  #Effects ~3% between control and social (for feb 16 meeting)
  mean(control.df_mean_at_time$m_late.diagnosed.ratio)
  #0.8906187
  mean(noSocial_intervention.df_mean_at_time$m_late.diagnosed.ratio)
  #0.8790043
  mean(intervention.df_mean_at_time$m_late.diagnosed.ratio)
  #0.8597492
  
  
#Proportion of agents diagnosed within 60 day "golden period" (compare navigated and non-navigated)
  #NOTE: this depends on time.until.diagnosis being correct
  #As of Feb 16, 2021 we see that time.until.diagnosis ranges from 17-30, with an avg of ~23
  ggplot()+
    theme_bw()+
    #geom_line(data = control.df_mean_at_time, aes(x=time, y=length(which(m_time.until.diagnosis <= 2)),color=con))+
    geom_line(data=control.df, alpha=0.1, aes(x=time, y=length(which(time.until.diagnosis <= 2)),color=con))+
    #geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=length(which(m_time.until.diagnosis <= 2)),color=ins))+
    geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=length(which(time.until.diagnosis <= 2)), color=ins))+
    #geom_line(data = intervention.df_mean_at_time, aes(x=time, y=length(which(m_time.until.diagnosis <= 2)), color=soc))+
    geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=length(which(time.until.diagnosis <= 2)), color=soc))+
    scale_color_manual("Scenario", values = c('green','red','blue'),
                       limits= c("control", "Institutional without social", "Institutional with social")
    )
    #ylim(c(0,1))
  
  #NOTE (feb 17 meeting):
  #NO agents are diagnosed in less than 60 days
  #> length(which(control.df$time.until.diagnosis <= 2))
  #[1] 0
  #> length(which(intervention.df$time.until.diagnosis <= 2))
  #[1] 0
  #> length(which(noSocial_intervention.df$time.until.diagnosis <= 2))
  #[1] 0
  
  #In fact, the fastest diagnosis takes 16 months...
  #> min(noSocial_intervention.df$time.until.diagnosis)
  #[1] 16
  
#TODO
#Referral to diagnosis time
  
  
#Time to diagnosis
ggplot()+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_time.until.diagnosis,color=con))+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=time.until.diagnosis,color=con))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_time.until.diagnosis,color=ins))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=time.until.diagnosis, color=ins))+
  geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_time.until.diagnosis, color=soc))+
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=time.until.diagnosis, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits= c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(0,30))

#Total number of navigated
ggplot()+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.navigated.agents,color=con))+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.navigated.agents, color=con))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.navigated.agents,color=ins))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.navigated.agents,color=ins))+
  geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.navigated.agents, color=soc))+
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.navigated.agents, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c(con, ins, soc)
  )+
  ylim(c(0,300))
#number.of.positive.bc.agents
ggplot()+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.positive.bc.agents,color="control"))+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.positive.bc.agents,color=con))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.positive.bc.agents,color="Institutional without social"))+
  geom_line(data=noSocial_intervention.df,alpha=0.1, aes(x=time, y=number.of.positive.bc.agents,color=ins))+
  geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.positive.bc.agents, color="Institutional with social"))+
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.positive.bc.agents, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(50, 140))
#(number.of.positive.bc.agents/N)*100
#Breast Cancer Prevalence (%)
ggplot()+
  theme_bw()+
  geom_line(data=control.df, alpha=0.1,aes(x=time, y=((number.of.positive.bc.agents/N)*100),color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=((m_number.of.positive.bc.agents/N)*100),color=con))+
  geom_line(data=noSocial_intervention.df, alpha=0.1,aes(x=time, y=((number.of.positive.bc.agents/N)*100),color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time, y=((m_number.of.positive.bc.agents/N)*100),color=ins))+
  geom_line(data=intervention.df, alpha=0.1,aes(x=time, y=((number.of.positive.bc.agents/N)*100), color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=((m_number.of.positive.bc.agents/N)*100), color=soc))+
  labs(y="Breast Cancer Prevalence (%)")+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits= c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(1,3))
#number.of.screening.visits.at.t
ggplot()+  
  theme_bw()+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.screening.visits.at.t,color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=m_number.of.screening.visits.at.t,color=con))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.screening.visits.at.t,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.screening.visits.at.t,color=ins))+  
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.screening.visits.at.t, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=m_number.of.screening.visits.at.t, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(0,125))
#number.of.diagnostic.referrals.at.t
ggplot()+
  theme_bw()+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t,color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=m_number.of.diagnostic.referrals.at.t,color=con))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.diagnostic.referrals.at.t,color=ins))+  
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=m_number.of.diagnostic.referrals.at.t, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(0,20))
#number.of.screening.referrals
ggplot()+
  theme_bw()+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.screening.referrals,color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=m_number.of.screening.referrals,color=con))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.screening.referrals,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.screening.referrals,color=ins))+  
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.screening.referrals, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=m_number.of.screening.referrals, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )
  #ylim(c(700,1300)) #omits data that does not conform
#number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t
ggplot()+
  theme_bw()+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t, color = con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=m_number.of.diagnostic.referrals.at.t/m_number.of.screening.visits.at.t, color = con))+
  geom_line(data=noSocial_intervention.df,alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.diagnostic.referrals.at.t/m_number.of.screening.visits.at.t,color=ins))+  
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=m_number.of.diagnostic.referrals.at.t/m_number.of.screening.visits.at.t, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(0,0.5))

#number.of.dt.completed/number.of.screen.completed
ggplot()+
  theme_bw()+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.screen.completed,color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=m_number.of.dt.completed/m_number.of.screen.completed,color=con))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.screen.completed,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.dt.completed/m_number.of.screen.completed,color=ins))+  
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.dt.completed/number.of.screen.completed, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=m_number.of.dt.completed/m_number.of.screen.completed, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(0,0.2))
#number.of.screen.completed
ggplot()+
  theme_bw()+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.screen.completed,color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed,color=con))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.screen.completed,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed,color=ins))+  
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.screen.completed, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=m_number.of.screen.completed, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(25,125))
#number.of.dt.completed
ggplot()+
  theme_bw()+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.dt.completed,color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=m_number.of.dt.completed,color=con))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.dt.completed,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.dt.completed,color=ins))+  
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.dt.completed, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=m_number.of.dt.completed, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(0,15))
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
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.hneg.agents/number.of.hpos.agents,color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=m_number.of.hneg.agents/m_number.of.hpos.agents,color=con))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.hneg.agents/number.of.hpos.agents,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.hneg.agents/m_number.of.hpos.agents,color=ins))+  
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.hneg.agents/number.of.hpos.agents, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=m_number.of.hneg.agents/m_number.of.hpos.agents, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(0,1))
#number.of.diagnosed.cases
ggplot()+
  theme_bw()+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.diagnosed.cases,color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=m_number.of.diagnosed.cases,color=con))+
  geom_line(data=noSocial_intervention.df,alpha=0.1, aes(x=time, y=number.of.diagnosed.cases,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.diagnosed.cases,color=ins))+  
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.diagnosed.cases, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=m_number.of.diagnosed.cases, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(20,60))
#number.of.positive.bc.agents
ggplot()+
  theme_bw()+
  geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.positive.bc.agents,color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=m_number.of.positive.bc.agents,color=con))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.positive.bc.agents,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.positive.bc.agents,color=ins))+  
  geom_line(data=intervention.df,alpha=0.1, aes(x=time, y=number.of.positive.bc.agents, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=m_number.of.positive.bc.agents, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(70,140))
#nintros
ggplot()+
  theme_bw()+
  geom_line(data=control.df,alpha=0.1, aes(x=time, y=nintros,color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time, y=m_nintros,color=con))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=nintros,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time,aes(x=time, y=m_nintros,color=ins))+  
  geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=nintros, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time, y=m_nintros, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("control", "Institutional without social", "Institutional with social")
  )+
  ylim(c(40,110))
}
#end capturing plots if not using knit  
#dev.off()

