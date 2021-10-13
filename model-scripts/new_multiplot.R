# Analyze calibration data
#Pulls data from .data files (written by demography-reset.R)
#Loads data into dataframes and generates plots 

# Libraries  ----------
rm(list=ls())

library(dplyr)
library(ggplot2)
library(network)
library(networkDynamic)

new_plots <- TRUE
old_plots <- TRUE

## Add names of data directories here
#Directory name format is {date}_full_run
bc_navigation_root <- '/project2/khanna7/bryanb/bc-navigation/dec9_navlength/bc-navigation/'
#date <- "13:41:19_2021-04-06"#pre social nav update
#date<-"12:52:28_2021-04-28" #2nd round (?)
date<-"14:39:00_2021-04-28" #3rd round (reset neighbor_navigation_roll at each navigation end point)
full_run_name <- paste0(date, '_full_run)/')

# Read data and set meta-parameters ----------
N<-5000 #number of agents
n.instances<-30 #number of runs
run_length<-100 #number of time steps in run

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

# Not Sure how to implement this part
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

##########################BEGIN PLOTS#############################################################################
con<-"control"
ins<-"Institutional without social"
soc<-"Institutional with social"

#Number navigated at time step
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
  )

#cumulative sum of total navigated
ggplot()+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time, y=cumsum(m_number.of.navigated.agents),color=con))+
  #geom_line(data=control.df, alpha=0.1, aes(x=time, y=cumsum(number.of.navigated.agents), color=con))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=cumsum(m_number.of.navigated.agents),color=ins))+
  #geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=cumsum(number.of.navigated.agents),color=ins))+
  geom_line(data = intervention.df_mean_at_time, aes(x=time, y=cumsum(m_number.of.navigated.agents), color=soc))+
  #geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=cumsum(number.of.navigated.agents), color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c(con, ins, soc)
  )

#Alternative count of screening expirations (via diagnostic event logs)
#control_sc.df <- tibble(control_sc.df)

#a <- group_by(control_sc.df, "expired")










########Referral Lengths###########################################################################################

 #Cumulative Completion Percentage by Length
control_tbl<-control_dt.df %>% group_by(diagnostic_referral_length)
noSocial_tbl<-noSocial_intervention_dt.df %>% group_by(diagnostic_referral_length)
social_tbl<-intervention_dt.df %>% group_by(diagnostic_referral_length)

control_pct <- table(group_indices(control_tbl))/length(control_tbl$time)
noSocial_pct <- table(group_indices(noSocial_tbl))/length(noSocial_tbl$time)
social_pct <- table(group_indices(social_tbl))/length(social_tbl$time)

#Shift each index over to start at 1 (since the previous 1 was actually expirations)
for(i in 2:13){control_pct[i-1] <- control_pct[i]
}
control_pct <- control_pct[1:12]
for(i in 2:13){noSocial_pct[i-1] <- noSocial_pct[i]
}
noSocial_pct <- noSocial_pct[1:12]
for(i in 2:13){social_pct[i-1] <- social_pct[i]
}
social_pct <- social_pct[1:12]

ggplot()+
  geom_line(data=data.frame(cumsum(social_pct)), aes(x=1:12,y=cumsum.social_pct.),color='blue')+
  geom_line(data=data.frame(cumsum(noSocial_pct)), aes(x=1:12,y=cumsum.noSocial_pct.), color='red')+
  geom_line(data=data.frame(cumsum(control_pct)), aes(x=1:12,y=cumsum.control_pct.), color='green')+
  ylab("Cumulative Percent")+
  xlab("Diagnostic Referral Length")+
  scale_x_continuous(breaks=1:12)+
  theme_bw()+
  ylim(c(0,1))

#Diagnostic test referral completion length by percentage
control_tbl<-control_dt.df %>% group_by(diagnostic_referral_length)
noSocial_tbl<-noSocial_intervention_dt.df %>% group_by(diagnostic_referral_length)
social_tbl<-intervention_dt.df %>% group_by(diagnostic_referral_length)

control_pct <- table(group_indices(control_tbl))/length(control_tbl$time)
noSocial_pct <- table(group_indices(noSocial_tbl))/length(noSocial_tbl$time)
social_pct <- table(group_indices(social_tbl))/length(social_tbl$time)

#Shift each index over to start at 0 (setting 0 to mean 'expired')
temp <- control_pct[1]
for(i in 1:13){control_pct[i-1] <- control_pct[i]
}
control_pct[13] <- temp
temp <- noSocial_pct[1]
for(i in 1:13){noSocial_pct[i-1] <- noSocial_pct[i]
}
noSocial_pct[13] <- temp
temp <- social_pct[1]
for(i in 1:13){social_pct[i-1] <- social_pct[i]
}
social_pct[13] <- temp

#Plotting the by percentage (NON cumulative) breakdown
ggplot()+
  geom_point(data=data.frame(social_pct), aes(x=Var1,y=Freq,color=soc))+
  geom_point(data=data.frame(noSocial_pct), aes(x=Var1,y=Freq, color=ins))+
  geom_point(data=data.frame(control_pct), aes(x=Var1,y=Freq, color=con))+
  ylab("Cumulative Percent")+
  xlab("Diagnostic Referral Length")+
  #scale_x_continuous(breaks=1:12)+
  theme_bw()+  
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                                  values = c('green','red','blue'))+
  ylim(c(0,1))

##Diagnostic
ggplot()+
  geom_histogram(data=control_dt.df, aes(x=diagnostic_referral_length, color = con), fill = 'none', bins=14)+
  geom_histogram(data=noSocial_intervention_dt.df, aes(x=diagnostic_referral_length, color = ins), fill = 'none',bins=14)+
  geom_histogram(data=intervention_dt.df, aes(x=diagnostic_referral_length, color = soc), fill = 'none',bins=14)+
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))

#Screening completion length by percentage
control_tbl<-control_sc.df %>% group_by(screening_referral_length)
noSocial_tbl<-noSocial_intervention_sc.df %>% group_by(screening_referral_length)
social_tbl<-intervention_sc.df %>% group_by(screening_referral_length)

control_pct <- table(group_indices(control_tbl))/length(control_tbl$time)
noSocial_pct <- table(group_indices(noSocial_tbl))/length(noSocial_tbl$time)
social_pct <- table(group_indices(social_tbl))/length(social_tbl$time)

#Shift each index over because [1] is actually expiration % (switch so that [13] contains 'expired')
temp <- control_pct[1]
for(i in 1:13){control_pct[i-1] <- control_pct[i]
}
control_pct[13] <- temp
temp <- noSocial_pct[1]
for(i in 1:13){noSocial_pct[i-1] <- noSocial_pct[i]
}
noSocial_pct[13] <- temp
temp <- social_pct[1]
for(i in 1:13){social_pct[i-1] <- social_pct[i]
}
social_pct[13] <- temp

#Plotting Screening referral length by percentage (NON cumulative) breakdown
ggplot()+
  geom_point(data=data.frame(social_pct), aes(x=Var1,y=Freq,color=soc))+
  geom_point(data=data.frame(noSocial_pct), aes(x=Var1,y=Freq, color=ins))+
  geom_point(data=data.frame(control_pct), aes(x=Var1,y=Freq, color=con))+
  ylab("Percent")+
  xlab("Screening Referral Length")+
  #scale_x_continuous(breaks=1:12)+
  theme_bw()+  
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))+
  ylim(c(0,1))

ggplot()+
  geom_line(data=data.frame(cumsum(social_pct)), aes(x=1:13,y=cumsum.social_pct.),color='blue')+
  geom_line(data=data.frame(cumsum(noSocial_pct)), aes(x=1:13,y=cumsum.noSocial_pct.), color='red')+
  geom_line(data=data.frame(cumsum(control_pct)), aes(x=1:13,y=cumsum.control_pct.), color='green')+
  ylab("Cumulative Percent")+
  xlab("Diagnostic Referral Length")+
  scale_x_continuous(breaks=1:12)+
  theme_bw()+
  ylim(c(0,1))

################################BEGIN OLD 
#(frequency plots to compare to the above percentage plots)
##Screening
ggplot()+
  geom_histogram(data=intervention_sc.df, aes(x=screening_referral_length, color = soc), fill = 'none', bins=14)+    
  geom_histogram(data=noSocial_intervention_sc.df, aes(x=screening_referral_length, color = ins), fill = 'none', bins=14)+
  geom_histogram(data=control_sc.df, aes(x=screening_referral_length, color = con), fill = 'none', bins=14)+
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))
##Diagnostic
ggplot()+
  geom_histogram(data=control_dt.df, aes(x=diagnostic_referral_length, color = con), fill = 'none', bins=14)+
  geom_histogram(data=noSocial_intervention_dt.df, aes(x=diagnostic_referral_length, color = ins), fill = 'none',bins=14)+
  geom_histogram(data=intervention_dt.df, aes(x=diagnostic_referral_length, color = soc), fill = 'none',bins=14)+
  scale_color_manual("Scenario", limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))

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

###########################################END OLD

#Navigated Diagnostic completion length by percentage
noSocial_tbl<-filter(noSocial_intervention_sc.df,navigated==1) %>% group_by(screening_referral_length)
social_tbl<-filter(intervention_sc.df,navigated==1) %>% group_by(screening_referral_length)

noSocial_tbl_unnav<-filter(noSocial_intervention_sc.df,navigated==0) %>% group_by(screening_referral_length)
social_tbl_unnav<-filter(intervention_sc.df,navigated==0) %>% group_by(screening_referral_length)

noSocial_pct <- table(group_indices(noSocial_tbl))/length(noSocial_tbl$time)
social_pct <- table(group_indices(social_tbl))/length(social_tbl$time)

noSocial_pct_unnav <- table(group_indices(noSocial_tbl_unnav))/length(noSocial_tbl_unnav$time)
social_pct_unnav <- table(group_indices(social_tbl_unnav))/length(social_tbl_unnav$time)

#Shift each index over because [1] is actually expiration % (switch so that [13] contains 'expired')
temp <- noSocial_pct[1]
for(i in 1:13){noSocial_pct[i-1] <- noSocial_pct[i]
}
noSocial_pct[13] <- temp
temp <- social_pct[1]
for(i in 1:13){social_pct[i-1] <- social_pct[i]
}
social_pct[13] <- temp

temp <- noSocial_pct_unnav[1]
for(i in 1:13){noSocial_pct_unnav[i-1] <- noSocial_pct_unnav[i]
}
noSocial_pct_unnav[13] <- temp
temp <- social_pct_unnav[1]
for(i in 1:13){social_pct_unnav[i-1] <- social_pct_unnav[i]
}
social_pct_unnav[13] <- temp

#Plotting NAVIGATED diagnostic referral length by percentage (NON cumulative) breakdown
ggplot()+
  geom_point(data=data.frame(social_pct), aes(x=Var1,y=Freq,color=soc))+
  geom_point(data=data.frame(noSocial_pct), aes(x=Var1,y=Freq, color=ins))+
  geom_point(data=data.frame(social_pct_unnav), aes(shape=21,x=Var1,y=Freq,color=soc))+
  geom_point(data=data.frame(noSocial_pct_unnav), aes(shape=21,x=Var1,y=Freq, color=ins))+
  scale_shape_identity()+
  ylab("Percent")+
  xlab("Navigated Screening Referral Length")+
  #scale_x_continuous(breaks=1:12)+
  theme_bw()+
  scale_color_manual("Scenario", limits = c( "Institutional without social", "Institutional with social"),
                     values = c('red','blue'))+
  ylim(c(0,1))


############OLD 
#This version is not percentage
#Comparing Nav. vs. Unnav by Length of diagnostic referral
ggplot()+
  geom_histogram(data=filter(intervention_dt.df, navigated==1), aes(x=diagnostic_referral_length, color = 'navigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=filter(intervention_dt.df, navigated==0), aes(x=diagnostic_referral_length, color = 'unnavigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  ylab("Frequency")+
  ylim(0,12500)+
  theme_bw()+
  scale_x_continuous(name="Referral Length", breaks=-1:13, labels=-1:13)+ #No X-axis name or Labels for some reason??
  scale_color_manual("Social + Institutional", limits = c("navigated", "unnavigated"),
                     values = c('blue', 'green'))
ggplot()+
  geom_histogram(data=filter(noSocial_intervention_dt.df, navigated==1), aes(x=diagnostic_referral_length, color = 'navigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  geom_histogram(data=filter(noSocial_intervention_dt.df, navigated==0), aes(x=diagnostic_referral_length, color = 'unnavigated'), fill = 'none', bins=15, binwidth=1, boundary=-0.5)+
  ylab("Frequency")+
  ylim(0,12500)+
  theme_bw()+
  scale_x_continuous(name="Referral Length", breaks=-1:13, labels=-1:13)+ #No X-axis name or Labels for some reason??
  scale_color_manual("Institutional Only", limits = c("navigated", "unnavigated"),
                     values = c('blue', 'green'))

########END OLD

#Screening Referrals: Completion vs. Expiration
#Cumulative Screening Referrals, Completions and Expirations
ggplot()+
  theme_bw()+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.screening.referrals,color="Screening Referrals"))+
  geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=cumsum(m_number.of.screen.completed),color="Screen Completed"))+
  #geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time, y=m_number.of.expired.screening.referrals, color="Screen Expired"))+
  geom_line(data=filter(noSocial_intervention_sc.df, slurm_instance == 1), aes(x=time, y=cumsum(expired), color = "expirations"))+
  scale_color_manual("Scenario", limits = c("Screening Referrals", "Screen Completed", "expirations"),
                     values = c('green','red','blue')
  )


#Expirations #NOTE: m_number.of.expired.screening.referrals is CUMULATIVE ALREADY
#Screening
ggplot(data=control.df_mean_at_time)+
  geom_line(aes(x=time,y=m_number.of.expired.screening.referrals, color="total screening expiry count at t"))+
  geom_line(aes(x=time,y=m_number.of.screening.referrals, color="daily outstanding screening referrals"))+
  geom_line(aes(x=time,y=cumsum(m_number.of.screen.completed), color="total screening completion count at t"))+ 
  geom_line(aes(x=time,y=cumsum(m_number.of.screening.referrals.at.t), color="number of screening referrals at t"))+
  geom_line(aes(x=time,y=m_number.of.expired.screening.referrals + cumsum(m_number.of.screen.completed), color="total combined expirations + completions at t"))+
  ylab("Number")+
  scale_color_manual("Scenario:Control", 
                     limits = c("total screening expiry count at t", "daily outstanding screening referrals", "total screening completion count at t","number of screening referrals at t","total combined expirations + completions at t"),
                     values = c('green','red','blue', "yellow", "black"))
#Diagnostic
ggplot(data=control.df_mean_at_time)+
  geom_line(aes(x=time,y=m_number.of.expired.diagnostic.referrals, color="total diagnostic expiry count at t"))+
  geom_line(aes(x=time,y=m_number.of.diagnostic.referrals, color="daily outstanding diagnostic referrals"))+
  geom_line(aes(x=time,y=cumsum(m_number.of.dt.completed), color="total diagnostic completion count at t"))+ 
  geom_line(aes(x=time,y=cumsum(m_number.of.diagnostic.referrals.at.t), color="number of diagnostic referrals at t"))+
  geom_line(aes(x=time,y=m_number.of.expired.diagnostic.referrals + cumsum(m_number.of.dt.completed), color="total combined expirations + completions at t"))+
  ylab("Number")+
  scale_color_manual("Scenario:Control", 
                     limits = c("total diagnostic expiry count at t", "daily outstanding diagnostic referrals", "total diagnostic completion count at t","number of diagnostic referrals at t","total combined expirations + completions at t"),
                     values = c('green','red','blue', "yellow", "black"))


#Inter-scenario Referral Expirations
##Screening
ggplot(data=control.df_mean_at_time)+
  geom_line(aes(x=time,y=m_number.of.expired.screening.referrals, color=con))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.expired.screening.referrals, color=ins))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time,y=m_number.of.expired.screening.referrals, color=soc))+
  ylab("Number of screening expirations")+
  scale_color_manual("Scenario:Control", 
                     limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))
  #Inter-scenario # of screening expirations by screening events log
  ggplot(data=control.df_mean_at_time)+
    geom_line(data=filter(control_sc.df, slurm_instance == 1), aes(x=time, y=cumsum(expired), color = con))+
    geom_line(data=filter(noSocial_intervention_sc.df, slurm_instance == 1), aes(x=time, y=cumsum(expired), color = ins))+
    geom_line(data=filter(intervention_sc.df, slurm_instance == 1), aes(x=time, y=cumsum(expired), color = soc))+
    ylab("Number of screening expirations")+
    scale_color_manual("Scenario:Control", 
                       limits = c("control", "Institutional without social", "Institutional with social"),
                       values = c('green','red','blue'))
#Expiration Ratio
ggplot(data=control.df_mean_at_time)+
  geom_line(aes(x=time,y=m_number.of.expired.screening.referrals/cumsum(m_number.of.screening.referrals.at.t), color=con))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.expired.screening.referrals/cumsum(m_number.of.screening.referrals.at.t), color=ins))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time,y=m_number.of.expired.screening.referrals/cumsum(m_number.of.screening.referrals.at.t), color=soc))+
  ylab("Number of screening expirations")+
  scale_color_manual("Scenario:Control", 
                     limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))+
  ylim(c(0,1))
  
##Diagnostic
ggplot(data=control.df_mean_at_time)+
  geom_point(aes(x=time,y=m_number.of.expired.diagnostic.referrals, color=con))+
  geom_point(data=noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.expired.diagnostic.referrals, color=ins))+
  geom_point(data=intervention.df_mean_at_time, aes(x=time,y=m_number.of.expired.diagnostic.referrals, color=soc))+
  ylab("Number of diagnostic expirations")+
  scale_color_manual("Scenario:institutional", 
                     limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))
#Expiration Ratio
ggplot(data=control.df_mean_at_time)+
  geom_line(aes(x=time,y=m_number.of.expired.diagnostic.referrals/cumsum(m_number.of.diagnostic.referrals.at.t), color=con))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.expired.diagnostic.referrals/cumsum(m_number.of.diagnostic.referrals.at.t), color=ins))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time,y=m_number.of.expired.diagnostic.referrals/cumsum(m_number.of.diagnostic.referrals.at.t), color=soc))+
  ylab("Number of screening expirations")+
  scale_color_manual("Scenario:Control", 
                     limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))+
  ylim(c(0,1))
  

################################Testing single instance event log data plotting
a <- filter(intervention_sc.df, slurm_instance == 1 & navigated == 0)
b <- filter(intervention_sc.df, slurm_instance == 1 & navigated == 1)

ggplot()+
  geom_line(data=a, aes(x=time, y=cumsum(expired), color = ins))+
  geom_line(data=b, aes(x=time, y=cumsum(expired), color = soc))+
  ylab("Number of screening expirations")+
  scale_color_manual("Scenario:Control", 
                     limits = c("control", "Institutional without social", "Institutional with social"),
                     values = c('green','red','blue'))
ggplot()+
  geom_point(data=filter(control_sc.df, slurm_instance == 1), aes(x=time, y=cumsum(expired), color = "expirations"))

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
#Plotting symptom severity percentage breakdown

#Early vs. Late diagnosis (this is highly dependent on our cancer progression model (big limitation in my opinion))
##Plot is confusing because we are just dealing with single percentages

early_vs_late <-c(p_institutional_SS_bc_pos_nav_late,
                  p_institutional_SS_bc_pos_unnav_late,
                  p_social_SS_bc_pos_nav_late,
                  p_social_SS_bc_pos_unnav_late
                  )%>% 
                  cbind(c(1:4))

colnames(early_vs_late) <- c("percent","scenario")

ggplot(data=data.frame(early_vs_late))+
  geom_col(aes(x = scenario, y=percent))+
  ylab("Percent of Diagnoses")+
  xlab("Scenario and Diagnostic Outcome")+
  scale_x_discrete(limits=c(1:4),labels=c("Institutional Navigated-Late", "Institutional Unavigated-Late", "Social Navigated-Late", "Social Unnavigated-Late"))+
  ylim(c(0,50))


#Generate table
p_control_SS_total
p_control_SS_unnav


p_institutional_SS_total
p_institutional_SS_nav
p_institutional_SS_unnav

p_social_SS_total
p_social_SS_nav
p_social_SS_unnav


#other?
p_institutional_SS_bc_pos_nav
p_institutional_SS_bc_pos_unnav

p_social_SS_bc_pos_unnav
p_social_SS_bc_pos_nav


