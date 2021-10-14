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
bc_navigation_root <- '/project2/khanna7/bryanb/bc-navigation/dec9_navlength/bc-navigation/'
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


#CALCULATE OVERALL POPULATION BC STAGE
cicalc <- function(data){
  a <- mean(data)
  s <- sd(data)
  n <- length(data)
  error <- qnorm(0.975)*s/sqrt(n)
  left <- a-error
  right <- a+error
  ci_interval <- c(round(left,digits=2),round(right,digits=2))
  return(ci_interval)
}

end_times <- c(1:30)*360 #used to capture end points of each instance

# Overall Population Stage at Diagnosis -----------------------------------


##OVERALL POP BC STAGE CONTROL:
#ss0 has an issue where all BC negative agents have ss0, have to derive # of true ss0 (BC+)
con_ss0_with_cancer <- control.df[end_times,]$number.of.positive.bc.agents - (control.df[end_times,]$n_ss1+
                                                                              control.df[end_times,]$n_ss2+
                                                                              control.df[end_times,]$n_ss3)

#Local, regional, distant stage
con_ss_instance_lists <- rbind((con_ss0_with_cancer + control.df[end_times,]$n_ss1)/control.df[end_times,]$number.of.positive.bc.agents*100,
                               control.df[end_times,]$n_ss2/control.df[end_times,]$number.of.positive.bc.agents*100,
                               control.df[end_times,]$n_ss3/control.df[end_times,]$number.of.positive.bc.agents*100
                              )

#output
con_mean_control_ss <- apply(con_ss_instance_lists, 1, mean)
con_pct_ss_95ci <- apply(con_ss_instance_lists, 1, cicalc)
round(con_mean_control_ss,2)
round(con_pct_ss_95ci,2)
##Debugging not summing to 100:
for (i in c(1:30)){a[i] <- sum(con_ss_instance_lists[,i])}



##OVERALL POP BC STAGE INSTITUTIONAL:
nos_ss0_with_cancer <- noSocial_intervention.df[end_times,]$number.of.positive.bc.agents - (noSocial_intervention.df[end_times,]$n_ss1+
                                                                                            noSocial_intervention.df[end_times,]$n_ss2+
                                                                                            noSocial_intervention.df[end_times,]$n_ss3)

#Local, regional, distant stage
nos_ss_instance_lists <- rbind((nos_ss0_with_cancer + noSocial_intervention.df[end_times,]$n_ss1)/noSocial_intervention.df[end_times,]$number.of.positive.bc.agents*100,
                               noSocial_intervention.df[end_times,]$n_ss2/noSocial_intervention.df[end_times,]$number.of.positive.bc.agents*100,
                               noSocial_intervention.df[end_times,]$n_ss3/noSocial_intervention.df[end_times,]$number.of.positive.bc.agents*100
)

#output
nos_mean_ss <- apply(nos_ss_instance_lists, 1, mean)
nos_pct_ss_95ci <- apply(nos_ss_instance_lists, 1, cicalc)
round(nos_mean_ss,2)
round(nos_pct_ss_95ci,2)

##OVERALL POP BC STAGE SOCIAL:
ins_ss0_with_cancer <- intervention.df[end_times,]$number.of.positive.bc.agents - (intervention.df[end_times,]$n_ss1+
                                                                                     intervention.df[end_times,]$n_ss2+
                                                                                     intervention.df[end_times,]$n_ss3)

#Local, regional, distant stage
ins_ss_instance_lists <- rbind((ins_ss0_with_cancer + intervention.df[end_times,]$n_ss1)/intervention.df[end_times,]$number.of.positive.bc.agents*100,
                                                     intervention.df[end_times,]$n_ss2/intervention.df[end_times,]$number.of.positive.bc.agents*100,
                                                     intervention.df[end_times,]$n_ss3/intervention.df[end_times,]$number.of.positive.bc.agents*100
                          )

#output
ins_mean_ss <- apply(ins_ss_instance_lists, 1, mean)
ins_pct_ss_95ci <- apply(ins_ss_instance_lists, 1, cicalc)
round(ins_mean_ss,2)
round(ins_pct_ss_95ci,2)




#TODO Update dataset to 30 yr run

# old code ----------------------------------------------------------------


#Percent of Population Navigated
# ggplot()+
#   theme_bw()+
#   geom_line(data = control.df_mean_at_time, aes(x=time/12, y=m_number.of.navigated.agents/5000 * 100,color=con))+
#   geom_line(data=control.df, alpha=0.1, aes(x=time/12, y=number.of.navigated.agents/5000 * 100 , color=con))+
#   geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time/12,y=m_number.of.navigated.agents/5000 * 100,color=ins))+
#   geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time/12, y=number.of.navigated.agents/5000 * 100,color=ins))+
#   geom_line(data = intervention.df_mean_at_time, aes(x=time/12, y=m_number.of.navigated.agents/5000 * 100, color=soc))+
#   geom_line(data=intervention.df, alpha=0.1, aes(x=time/12, y=number.of.navigated.agents/5000 * 100, color=soc))+
#   xlab("Time (years)")+
#   ylab("Percent of Women Navigated")+
#   theme(legend.position = c(0.8, 0.25))+
#   scale_color_manual("Scenario", values = c('green','red','blue'),
#                      limits = c(con, ins, soc)
#   )
# 
# mean(noSocial_intervention.df_mean_at_time[60:360,]$m_number.of.navigated.agents/50)
# #mean(noSocial_intervention.df_mean_at_time$m_number.of.navigated.agents[5*365:length(noSocial_intervention.df$time)]/5000 * 100 )
# mean(intervention.df_mean_at_time[60:360,]$m_number.of.navigated.agents/50)
# #mean(intervention.df$number.of.navigated.agents/5000 * 100 )
# 


#Symptom severity calculations
control_SS_total<-table(control_dt.df$symptom_severity)
institutional_SS_total<-table(noSocial_intervention_dt.df$symptom_severity)
social_SS_total<-table(intervention_dt.df$symptom_severity)
p_control_SS_total<-control_SS_total/sum(control_SS_total)*100
p_institutional_SS_total<-institutional_SS_total/sum(institutional_SS_total)*100
p_social_SS_total<- social_SS_total/sum(social_SS_total)*100


#Calculating means and 95CI for stage-at-diagnosis


upper_ci <- function(data){
  a <- mean(data)
  s <- sd(data)
  n <- length(data)
  error <- qnorm(0.975)*s/sqrt(n)
  upper_ci <- a+error
  return(upper_ci)
}
lower_ci <- function(data){
  a <- mean(data)
  s <- sd(data)
  n <- length(data)
  error <- qnorm(0.975)*s/sqrt(n)
  lower_ci <- a-error
  return(lower_ci)
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


  ##CONTROL SS FOR NO NAV NO NA
no_no_control_grouped_ss<- control_dt.df %>%
  filter(cancer_status == 1 & navigated==0 & neighbor_navigated==0) %>%
  select(symptom_severity, instance) %>%
  mutate(scenario = "control") %>%
  group_by(instance)

no_no_ctemp <- c()
for(i in c(1:30)){
  #temp <- append(temp,i)
  for (j in 0:3){
    no_no_ctemp <- append(no_no_ctemp,length(filter(no_no_control_grouped_ss, instance == i & symptom_severity == j)$symptom_severity)/length(filter(no_no_control_grouped_ss, instance == i)$symptom_severity)*100)
  }
}
no_no_control_ss1 <- no_no_ctemp[seq(1,length(no_no_ctemp),4)]
no_no_control_ss2 <- no_no_ctemp[seq(2,length(no_no_ctemp),4)]
no_no_control_ss3 <- no_no_ctemp[seq(3,length(no_no_ctemp),4)]
no_no_control_ss4 <- no_no_ctemp[seq(4,length(no_no_ctemp),4)]

#Localized
cat(round(mean(no_no_control_ss1+no_no_control_ss2), digits=2),"%(",round(lower_ci(no_no_control_ss1+no_no_control_ss2), digits=2),",",round(upper_ci(no_no_control_ss1+no_no_control_ss2), digits=2),")")
#Regional
cat(round(mean(no_no_control_ss3), digits=2),"%(",round(lower_ci(no_no_control_ss3), digits=2),",",round(upper_ci(no_no_control_ss3), digits=2),")")
#Distant
cat(round(mean(no_no_control_ss4), digits=2),"%(",round(lower_ci(no_no_control_ss4), digits=2),",",round(upper_ci(no_no_control_ss4), digits=2),")")



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

cat(round(mean(institutional_ss1+institutional_ss2), digits=2),"%(",round(lower_ci(institutional_ss1+institutional_ss2), digits=2),",",round(upper_ci(institutional_ss1+institutional_ss2), digits=2),")")
#cat(round(mean(institutional_ss2), digits=2),"%(",round(lower_ci(institutional_ss2), digits=2),",",round(upper_ci(institutional_ss2), digits=2),")")
cat(round(mean(institutional_ss3), digits=2),"%(",round(lower_ci(institutional_ss3), digits=2),",",round(upper_ci(institutional_ss3), digits=2),")")
cat(round(mean(institutional_ss4), digits=2),"%(",round(lower_ci(institutional_ss4), digits=2),",",round(upper_ci(institutional_ss4), digits=2),")")

##INSTITUTIONAL SS FOR NO NAV NO NA
no_no_institutional_grouped_ss<- noSocial_intervention_dt.df %>%
  filter(cancer_status == 1 & navigated==0 & neighbor_navigated==0) %>%
  select(symptom_severity, instance) %>%
  mutate(scenario = "institutional") %>%
  group_by(instance)

no_no_itemp <- c()
for(i in c(1:30)){
  #temp <- append(temp,i)
  for (j in 0:3){
    no_no_itemp <- append(no_no_itemp,length(filter(no_no_institutional_grouped_ss, instance == i & symptom_severity == j)$symptom_severity)/length(filter(no_no_institutional_grouped_ss, instance == i)$symptom_severity)*100)
  }
}
no_no_institutional_ss1 <- no_no_itemp[seq(1,length(no_no_itemp),4)]
no_no_institutional_ss2 <- no_no_itemp[seq(2,length(no_no_itemp),4)]
no_no_institutional_ss3 <- no_no_itemp[seq(3,length(no_no_itemp),4)]
no_no_institutional_ss4 <- no_no_itemp[seq(4,length(no_no_itemp),4)]

#Localized
cat(round(mean(no_no_institutional_ss1+no_no_institutional_ss2), digits=2),"%(",round(lower_ci(no_no_institutional_ss1+no_no_institutional_ss2), digits=2),",",round(upper_ci(no_no_institutional_ss1+no_no_institutional_ss2), digits=2),")")
#Regional
cat(round(mean(no_no_institutional_ss3), digits=2),"%(",round(lower_ci(no_no_institutional_ss3), digits=2),",",round(upper_ci(no_no_institutional_ss3), digits=2),")")
#Distant
cat(round(mean(no_no_institutional_ss4), digits=2),"%(",round(lower_ci(no_no_institutional_ss4), digits=2),",",round(upper_ci(no_no_institutional_ss4), digits=2),")")


##INSTITUTIONAL SS FOR YES NAV NO NA
yes_no_institutional_grouped_ss<- noSocial_intervention_dt.df %>%
  filter(cancer_status == 1 & navigated==1 & neighbor_navigated==0) %>%
  select(symptom_severity, instance) %>%
  mutate(scenario = "institutional") %>%
  group_by(instance)

yes_no_itemp <- c()
for(i in c(1:30)){
  #temp <- append(temp,i)
  for (j in 0:3){
    yes_no_itemp <- append(yes_no_itemp,length(filter(yes_no_institutional_grouped_ss, instance == i & symptom_severity == j)$symptom_severity)/length(filter(yes_no_institutional_grouped_ss, instance == i)$symptom_severity)*100)
  }
}
yes_no_institutional_ss1 <-yes_no_itemp[seq(1,length(yes_no_itemp),4)]
yes_no_institutional_ss2 <-yes_no_itemp[seq(2,length(yes_no_itemp),4)]
yes_no_institutional_ss3 <-yes_no_itemp[seq(3,length(yes_no_itemp),4)]
yes_no_institutional_ss4 <-yes_no_itemp[seq(4,length(yes_no_itemp),4)]
#Localized
cat(round(mean(yes_no_institutional_ss1+yes_no_institutional_ss2), digits=2),"%(",round(lower_ci(yes_no_institutional_ss1+yes_no_institutional_ss2), digits=2),",",round(upper_ci(yes_no_institutional_ss1+yes_no_institutional_ss2), digits=2),")")
#Regional
cat(round(mean(yes_no_institutional_ss3), digits=2),"%(",round(lower_ci(yes_no_institutional_ss3), digits=2),",",round(upper_ci(yes_no_institutional_ss3), digits=2),")")
#Distant
cat(round(mean(yes_no_institutional_ss4), digits=2),"%(",round(lower_ci(yes_no_institutional_ss4), digits=2),",",round(upper_ci(yes_no_institutional_ss4), digits=2),")")





#SOCIAL
social_grouped_ss<- intervention_dt.df %>%
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

cat(round(mean(social_ss1+social_ss2), digits=2),"%(",round(lower_ci(social_ss1+social_ss2), digits=2),",",round(upper_ci(social_ss1+social_ss2), digits=2),")")
#cat(round(mean(social_ss2), digits=2),"%(",round(lower_ci(social_ss2), digits=2),",",round(upper_ci(social_ss2), digits=2),")")
cat(round(mean(social_ss3), digits=2),"%(",round(lower_ci(social_ss3), digits=2),",",round(upper_ci(social_ss3), digits=2),")")
cat(round(mean(social_ss4), digits=2),"%(",round(lower_ci(social_ss4), digits=2),",",round(upper_ci(social_ss4), digits=2),")")

##SOCIAL SS FOR NO NAV NO NA
no_no_social_grouped_ss<- intervention_dt.df %>%
  filter(cancer_status == 1 & navigated==0 & neighbor_navigated==0) %>%
  select(symptom_severity, instance) %>%
  mutate(scenario = "social") %>%
  group_by(instance)

no_no_stemp <- c()
for(i in c(1:30)){
  #temp <- append(temp,i)
  for (j in 0:3){
    no_no_stemp <- append(no_no_stemp,length(filter(no_no_social_grouped_ss, instance == i & symptom_severity == j)$symptom_severity)/length(filter(no_no_social_grouped_ss, instance == i)$symptom_severity)*100)
  }
}
no_no_social_ss1 <- no_no_stemp[seq(1,length(no_no_stemp),4)]
no_no_social_ss2 <- no_no_stemp[seq(2,length(no_no_stemp),4)]
no_no_social_ss3 <- no_no_stemp[seq(3,length(no_no_stemp),4)]
no_no_social_ss4 <- no_no_stemp[seq(4,length(no_no_stemp),4)]

#Localized
cat(round(mean(no_no_social_ss1+no_no_social_ss2), digits=2),"%(",round(lower_ci(no_no_social_ss1+no_no_social_ss2), digits=2),",",round(upper_ci(no_no_social_ss1+no_no_social_ss2), digits=2),")")
#Regional
cat(round(mean(no_no_social_ss3), digits=2),"%(",round(lower_ci(no_no_social_ss3), digits=2),",",round(upper_ci(no_no_social_ss3), digits=2),")")
#Distant
cat(round(mean(no_no_social_ss4), digits=2),"%(",round(lower_ci(no_no_social_ss4), digits=2),",",round(upper_ci(no_no_social_ss4), digits=2),")")


##social SS FOR YES NAV NO NA
yes_no_social_grouped_ss<- intervention_dt.df %>%
  filter(cancer_status == 1 & navigated==1 & neighbor_navigated==0) %>%
  select(symptom_severity, instance) %>%
  mutate(scenario = "social") %>%
  group_by(instance)

yes_no_stemp <- c()
for(i in c(1:30)){
  #temp <- append(temp,i)
  for (j in 0:3){
    yes_no_stemp <- append(yes_no_stemp,length(filter(yes_no_social_grouped_ss, instance == i & symptom_severity == j)$symptom_severity)/length(filter(yes_no_social_grouped_ss, instance == i)$symptom_severity)*100)
  }
}
yes_no_social_ss1 <-yes_no_stemp[seq(1,length(yes_no_stemp),4)]
yes_no_social_ss2 <-yes_no_stemp[seq(2,length(yes_no_stemp),4)]
yes_no_social_ss3 <-yes_no_stemp[seq(3,length(yes_no_stemp),4)]
yes_no_social_ss4 <-yes_no_stemp[seq(4,length(yes_no_stemp),4)]
#Localized
cat(round(mean(yes_no_social_ss1+yes_no_social_ss2), digits=2),"%(",round(lower_ci(yes_no_social_ss1+yes_no_social_ss2), digits=2),",",round(upper_ci(yes_no_social_ss1+yes_no_social_ss2), digits=2),")")
#Regional
cat(round(mean(yes_no_social_ss3), digits=2),"%(",round(lower_ci(yes_no_social_ss3), digits=2),",",round(upper_ci(yes_no_social_ss3), digits=2),")")
#Distant
cat(round(mean(yes_no_social_ss4), digits=2),"%(",round(lower_ci(yes_no_social_ss4), digits=2),",",round(upper_ci(yes_no_social_ss4), digits=2),")")

##SOCIAL SS FOR NO NAV YES NA
no_yes_social_grouped_ss<- intervention_dt.df %>%
  filter(cancer_status == 1 & navigated==0 & neighbor_navigated==1) %>%
  select(symptom_severity, instance) %>%
  mutate(scenario = "social") %>%
  group_by(instance)

no_yes_stemp <- c()
for(i in c(1:30)){
  #temp <- append(temp,i)
  for (j in 0:3){
    no_yes_stemp <- append(no_yes_stemp,length(filter(no_yes_social_grouped_ss, instance == i & symptom_severity == j)$symptom_severity)/length(filter(no_yes_social_grouped_ss, instance == i)$symptom_severity)*100)
  }
}
no_yes_social_ss1 <- no_yes_stemp[seq(1,length(no_yes_stemp),4)]
no_yes_social_ss2 <- no_yes_stemp[seq(2,length(no_yes_stemp),4)]
no_yes_social_ss3 <- no_yes_stemp[seq(3,length(no_yes_stemp),4)]
no_yes_social_ss4 <- no_yes_stemp[seq(4,length(no_yes_stemp),4)]

#Localized
cat(round(mean(no_yes_social_ss1+no_yes_social_ss2), digits=2),"%(",round(lower_ci(no_yes_social_ss1+no_yes_social_ss2), digits=2),",",round(upper_ci(no_yes_social_ss1+no_yes_social_ss2), digits=2),")")
#Regional
cat(round(mean(no_yes_social_ss3), digits=2),"%(",round(lower_ci(no_yes_social_ss3), digits=2),",",round(upper_ci(no_yes_social_ss3), digits=2),")")
#Distant
cat(round(mean(no_yes_social_ss4), digits=2),"%(",round(lower_ci(no_yes_social_ss4), digits=2),",",round(upper_ci(no_yes_social_ss4), digits=2),")")

##social SS FOR YES NAV YES NA
yes_yes_social_grouped_ss<- intervention_dt.df %>%
  filter(cancer_status == 1 & navigated==1 & neighbor_navigated==1) %>%
  select(symptom_severity, instance) %>%
  mutate(scenario = "social") %>%
  group_by(instance)

yes_yes_stemp <- c()
for(i in c(1:30)){
  #temp <- append(temp,i)
  for (j in 0:3){
    yes_yes_stemp <- append(yes_yes_stemp,length(filter(yes_yes_social_grouped_ss, instance == i & symptom_severity == j)$symptom_severity)/length(filter(yes_yes_social_grouped_ss, instance == i)$symptom_severity)*100)
  }
}
yes_yes_social_ss1 <-yes_yes_stemp[seq(1,length(yes_yes_stemp),4)]
yes_yes_social_ss2 <-yes_yes_stemp[seq(2,length(yes_yes_stemp),4)]
yes_yes_social_ss3 <-yes_yes_stemp[seq(3,length(yes_yes_stemp),4)]
yes_yes_social_ss4 <-yes_yes_stemp[seq(4,length(yes_yes_stemp),4)]
#Localized
cat(round(mean(yes_yes_social_ss1+yes_yes_social_ss2), digits=2),"%(",round(lower_ci(yes_yes_social_ss1+yes_yes_social_ss2), digits=2),",",round(upper_ci(yes_yes_social_ss1+yes_yes_social_ss2), digits=2),")")
#Regional
cat(round(mean(yes_yes_social_ss3), digits=2),"%(",round(lower_ci(yes_yes_social_ss3), digits=2),",",round(upper_ci(yes_yes_social_ss3), digits=2),")")
#Distant
cat(round(mean(yes_yes_social_ss4), digits=2),"%(",round(lower_ci(yes_yes_social_ss4), digits=2),",",round(upper_ci(yes_yes_social_ss4), digits=2),")")


# very old ----------------------------------------------------------------

########Referral Lengths###########################################################################################
##SCREENING
# control_tbl<-control_sc.df %>% group_by(screening_referral_length)
# noSocial_tbl<-noSocial_intervention_sc.df %>% group_by(screening_referral_length)
# social_tbl<-intervention_sc.df %>% group_by(screening_referral_length)
# 
# #Attempt to get data over runs
# p <- ggplot(control_tbl, aes(factor(instance)))
# p + geom_bar(aes(fill=factor(expired)))
# 
# 
# 
# 
# 
# control_pct <- table(group_indices(control_tbl))/length(control_tbl$time)
# noSocial_pct <- table(group_indices(noSocial_tbl))/length(noSocial_tbl$time)
# social_pct <- table(group_indices(social_tbl))/length(social_tbl$time)

#Shift each index over because [1] is actually expiration % (switch so that [13] contains 'expired')
# temp <- control_pct[1]
# for(i in 1:13){control_pct[i-1] <- control_pct[i]}
# control_pct[13] <- temp
# temp <- noSocial_pct[1]
# for(i in 1:13){noSocial_pct[i-1] <- noSocial_pct[i]}
# noSocial_pct[13] <- temp
# temp <- social_pct[1]
# for(i in 1:13){social_pct[i-1] <- social_pct[i]}
# social_pct[13] <- temp

#Plotting Screening referral length by percentage (NON cumulative) breakdown
# ggplot()+
#   geom_line(data=data.frame(cumsum(social_pct)), aes(x=1:13,y=cumsum.social_pct.*100,color=soc))+
#   geom_line(data=data.frame(cumsum(noSocial_pct)), aes(x=1:13,y=cumsum.noSocial_pct.*100, color=ins))+
#   geom_line(data=data.frame(cumsum(control_pct)), aes(x=1:13,y=cumsum.control_pct.*100, color=con))+
#   ylab("Percent of All Screening Referrals Completed by Month x (%)")+ #TODO REWORD this for clarity
#   xlab("Screening Referral Length (months)")+
#   scale_x_continuous(breaks=1:12, limits= c(1,12))+
#   theme_bw()+
#   ylim(c(0,100))+
#   theme(legend.position = c(0.81, 0.18))+
#   scale_color_manual("Scenario", values = c('green','red','blue'),
#                      limits = c(con, ins, soc)
#   )

##DIAGNOSTIC
#Cumulative Completion Percentage by Length
# control_tbl<-control_dt.df %>% group_by(diagnostic_referral_length)
# noSocial_tbl<-noSocial_intervention_dt.df %>% group_by(diagnostic_referral_length)
# social_tbl<-intervention_dt.df %>% group_by(diagnostic_referral_length)
# 
# control_pct <- table(group_indices(control_tbl))/length(control_tbl$time)
# noSocial_pct <- table(group_indices(noSocial_tbl))/length(noSocial_tbl$time)
# social_pct <- table(group_indices(social_tbl))/length(social_tbl$time)

#Shift each index over to start at 1 (since the previous 1 was actually expirations)
# for(i in 2:13){control_pct[i-1] <- control_pct[i]}
# control_pct <- control_pct[1:12]
# for(i in 2:13){noSocial_pct[i-1] <- noSocial_pct[i]}  
# noSocial_pct <- noSocial_pct[1:12]
# for(i in 2:13){social_pct[i-1] <- social_pct[i]}
# social_pct <- social_pct[1:12]
# 
# ggplot()+
#   geom_line(data=data.frame(cumsum(social_pct)), aes(x=1:12,y=cumsum.social_pct.*100,color=soc))+
#   geom_line(data=data.frame(cumsum(social_pct)), aes(x=1:12,y=cumsum.social_pct.*100,color=soc))+
#   
#   geom_line(data=data.frame(cumsum(noSocial_pct)), aes(x=1:12,y=cumsum.noSocial_pct.*100, color=ins))+
#   geom_line(data=data.frame(cumsum(control_pct)), aes(x=1:12,y=cumsum.control_pct.*100, color=con))+
#   ylab("Cumulative Percent of Referrals Completed by Month x")+
#   xlab("Diagnostic Referral Length (months)")+
#   scale_x_continuous(breaks=1:12)+
#   theme_bw()+
#   ylim(c(0,100))+
#   theme(legend.position = c(0.81, 0.18))+
#   scale_color_manual("Scenario", values = c('green','red','blue'),
#                      limits = c(con, ins, soc)
#   )

##Stage at Diagnosis Table
####################################Early vs late diagnosis
#Plotting stage at logging by percent
# n_total_referral_ends_control <- length(control_dt.df$time)
# n_total_referral_ends_institutional <- length(noSocial_intervention_dt.df$time)
# n_total_referral_ends_social <- length(intervention_dt.df$time)
# 
# bc_pos_n_total_referral_ends_control <- length(filter(control_dt.df, cancer_status==1)$time)
# bc_pos_n_total_referral_ends_institutional <- length(filter(noSocial_intervention_dt.df, cancer_status==1)$time)
# bc_pos_n_total_referral_ends_social <- length(filter(intervention_dt.df, cancer_status==1)$time)
# 
# nav_referral_ends_control <- filter(control_dt.df, navigated==1) 
# nav_referral_ends_institutional <- filter(noSocial_intervention_dt.df, navigated==1)
# nav_referral_ends_social <- filter(intervention_dt.df, navigated==1)
# 
# bc_pos_nav_referral_ends_control <- filter(nav_referral_ends_control, cancer_status==1) 
# bc_pos_nav_referral_ends_institutional <- filter(nav_referral_ends_institutional, cancer_status==1)
# bc_pos_nav_referral_ends_social <- filter(nav_referral_ends_social, cancer_status==1)
# bc_neg_nav_referral_ends_control <- filter(nav_referral_ends_control, cancer_status==0) 
# bc_neg_nav_referral_ends_institutional <- filter(nav_referral_ends_institutional, cancer_status==0)
# bc_neg_nav_referral_ends_social <- filter(nav_referral_ends_social, cancer_status==0)
# 
# unnav_referral_ends_control <- filter(control_dt.df, navigated==0)
# unnav_referral_ends_institutional <- filter(noSocial_intervention_dt.df, navigated==0)
# unnav_referral_ends_social <- filter(intervention_dt.df, navigated==0)
# 
# bc_pos_unnav_referral_ends_control <- filter(unnav_referral_ends_control, cancer_status==1)
# bc_pos_unnav_referral_ends_institutional <- filter(unnav_referral_ends_institutional, cancer_status==1)
# bc_pos_unnav_referral_ends_social <- filter(unnav_referral_ends_social, cancer_status==1)
# bc_neg_unnav_referral_ends_control <- filter(unnav_referral_ends_control, cancer_status==0)
# bc_neg_unnav_referral_ends_institutional <- filter(unnav_referral_ends_institutional, cancer_status==0)
# bc_neg_unnav_referral_ends_social <- filter(unnav_referral_ends_social, cancer_status==0)

#SS Breakdowns
# control_SS_nav  <-table(nav_referral_ends_control$symptom_severity)
# control_SS_unnav<-table(unnav_referral_ends_control$symptom_severity)
#control_SS_total<-table(control_dt.df$symptom_severity)
# #control_SS_bc_pos_nav<- table(bc_pos_nav_referral_ends_control$symptom_severity)
# control_SS_bc_pos_unnav<- table(bc_pos_unnav_referral_ends_control$symptom_severity)
# #control_SS_bc_neg_nav<- table(bc_neg_nav_referral_ends_control$symptom_severity)
# control_SS_bc_neg_unnav<- table(bc_neg_unnav_referral_ends_control$symptom_severity)

# institutional_SS_nav  <-table(nav_referral_ends_institutional$symptom_severity)
# institutional_SS_unnav<-table(unnav_referral_ends_institutional$symptom_severity)
#institutional_SS_total<-table(noSocial_intervention_dt.df$symptom_severity)
# institutional_SS_bc_pos_nav  <- table(bc_pos_nav_referral_ends_institutional$symptom_severity)
# institutional_SS_bc_pos_unnav<- table(bc_pos_unnav_referral_ends_institutional$symptom_severity)
# institutional_SS_bc_neg_nav  <- table(bc_neg_nav_referral_ends_institutional$symptom_severity)
# institutional_SS_bc_neg_unnav<- table(bc_neg_unnav_referral_ends_institutional$symptom_severity)

# social_SS_nav  <-table(nav_referral_ends_social$symptom_severity)
# social_SS_unnav<-table(unnav_referral_ends_social$symptom_severity)
#social_SS_total<-table(intervention_dt.df$symptom_severity)
# social_SS_bc_pos_nav  <- table(bc_pos_nav_referral_ends_social$symptom_severity)
# social_SS_bc_pos_unnav<- table(bc_pos_unnav_referral_ends_social$symptom_severity)
# social_SS_bc_neg_nav  <- table(bc_neg_nav_referral_ends_social$symptom_severity)
# social_SS_bc_neg_unnav<- table(bc_neg_unnav_referral_ends_social$symptom_severity)

#SS Breakdown by percent
# p_control_SS_nav<-control_SS_nav/sum(control_SS_nav)*100
# p_control_SS_unnav<-control_SS_unnav/sum(control_SS_unnav)*100
#p_control_SS_total<-control_SS_total/sum(control_SS_total)*100

# p_institutional_SS_nav  <-institutional_SS_nav/sum(institutional_SS_nav)*100
# p_institutional_SS_unnav<-institutional_SS_unnav/sum(institutional_SS_unnav)*100
#p_institutional_SS_total<-institutional_SS_total/sum(institutional_SS_total)*100
# p_institutional_SS_bc_pos_nav  <-institutional_SS_bc_pos_nav/sum(institutional_SS_bc_pos_nav)*100
# p_institutional_SS_bc_pos_unnav<-institutional_SS_bc_pos_unnav/sum(institutional_SS_bc_pos_unnav)*100
# p_institutional_SS_bc_neg_nav  <-institutional_SS_bc_neg_nav/sum(institutional_SS_bc_neg_nav)*100
# p_institutional_SS_bc_neg_unnav<-institutional_SS_bc_neg_unnav/sum(institutional_SS_bc_neg_unnav)*100
# 
# p_social_SS_nav  <-social_SS_nav/sum(social_SS_nav)*100
# p_social_SS_unnav<-social_SS_unnav/sum(social_SS_unnav)*100
#p_social_SS_total<- social_SS_total/sum(social_SS_total)*100
# p_social_SS_bc_pos_nav  <-social_SS_bc_pos_nav/sum(social_SS_bc_pos_nav)*100
# p_social_SS_bc_pos_unnav<-social_SS_bc_pos_unnav/sum(social_SS_bc_pos_unnav)*100
# p_social_SS_bc_neg_nav  <-social_SS_bc_neg_nav/sum(social_SS_bc_neg_nav)*100
# p_social_SS_bc_neg_unnav<-social_SS_bc_neg_unnav/sum(social_SS_bc_neg_unnav)*100
# 
# p_institutional_SS_nav_early  <- sum(institutional_SS_nav[1:2])/sum(institutional_SS_nav)*100
# p_institutional_SS_unnav_early<- sum(institutional_SS_unnav[1:2])/sum(institutional_SS_unnav)*100
# p_institutional_SS_total_early<- sum(institutional_SS_total[1:2])/sum(institutional_SS_total)*100
# p_institutional_SS_bc_pos_nav_early  <-sum(institutional_SS_bc_pos_nav[1:2])/sum(institutional_SS_bc_pos_nav)*100
# p_institutional_SS_bc_pos_unnav_early<-sum(institutional_SS_bc_pos_unnav[1:2])/sum(institutional_SS_bc_pos_unnav)*100
# p_institutional_SS_bc_neg_nav_early  <-sum(institutional_SS_bc_neg_nav[1:2])/sum(institutional_SS_bc_neg_nav)*100
# p_institutional_SS_bc_neg_unnav_early<-sum(institutional_SS_bc_neg_unnav[1:2])/sum(institutional_SS_bc_neg_unnav)*100
# 
# p_social_SS_nav_early  <-social_SS_nav/sum(social_SS_nav)*100
# p_social_SS_unnav_early<-social_SS_unnav/sum(social_SS_unnav)*100
# p_social_SS_total_early<- social_SS_total/sum(social_SS_total)*100
# p_social_SS_bc_pos_nav_early  <-sum(social_SS_bc_pos_nav[1:2])/sum(social_SS_bc_pos_nav)*100
# p_social_SS_bc_pos_unnav_early<-sum(social_SS_bc_pos_unnav[1:2])/sum(social_SS_bc_pos_unnav)*100
# p_social_SS_bc_neg_nav_early  <-sum(social_SS_bc_neg_nav[1:2])/sum(social_SS_bc_neg_nav)*100
# p_social_SS_bc_neg_unnav_early<-sum(social_SS_bc_neg_unnav[1:2])/sum(social_SS_bc_neg_unnav)*100
# 
# p_institutional_SS_nav_late  <- sum(institutional_SS_nav[3:4])/sum(institutional_SS_nav)*100
# p_institutional_SS_unnav_late<- sum(institutional_SS_unnav[3:4])/sum(institutional_SS_unnav)*100
# p_institutional_SS_total_late<- sum(institutional_SS_total[3:4])/sum(institutional_SS_total)*100
# p_institutional_SS_bc_pos_nav_late <-sum(institutional_SS_bc_pos_nav[3:4])/sum(institutional_SS_bc_pos_nav)*100
# p_institutional_SS_bc_pos_unnav_late<-sum(institutional_SS_bc_pos_unnav[3:4])/sum(institutional_SS_bc_pos_unnav)*100
# p_institutional_SS_bc_neg_nav_late <-sum(institutional_SS_bc_neg_nav[3:4])/sum(institutional_SS_bc_neg_nav)*100
# p_institutional_SS_bc_neg_unnav_late<-sum(institutional_SS_bc_neg_unnav[3:4])/sum(institutional_SS_bc_neg_unnav)*100
# 
# p_social_SS_nav_late  <-sum(social_SS_nav[3:4])/sum(social_SS_nav)*100
# p_social_SS_unnav_late<-sum(social_SS_unnav[3:4])/sum(social_SS_unnav)*100
# p_social_SS_total_late<- sum(social_SS_total[3:4])/sum(social_SS_total)*100
# p_social_SS_bc_pos_nav_late  <-sum(social_SS_bc_pos_nav[3:4])/sum(social_SS_bc_pos_nav)*100
# p_social_SS_bc_pos_unnav_late<-sum(social_SS_bc_pos_unnav[3:4])/sum(social_SS_bc_pos_unnav)*100
# p_social_SS_bc_neg_nav_late  <-sum(social_SS_bc_neg_nav[3:4])/sum(social_SS_bc_neg_nav)*100
# p_social_SS_bc_neg_unnav_late<-sum(social_SS_bc_neg_unnav[3:4])/sum(social_SS_bc_neg_unnav)*100

#Generate OLD broken table of symptom severity at diag. ref. end that doesn't exclude agents without cancer
#p_control_SS_total
##p_control_SS_unnav

#p_institutional_SS_total
##p_institutional_SS_nav
##p_institutional_SS_unnav

#p_social_SS_total
##p_social_SS_nav
##p_social_SS_unnav


# #Simultaneous screenings
# ggplot()+
#   theme_bw()+
#   geom_line(data = control.df_mean_at_time, aes(x=time, y=m_number.of.screening.referrals,color=con))+
#   geom_line(data=control.df, alpha=0.1, aes(x=time, y=number.of.screening.referrals, color=con))+
#   geom_line(data = noSocial_intervention.df_mean_at_time, aes(x=time,y=m_number.of.screening.referrals,color=ins))+
#   geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time, y=number.of.screening.referrals,color=ins))+
#   geom_line(data = intervention.df_mean_at_time, aes(x=time, y=m_number.of.screening.referrals, color=soc))+
#   geom_line(data=intervention.df, alpha=0.1, aes(x=time, y=number.of.screening.referrals, color=soc))+
#   scale_color_manual("Scenario", values = c('green','red','blue'),
#                      limits = c(con, ins, soc)
#   )#+
