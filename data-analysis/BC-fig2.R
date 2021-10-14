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
date<-"13:50:15_2021-05-13" #3rd round (reset neighbor_navigation_roll at each navigation end point)
#date <- "11:12:00_2021-06-05" #includes institutional draw for navigation end
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
  "instance",
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

########Referral Lengths###########################################################################################
##SCREENING
con_expirations <- control_sc.df %>% 
                     filter(time >= 60) %>%
                     group_by(instance) %>% 
                     summarize(sum(expired)/length(expired)*100, 
                              100-(sum(expired)/length(expired)*100),
                              length(expired))

noSocial_expirations <- noSocial_intervention_sc.df %>% 
                          filter(time >= 60) %>%
                          group_by(instance) %>% 
                          summarize(sum(expired)/length(expired)*100, 
                                    100-(sum(expired)/length(expired)*100),
                                    length(expired))

social_expirations <- intervention_sc.df %>% 
                        filter(time >= 60) %>%
                        group_by(instance) %>% 
                        summarize(sum(expired)/length(expired)*100, 
                                  100-(sum(expired)/length(expired)*100),
                                  length(expired))

con_expirations <-cbind(con_expirations, "scenario"= "Control")
noSocial_expirations <- cbind(noSocial_expirations, "scenario"= "Institutional Only")
social_expirations <- cbind(social_expirations, "scenario"= "Institutional and Social")

colnames(con_expirations) <- c("Instance", "Percent_expired","Percent_completed","Total_Screenings", "Scenario")
colnames(noSocial_expirations) <- c("Instance", "Percent_expired","Percent_completed","Total_Screenings", "Scenario")
colnames(social_expirations) <- c("Instance", "Percent_expired","Percent_completed","Total_Screenings", "Scenario")

total_expirations <- rbind(con_expirations, noSocial_expirations, social_expirations)

#Notch gives us an approximate 95% CI
a <- ggplot(total_expirations, aes(group=Scenario, y=Percent_expired, fill = Scenario))+ 
  geom_boxplot()+
  scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
  ylab("Referral Expiration Rate (%)")+
  xlab("Scenario")

a100 <- ggplot(total_expirations, aes(group=Scenario, y=Percent_expired, fill = Scenario))+ 
  geom_boxplot()+
  scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
  ylab("Referral Expiration Rate (%)")+
  xlab("Scenario")+
  ylim(0,100)

plot_grid(nrow=1,ncol=2,a100+theme(legend.position = "none"),a)

mean(total_expirations[1:30,]$Percent_expired)#control
mean(total_expirations[31:60,]$Percent_expired)#institutional
mean(total_expirations[60:90,]$Percent_expired)#social


###############Navigated vs unnavigated
nun_con_expirations <- control_sc.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total_screening_referrals"=length(expired),
            "navigated_completion_rate"= length(which(navigated == 0 & expired == 0)) / length(which(navigated == 0))*100,  #No navigation in control!
            "unnavigated_completion_rate"=length(which(navigated == 0 & expired == 0)) / length(which(navigated == 0))*100
            )

nun_noSocial_expirations <- noSocial_intervention_sc.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total_screening_referrals"=length(expired),
            "navigation_completion_rate"=length(which(navigated == 1 & expired == 0)) / length(which(navigated == 1))*100,
            "unnavigated_completion_rate"=length(which(navigated == 0 & expired == 0)) / length(which(navigated == 0))*100
            )
            
nun_social_expirations <- intervention_sc.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total_screening_referrals"=length(expired),
            "navigated_NA_completion_rate"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1))*100,
            "navigated_no_NA_completion_rate"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1))*100,
            "unnavigated_NA_completion_rate"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0))*100,
            "unnavigated_no_NA_completion_rate"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0))*100
            ) #TODO stratify

nun_con_expirations <-cbind(nun_con_expirations, "scenario"= "Control")
nun_noSocial_expirations <- cbind(nun_noSocial_expirations, "scenario"= "Institutional Only")
nun_social_expirations <- cbind(nun_social_expirations, "scenario"= "Institutional and Social")

#TODO stratify
colnames(nun_con_expirations)     <- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_completion_rate", "unnavigated_completion_rate","Scenario")
colnames(nun_noSocial_expirations)<- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_completion_rate", "unnavigated_completion_rate","Scenario")
colnames(nun_social_expirations)  <- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_completion_rate", "unnavigated_completion_rate","Scenario")

nun_total_expirations <- rbind(nun_con_expirations, nun_noSocial_expirations, nun_social_expirations)

#Unnvigated
b <-ggplot(nun_total_expirations, aes(group=Scenario, y=unnavigated_completion_rate, fill = Scenario))+ 
  geom_boxplot()+
  scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
  ylab("Screening Referral Completion Rate (%)")+
  theme_bw()+
  scale_fill_manual("Scenario", values = c("#999999", "#E69F00", "#56B4E9"),
  )
mean(nun_total_expirations[1:30,]$unnavigated_completion_rate)#control
mean(nun_total_expirations[31:60,]$unnavigated_completion_rate)#institutional
mean(nun_total_expirations[60:90,]$unnavigated_completion_rate)#social
#relative increases
(mean(nun_total_expirations[31:60,]$unnavigated_completion_rate)-mean(nun_total_expirations[1:30,]$unnavigated_completion_rate))/mean(nun_total_expirations[1:30,]$unnavigated_completion_rate)*100#institutional
(mean(nun_total_expirations[60:90,]$unnavigated_completion_rate)-mean(nun_total_expirations[1:30,]$unnavigated_completion_rate))/mean(nun_total_expirations[1:30,]$unnavigated_completion_rate)*100 #social

#Navigated
#Notch gives us an approximate 95% CI
ggplot(nun_total_expirations, aes(group=Scenario, y=navigated_completion_rate, fill = Scenario))+ 
  geom_boxplot()+
  #geom_boxplot(aes(group=Scenario, y=unnavigated_completion_rate, fill = Scenario),notch=TRUE)+
  scale_x_continuous(breaks = c(-.18555, .18555),labels=c("Institutional Only", "Institutional and Social"))+
  ylab("Screening Referral Completion Rate (%)")+
  theme_bw()+
  scale_fill_manual("Scenario", values = c("#999999", "#E69F00", "#56B4E9"),
  )

#Mean completion rates
mean(nun_total_expirations[1:30,]$navigated_completion_rate)#control
mean(nun_total_expirations[31:60,]$navigated_completion_rate)#institutional
mean(nun_total_expirations[60:90,]$navigated_completion_rate)#social
#relative increases
(mean(nun_total_expirations[31:60,]$navigated_completion_rate)-mean(nun_total_expirations[1:30,]$navigated_completion_rate))/mean(nun_total_expirations[1:30,]$navigated_completion_rate)*100#control#institutional

(mean(nun_total_expirations[60:90,]$navigated_completion_rate)-mean(nun_total_expirations[1:30,]$navigated_completion_rate))#control/mean(nun_total_expirations[1:30,]$navigated_completion_rate)*100 #social

#All together nav. vs. unnav plot
s <- ggplot()+ #TODO stratify
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_completion_rate, fill = Scenario, color="Navigated"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_completion_rate, fill = Scenario, color="Unnavigated"))+
  scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
  ylab("Screening Referral Completion Rate (%)")+
  theme_bw()+
  scale_fill_manual("Scenario", values = c("#999999", "#E69F00", "#56B4E9")
  )+
  scale_color_manual("Navigation Status",breaks = c("Navigated","Unnavigated"), values=c("black","brown3")
  )
 
s100 <- c <- ggplot()+ 
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_completion_rate, fill = Scenario, color="Navigated"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_completion_rate, fill = Scenario, color="Unnavigated"))+
  scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
  ylab("Screening Referral Completion Rate (%)")+
  theme_bw()+
  scale_fill_manual("Scenario", values = c("#999999", "#E69F00", "#56B4E9")
  )+
  scale_color_manual("Navigation Status",breaks = c("Navigated","Unnavigated"), values=c("black","brown3")
  )+
  ylim(0,100)

plot_grid(nrow=1,ncol=2, s100 + theme(legend.position = "none"), s+ylab(""))
