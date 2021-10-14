rm(list=ls())

  library(dplyr)
  library(ggplot2)
  library(network)
  library(networkDynamic)
  library(cowplot)

## Add names of data directories here
#Directory name format is {date}_full_run
bc_navigation_root <- '/project2/khanna7/bryanb/bc-navigation/dec9_navlength/bc-navigation/'
#date<-"13:50:15_2021-05-13" #3rd round (reset neighbor_navigation_roll at each navigation end point)
date <- "20:53:45_2021-06-08"
full_run_name <- paste0(date, '_full_run)/')

# Read data and set meta-parameters ----------
N<-5000 #number of agents
n.instances<-30 #number of runs
run_length<-360 #number of time steps in run

control_dt_list <- as.list(1:n.instances)
intervention_dt_list <- as.list(1:n.instances)
noSocial_intervention_dt_list <- as.list(1:n.instances)

control_sc_list <- as.list(1:n.instances)
intervention_sc_list <- as.list(1:n.instances)
noSocial_intervention_sc_list <- as.list(1:n.instances)

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
control_dt.df <- bind_rows(control_dt_list[1:n.instances])
intervention_dt.df <- bind_rows(intervention_dt_list[1:n.instances])
noSocial_intervention_dt.df <- bind_rows(noSocial_intervention_dt_list[1:n.instances])

# Not Sure What this is all about
control_sc.df <- bind_rows(control_sc_list[1:n.instances])
intervention_sc.df <- bind_rows(intervention_sc_list[1:n.instances])
noSocial_intervention_sc.df <- bind_rows(noSocial_intervention_sc_list[1:n.instances])

colnames(control_dt.df) <- diagnostic_event_columns
colnames(intervention_dt.df) <- diagnostic_event_columns
colnames(noSocial_intervention_dt.df) <- diagnostic_event_columns

colnames(control_sc.df) <- screening_event_columns
colnames(intervention_sc.df) <- screening_event_columns
colnames(noSocial_intervention_sc.df) <- screening_event_columns

####BEGIN PLOTS#############################################################################
con<-"Control"
ins<-"Institutional without social"
soc<-"Institutional with social"

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

write.csv(nun_total_expirations, 'expiration_rate_sample.csv' )

  d <- ggplot()+ #TODO stratify
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_NA_completion_rate, color="Navigated with NA"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_no_NA_completion_rate, color="Navigated no NA"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_NA_completion_rate, color="Unnavigated with NA"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_no_NA_completion_rate, color="Unnavigated no NA"))+
  scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
  ylab("Diagnostic Referral Completion Rate (%)")+
  theme_bw()


  plot_grid(ncol=2,nrow=1, d+ylim(0,100)+theme(legend.position="none"),d+ylab("")+ylim(75,100))

  
  #Help from zyke on discoRd
  library(tidyverse)
  bran <- nun_total_expirations
  bran_long <- 
    bran %>% 
    pivot_longer(
      cols = c(navigated_no_NA_completion_rate, 
               unnavigated_NA_completion_rate, 
               unnavigated_no_NA_completion_rate,
               navigated_NA_completion_rate
      ), 
      names_to = 'group', 
      values_to = 'values'
    )
  
  #version in a line
d1<-ggplot(bran_long, aes(
    y = values, 
    x = Scenario,
    color = group
  )) +
    geom_boxplot(position = 'identity', alpha=.1)+
    ylab("Diagnostic Referral Completion Rate (%)")+  
    labs(group = "Treatment")+
    theme_bw()
  
  #version with non-overlapping boxplots
d2<-ggplot(bran_long, aes(
    y = values, 
    x = Scenario,
    color = group
  )) +
    geom_boxplot()+
    ylab("Diagnostic Referral Completion Rate (%)")+  
    labs(group = "Treatment")+ #TODO figure out why this doesn't work
    theme_bw()

#stacked boxplots
plot_grid(ncol=2,nrow=1, d1+ylim(0,100)+theme(legend.position="none"),d1+ylab("")+ylim(75,100))

#non-overlapping boxplots
plot_grid(ncol=2,nrow=1, d2+ylim(0,100)+theme(legend.position="none"),d2+ylab("")+ylim(75,100))

  # bran_long %>% 
  #   filter(
  #     !(Scenario == "Control" & group %in% c("navigated_NA_completion_rate", "unnavigated_no_NA_completion_rate") |
  #         Scenario == "Institutional Only" & group %in% c("navigated_NA_completion_rate")
  #     )
  #   ) %>% 
  #   ggplot(aes(
  #     y = values, 
  #     x = Scenario,
  #     color = group
  #   )) +
  #   geom_boxplot()

#Screening Completions
nun_con_expirations <- control_sc.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total_screening_referrals"=length(expired),
            "Navigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,#length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1& neighbor_navigated == 1))*100,
            "Navigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,#length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1 & neighbor_navigated == 0))*100,
            "Unnavigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0& neighbor_navigated == 1))*100,
            "Unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100
  ) 
nun_noSocial_expirations <- noSocial_intervention_sc.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total_screening_referrals"=length(expired),
            "Navigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,#length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1& neighbor_navigated == 1))*100,
            "Navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1 & neighbor_navigated == 0))*100,
            "Unnavigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0& neighbor_navigated == 1))*100,
            "Unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100
  ) 

nun_social_expirations <- intervention_sc.df %>% 
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

colnames(nun_con_expirations)     <- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate","Scenario")
colnames(nun_noSocial_expirations)<- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate","Scenario")
colnames(nun_social_expirations)  <- c("Instance", "Percent_expired","Percent_completed","Total_screenings", "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate","Scenario")

nun_total_expirations <- rbind(nun_con_expirations, nun_noSocial_expirations, nun_social_expirations)

s <- ggplot()+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_NA_completion_rate, color="Navigated with NA"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_no_NA_completion_rate, color="Navigated no NA"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_NA_completion_rate, color="Unnavigated with NA"))+
  geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_no_NA_completion_rate, color="Unnavigated no NA"))+
  scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
  ylab("Screening Referral Completion Rate (%)")+
  theme_bw()+
  labs(color = "Treatment")

s  
plot_grid(nrow=1,ncol=2, s +ylim(0,100) +theme(legend.position = "none"), s+ylab(""))


#Confidence intervals
cicalc <- function(data){
  a <- mean(data)
  s <- sd(data)
  n <- length(data)
  error <- qnorm(0.975)*s/sqrt(n)
  left <- a-error
  right <- a+error
  ci_interval <- c(left,right)
  return(ci_interval)

}
