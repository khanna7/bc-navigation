rm(list=ls())

  library(tidyverse)
  library(network)
  library(networkDynamic)
  library(cowplot)

#load()

## Add names of data directories here
#Directory name format is {date}_full_run
bc_navigation_root <- '/project2/ahotton/bryanb/bc-navigation/dec9_navlength/bc-navigation/'
#date<-"13:50:15_2021-05-13" #3rd round (reset neighbor_navigation_roll at each navigation end point)
date <- "15:54:31_2021-07-08"
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





#Collect and format DIAGNOSTIC data for each scenario 
d_con_expirations <- control_dt.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total population"=length(which(expired == 0))/length(expired)*100,
            "Navigated with NA"=-100,#length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1& neighbor_navigated == 1))*100,
            "Navigated no NA"=-100,#length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1 & neighbor_navigated == 0))*100,
            "Unnavigated with NA"=-100,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0& neighbor_navigated == 1))*100,
            "Unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,
            "Number of navigated with NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1& cancer_status==1)),
            "Number of navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0& cancer_status==1)),
            "Number of unnavigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1& cancer_status==1)),
            "Number of unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0 & cancer_status==1)),
            "Total number of completions"=length(which(expired==0 &cancer_status==1))
            
  )

d_noSocial_expirations <- noSocial_intervention_dt.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total population"=length(which(expired == 0))/length(expired)*100,
            "Navigated with NA"=-100,##length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1& neighbor_navigated == 1))*100,
            "Navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1 & neighbor_navigated == 0))*100,
            "Unnavigated with NA"=-100,##length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0& neighbor_navigated == 1))*100,
            "Unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,
            #note that the below are DIAGNOSES, not just completions
            "Number of navigated with NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1& cancer_status==1)),
            "Number of navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0& cancer_status==1)),
            "Number of unnavigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1& cancer_status==1)),
            "Number of unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0& cancer_status==1)),
            "Total number of completions"=length(which(expired==0& cancer_status==1))
  )

d_social_expirations <- intervention_dt.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total population"=length(which(expired == 0))/length(expired)*100,
            "Navigated with NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1& neighbor_navigated == 1))*100,
            "Navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1 & neighbor_navigated == 0))*100,
            "Unnavigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0& neighbor_navigated == 1))*100,
            "Unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,
            "Number of navigated with NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1& cancer_status==1)),
            "Number of navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0& cancer_status==1)),
            "Number of unnavigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1& cancer_status==1)),
            "Number of unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0& cancer_status==1)),
            "Total number of completions"=length(which(expired==0 & cancer_status==1))
  )
d_con_expirations <-cbind(d_con_expirations, "scenario"= "Control")
d_noSocial_expirations <- cbind(d_noSocial_expirations, "scenario"= "Clinical Navigation")
d_social_expirations <- cbind(d_social_expirations, "scenario"= "Network Navigation")

colnames(d_con_expirations)     <- c("Instance", "Percent_expired","Percent_completed","Total_population", 
                                     "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate",
                                     "Number_of_navigated_with_NA","Number_of_navigated_no_NA","Number_of_unnavigated_with_NA","Number_of_unnavigated_no_NA","n_total_completions","Scenario")

colnames(d_noSocial_expirations)<- c("Instance", "Percent_expired","Percent_completed","Total_population", 
                                     "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate",
                                     "Number_of_navigated_with_NA","Number_of_navigated_no_NA","Number_of_unnavigated_with_NA","Number_of_unnavigated_no_NA","n_total_completions","Scenario")

colnames(d_social_expirations)  <- c("Instance", "Percent_expired","Percent_completed","Total_population", 
                                     "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate",
                                     "Number_of_navigated_with_NA","Number_of_navigated_no_NA","Number_of_unnavigated_with_NA","Number_of_unnavigated_no_NA","n_total_completions","Scenario")

d_total_expirations <- rbind(d_con_expirations, d_noSocial_expirations, d_social_expirations)

#Collect diagnostic data for later 95CI calculation
tot <- d_total_expirations %>%
  group_by(Scenario) %>%
  select(Instance, 
         unnavigated_no_NA_completion_rate,
         unnavigated_NA_completion_rate,
         navigated_no_NA_completion_rate,
         navigated_NA_completion_rate,
         n_total_completions,
         Percent_completed
  )

#Old Plot
  # d <- ggplot()+ #TODO stratify
  # geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_NA_completion_rate, color="Navigated with NA"))+
  # geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_no_NA_completion_rate, color="Navigated no NA"))+
  # geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_NA_completion_rate, color="Unnavigated with NA"))+
  # geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_no_NA_completion_rate, color="Unnavigated no NA"))+
  # geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_no_NA_completion_rate, color="Total Population"))+
  # scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
  # ylab("Diagnostic Referral Completion Rate (%)")+
  # theme_bw()
  # 
  # plot_grid(ncol=2,nrow=1, d+ylim(0,100)+theme(legend.position="none"),d+ylab("")+ylim(75,100))

  
#Help from zyke on discoRd
diagnostic_data_long <- 
  d_total_expirations %>% 
  pivot_longer(
    cols = c(unnavigated_no_NA_completion_rate,
             navigated_no_NA_completion_rate, 
             unnavigated_NA_completion_rate, 
             navigated_NA_completion_rate
    ), 
    names_to = 'group', 
    values_to = 'values'
  )

  #version with non-overlapping boxplots
d2<-ggplot(diagnostic_data_long, aes(
    y = values, 
    x = Scenario,
    color = group
  )) +
    geom_boxplot()+
    ylab("Diagnostic Referral Completion Rate (%)")+  
    labs(group = "Treatment")+ #TODO figure out why this doesn't work
    theme_bw()+
    labs(color = "Treatment")+
    scale_color_discrete(name="Treatment", labels = c("Navigated with NN", 
                                                 "Navigated without NN", 
                                                 "Unnavigated with NN",
                                                 "Unnavigated without NN"))+
    annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -100, ymax = 200,
             alpha = .2)+
    coord_cartesian(xlim = c(1,3), ylim = c(0,100))

#non-overlapping boxplots
#plot_grid(ncol=2,nrow=1, d2+ylim(0,100)+theme(legend.position="none"),d2+ylab("")+ylim(75,100))

#Collect and format SCREENING data for each scenario
s_con_expirations <- control_sc.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total population"=length(which(expired == 0))/length(expired)*100,
            "Navigated with NA"=-100,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,#length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1& neighbor_navigated == 1))*100,
            "Navigated no NA"=-100,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,#length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1 & neighbor_navigated == 0))*100,
            "Unnavigated with NA"=-100,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0& neighbor_navigated == 1))*100,
            "Unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,
            "Number of navigated with NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)),
            "Number of navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)),
            "Number of unnavigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)),
            "Number of unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0))
            
            ) 
s_noSocial_expirations <- noSocial_intervention_sc.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total population"=length(which(expired == 0))/length(expired)*100,
            "Navigated with NA"=-100,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,#length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1& neighbor_navigated == 1))*100,
            "Navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1 & neighbor_navigated == 0))*100,
            "Unnavigated with NA"=-100,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,#length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0& neighbor_navigated == 1))*100,
            "Unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,
            "Number of navigated with NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)),
            "Number of navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)),
            "Number of unnavigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)),
            "Number of unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0))
            
            ) 

s_social_expirations <- intervention_sc.df %>% 
  filter(time >= 60) %>%
  group_by(instance) %>% 
  summarize("Percent_expired"=sum(expired)/length(expired)*100, 
            "Percent_completed"=100-(sum(expired)/length(expired)*100),
            "Total population"=length(which(expired == 0))/length(expired)*100,
            "Navigated with NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 1& neighbor_navigated == 1))*100,
            "Navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 1 & neighbor_navigated == 0))*100,
            "Unnavigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)) / length(which(navigated == 0& neighbor_navigated == 1))*100,
            "Unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0)) / length(which(navigated == 0 & neighbor_navigated == 0))*100,
            "Number of navigated with NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 1)),
            "Number of navigated no NA"=length(which(navigated == 1 & expired == 0 & neighbor_navigated == 0)),
            "Number of unnavigated with NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 1)),
            "Number of unnavigated no NA"=length(which(navigated == 0 & expired == 0 & neighbor_navigated == 0))
            )   

s_con_expirations <-cbind(s_con_expirations, "scenario"= "Control")
s_noSocial_expirations <- cbind(s_noSocial_expirations, "scenario"= "Clinical Navigation")
s_social_expirations <- cbind(s_social_expirations, "scenario"= "Network Navigation")

colnames(s_con_expirations)     <- c("Instance", "Percent_expired","Percent_completed","Total_population", "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate",
                                     "Number_of_navigated_with_NA","Number_of_navigated_no_NA","Number_of_unnavigated_with_NA","Number_of_unnavigated_no_NA","Scenario")
colnames(s_noSocial_expirations)<- c("Instance", "Percent_expired","Percent_completed","Total_population", "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate",
                                     "Number_of_navigated_with_NA","Number_of_navigated_no_NA","Number_of_unnavigated_with_NA","Number_of_unnavigated_no_NA","Scenario")
colnames(s_social_expirations)  <- c("Instance", "Percent_expired","Percent_completed","Total_population", "navigated_NA_completion_rate", "navigated_no_NA_completion_rate","unnavigated_NA_completion_rate", "unnavigated_no_NA_completion_rate",
                                     "Number_of_navigated_with_NA","Number_of_navigated_no_NA","Number_of_unnavigated_with_NA","Number_of_unnavigated_no_NA","Scenario")

s_total_expirations <- rbind(s_con_expirations, s_noSocial_expirations, s_social_expirations)

#Old version
# s <- ggplot()+
#   geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_NA_completion_rate, color="Navigated with NA"))+
#   geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=navigated_no_NA_completion_rate, color="Navigated no NA"))+
#   geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_NA_completion_rate, color="Unnavigated with NA"))+
#   geom_boxplot(nun_total_expirations, mapping = aes(group=Scenario, y=unnavigated_no_NA_completion_rate, color="Unnavigated no NA"))+
#   scale_x_continuous(breaks= c(-0.25,0,0.25),labels=c("Control", "Institutional Only", "Institutional and Social"))+
#   ylab("Screening Referral Completion Rate (%)")+
#   theme_bw()+
#   labs(color = "Treatment")

#s  
#plot_grid(nrow=1,ncol=2, s +ylim(0,100) +theme(legend.position = "none"), s+ylab(""))

screening_data_long <- 
  s_total_expirations %>% 
  pivot_longer(
    cols = c(unnavigated_no_NA_completion_rate,
             navigated_no_NA_completion_rate, 
             unnavigated_NA_completion_rate, 
             navigated_NA_completion_rate
    ), 
    names_to = 'group', 
    values_to = 'values'
  )

s2<-ggplot(screening_data_long, aes(
  y = values, 
  x = Scenario,
  color = group
)) +
  geom_boxplot()+
  #stat_summary(fun=mean, geom="point", shape=10, size=2) +
  ylab("Screening Referral Completion Rate (%)")+  
  theme_bw()+
  labs(color = "Treatment")+
  scale_color_discrete("Treatment", labels = c("Navigated with NN", 
                                               "Navigated without NN", 
                                               "Unnavigated with NN",
                                               "Unnavigated without NN"))+
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -100, ymax = 200,
           alpha = .2)+
  coord_cartesian(xlim = c(1,3), ylim = c(0,100))
#geom_rect(data=NULL,aes(xmin=1.5,xmax=2.5,ymin=-Inf,ymax=Inf), alpha=.5,
#                    fill="lightgray")

plot_grid(ncol=1,nrow=2, s2+xlab(""),d2)

#Plotting total pop SCREENING
s_total<-ggplot(screening_data_long, aes(
                y = Total_population, 
                x = Scenario#,
                #color = group
         )) +
         geom_boxplot()+
         ylab("Total Pop. Screening Completion Rate (%)")+  
         theme_bw()+
         annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -100, ymax = 200,
                  alpha = .2)+
         coord_cartesian(xlim = c(1,3), ylim = c(0,100))

#Plotting total pop DIAGNOSTIC
d_total<-ggplot(diagnostic_data_long, aes(
         y = Total_population, 
         x = Scenario#,
         #color = group
         )) +
         geom_boxplot()+
         ylab("Total Pop. Diagnostic Completion Rate (%)")+  
         theme_bw()+
         annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -100, ymax = 200,
                  alpha = .2)+
         coord_cartesian(xlim = c(1,3), ylim = c(0,100))

plot_grid(ncol=1,nrow=2, s_total+xlab(""), d_total)



#Confidence intervals | Switched to t-dist from normal dist
cicalc <- function(data){
  x_bar <- mean(data)
  sigma <- sd(data)
  n.sim <- length(data)
  #error <- qnorm(0.975)*s/sqrt(n)
  A = qt(0.975, df=n.sim-1)*sigma/sqrt(n.sim)
  left <- x_bar - A
  right <- x_bar + A
  ci_interval <- c(round(left,digits=2),round(right,digits=2))
  return(ci_interval)
}
#Screening 95CI
tots <- s_total_expirations %>%
  group_by(Scenario) %>%
  select(Instance, 
         unnavigated_no_NA_completion_rate,
         unnavigated_NA_completion_rate,
         navigated_no_NA_completion_rate,
         navigated_NA_completion_rate,
         Number_of_navigated_with_NA,  #Not needed for screenings
         Number_of_navigated_no_NA,    #
         Number_of_unnavigated_with_NA,#
         Number_of_unnavigated_no_NA,  #
         Total_population,
         Percent_completed
  )

#Diagnostic 95CI
tot <- d_total_expirations %>%
  group_by(Scenario) %>%
  select(Instance, 
         #2
         unnavigated_no_NA_completion_rate,
         #3
         unnavigated_NA_completion_rate,
         #4
         navigated_no_NA_completion_rate,
         #5
         navigated_NA_completion_rate,
         #6
         Number_of_unnavigated_no_NA,
         #7
         Number_of_unnavigated_with_NA,
         #8
         Number_of_navigated_no_NA,
         #9
         Number_of_navigated_with_NA,
         #10
         n_total_completions,
         #11
         Percent_completed
         #12
         )
tot[7]=tot[7]/tot[11]
tot[8]=tot[8]/tot[11]
tot[9]=tot[9]/tot[11]
tot[10]=tot[10]/tot[11]

##Percent of diagnoses represented by each treatment group within scenario
#unnav_no_NA
cat(round(mean(unlist(tot[1:30,7]))*100,1), "[95%CI (",cicalc(unlist(tot[1:30,7]))*100,")]")
cat(round(mean(unlist(tot[30:60,7]))*100,1),"[95%CI (",cicalc(unlist(tot[30:60,7]))*100,")]")
cat(round(mean(unlist(tot[60:90,7]))*100,1),"[95%CI (",cicalc(unlist(tot[60:90,7]))*100,")]")
#unnav_NAss
cat(round(mean(unlist(tot[1:30,8])) *100,1), "[95%CI (",cicalc(unlist(tot[1:30,8]))*100,")]")
cat(round(mean(unlist(tot[30:60,8]))*100,1),"[95%CI (",cicalc(unlist(tot[30:60,8]))*100,")]")
cat(round(mean(unlist(tot[60:90,8]))*100,1),"[95%CI (",cicalc(unlist(tot[60:90,8]))*100,")]")
#nav_no_NAss
cat(round(mean(unlist(tot[1:30,9])) *100,1), "[95%CI (",cicalc(unlist(tot[1:30,9]))*100,")]")
cat(round(mean(unlist(tot[30:60,9]))*100,1),"[95%CI (",cicalc(unlist(tot[30:60,9]))*100,")]")
cat(round(mean(unlist(tot[60:90,9]))*100,1),"[95%CI (",cicalc(unlist(tot[60:90,9]))*100,")]")
#nav_NAss
cat(round(mean(unlist(tot[1:30,10])) *100,1), "[95%CI (",cicalc(unlist(tot[1:30,10]))*100,")]")
cat(round(mean(unlist(tot[30:60,10]))*100,1),"[95%CI (",cicalc(unlist(tot[30:60,10]))*100,")]")
cat(round(mean(unlist(tot[60:90,10]))*100,1),"[95%CI (",cicalc(unlist(tot[60:90,10]))*100,")]")




#95CI for total population SCREENING completion
cat(round(mean(unlist(tots[1:30,11])),2), "[95%CI (",cicalc(unlist(tots[1:30,11])),")]")
cat(round(mean(unlist(tots[30:60,11])),2),"[95%CI (",cicalc(unlist(tots[30:60,11])),")]")
cat(round(mean(unlist(tots[60:90,11])),2),"[95%CI (",cicalc(unlist(tots[60:90,11])),")]")


#95CI for total population DIAGNOSTIC completion
#total population
cat(round(mean(unlist(tot[1:30,12])),2),"[95%CI (",cicalc(unlist(tot[1:30,12])),")]")
cat(round(mean(unlist(tot[30:60,12])),2),"[95%CI (",cicalc(unlist(tot[30:60,12])),")]")
cat(round(mean(unlist(tot[60:90,12])),2),"[95%CI (",cicalc(unlist(tot[60:90,12])),")]")


#Screening referral completion rate mean and 95%CI in each strata for all 3 scenarios
#unnav_no_NA
cat(round(mean(unlist(tots[1:30,3])),2), "[95%CI (",cicalc(unlist(tots[1:30,3])),")]")
cat(round(mean(unlist(tots[30:60,3])),2),"[95%CI (",cicalc(unlist(tots[30:60,3])),")]")
cat(round(mean(unlist(tots[60:90,3])),2),"[95%CI (",cicalc(unlist(tots[60:90,3])),")]")
#unnav_NAss
cat(round(mean(unlist(tots[1:30,4])),2), "[95%CI (",cicalc(unlist(tots[1:30,4])),")]")
cat(round(mean(unlist(tots[30:60,4])),2),"[95%CI (",cicalc(unlist(tots[30:60,4])),")]")
cat(round(mean(unlist(tots[60:90,4])),2),"[95%CI (",cicalc(unlist(tots[60:90,4])),")]")
#nav_no_NAss
cat(round(mean(unlist(tots[1:30,5])),2), "[95%CI (",cicalc(unlist(tots[1:30,5])),")]")
cat(round(mean(unlist(tots[30:60,5])),2),"[95%CI (",cicalc(unlist(tots[30:60,5])),")]")
cat(round(mean(unlist(tots[60:90,5])),2),"[95%CI (",cicalc(unlist(tots[60:90,5])),")]")
#nav_NAss
cat(round(mean(unlist(tots[1:30,6])),2), "[95%CI (",cicalc(unlist(tots[1:30,6])),")]")
cat(round(mean(unlist(tots[30:60,6])),2),"[95%CI (",cicalc(unlist(tots[30:60,6])),")]")
cat(round(mean(unlist(tots[60:90,6])),2),"[95%CI (",cicalc(unlist(tots[60:90,6])),")]")
##STAGE AT DX SUBPOP BREAKDOWN 
#unnav_no_NA
cat(round(mean(unlist(tot[1:30,7]))*100,1), "[95%CI (",cicalc(unlist(tot[1:30,7]))*100,")]")
cat(round(mean(unlist(tot[30:60,7]))*100,1),"[95%CI (",cicalc(unlist(tot[30:60,7]))*100,")]")
cat(round(mean(unlist(tot[60:90,7]))*100,1),"[95%CI (",cicalc(unlist(tot[60:90,7]))*100,")]")
#unnav_NAss
cat(round(mean(unlist(tot[1:30,8])) *100,1), "[95%CI (",cicalc(unlist(tot[1:30,8]))*100,")]")
cat(round(mean(unlist(tot[30:60,8]))*100,1),"[95%CI (",cicalc(unlist(tot[30:60,8]))*100,")]")
cat(round(mean(unlist(tot[60:90,8]))*100,1),"[95%CI (",cicalc(unlist(tot[60:90,8]))*100,")]")
#nav_no_NAss
cat(round(mean(unlist(tot[1:30,9])) *100,1), "[95%CI (",cicalc(unlist(tot[1:30,9]))*100,")]")
cat(round(mean(unlist(tot[30:60,9]))*100,1),"[95%CI (",cicalc(unlist(tot[30:60,9]))*100,")]")
cat(round(mean(unlist(tot[60:90,9]))*100,1),"[95%CI (",cicalc(unlist(tot[60:90,9]))*100,")]")
#nav_NAss
cat(round(mean(unlist(tot[1:30,10])) *100,1), "[95%CI (",cicalc(unlist(tot[1:30,10]))*100,")]")
cat(round(mean(unlist(tot[30:60,10]))*100,1),"[95%CI (",cicalc(unlist(tot[30:60,10]))*100,")]")
cat(round(mean(unlist(tot[60:90,10]))*100,1),"[95%CI (",cicalc(unlist(tot[60:90,10]))*100,")]")

#Diagnostic referral completion rate mean and 95%CI in each strata for all 3 scenarios
#Note that tot is set far above while the diagnostic data is loaded into nun_total_expirations
#unnav_no_NA
cat(round(mean(unlist(tot[1:30,3])),2), "[95%CI (",cicalc(unlist(tot[1:30,3])),")]")
cat(round(mean(unlist(tot[30:60,3])),2),"[95%CI (",cicalc(unlist(tot[30:60,3])),")]")
cat(round(mean(unlist(tot[60:90,3])),2),"[95%CI (",cicalc(unlist(tot[60:90,3])),")]")
#unnav_NA
cat(round(mean(unlist(tot[1:30,4])),2),"[95%CI (",cicalc(unlist(tot[1:30,4])),")]")
cat(round(mean(unlist(tot[30:60,4])),2),"[95%CI (",cicalc(unlist(tot[30:60,4])),")]")
cat(round(mean(unlist(tot[60:90,4])),2),"[95%CI (",cicalc(unlist(tot[60:90,4])),")]")
#nav_no_NA
cat(round(mean(unlist(tot[1:30,5])),2),"[95%CI (",cicalc(unlist(tot[1:30,5])),")]")
cat(round(mean(unlist(tot[30:60,5])),2),"[95%CI (",cicalc(unlist(tot[30:60,5])),")]")
cat(round(mean(unlist(tot[60:90,5])),2),"[95%CI (",cicalc(unlist(tot[60:90,5])),")]")
#nav_NA
cat(round(mean(unlist(tot[1:30,6])),2),"[95%CI (",cicalc(unlist(tot[1:30,6])),")]")
cat(round(mean(unlist(tot[30:60,6])),2),"[95%CI (",cicalc(unlist(tot[30:60,6])),")]")
cat(round(mean(unlist(tot[60:90,6])),2),"[95%CI (",cicalc(unlist(tot[60:90,6])),")]")


#Screening completion rate unnav w NA
cat(round(mean(unlist(tots[60:90,4])),2),"[95%CI (",cicalc(unlist(tots[60:90,4])),")]")
