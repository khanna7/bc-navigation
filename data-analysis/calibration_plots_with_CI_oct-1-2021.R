#Final plots for model calibration
#Bryan Brickman
#08/18/2021

# Libraries  ----------
rm(list=ls())

library(dplyr)
library(ggplot2)
library(network)
library(networkDynamic)

new_plots <- FALSE
old_plots <- TRUE

## Add names of data directories here
#Directory name format is {date}_full_run
bc_navigation_root <- '/project2/ahotton/bryanb/bc-navigation/dec9_navlength/bc-navigation/'
#date <- "13:41:19_2021-04-06"#pre social nav update
date<-"13:26:26_2021-07-02"
full_run_name <- paste0(date, '_burnin_runs)/')

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


#NEEDS UPDATING TO WORK WITH CURRENT OUTPUTS (July 7, 2021)
#copy over the dt_columns definition from Final_Results_Plots.R
# dt_columns <- c(
#   #see `write.table` in https://github.com/khanna7/bc-navigation/blob/master/demography-reset.R for col names
#   "time", #TODO set to "time-step"
#   "nintros", #deaths
#   "number.of.positive.bc.agents",
#   "number.of.hpos.agents",
#   "number.of.hneg.agents",
#   
#   "number.of.diagnosed.cases",
#   "number.of.diagnostic.referrals",
#   "number.of.screening.referrals",
#   "number.of.screen.completed",
#   "number.of.dt.completed",
#   
#   "number.of.symptomatic",
#   "number.of.navigated.agents",
#   "time.with.cancer",
#   "time.until.diagnosis",
#   "time.until.diagnosis.navigated",
#   
#   "time.until.diagnosis.unnavigated",
#   "time.until.diagnosis.neigbor.navigated",
#   "number.of.diagnostic.referrals.at.t",
#   "number.of.screening.visits.at.t",#19
#   "number.of.ss0.diagnosed",#20
#   
#   "number.of.ss1.diagnosed",#21
#   "number.of.ss2.diagnosed",#22
#   "number.of.ss3.diagnosed",#23
#   "number.of.ss0.diagnosed.navigated",#24
#   "number.of.ss1.diagnosed.navigated",#25
#   
#   "number.of.ss2.diagnosed.navigated",#26
#   "number.of.ss3.diagnosed.navigated",#27
#   "number.of.ss0.diagnosed.unnavigated",#28
#   "number.of.ss1.diagnosed.unnavigated",#29
#   "number.of.ss2.diagnosed.unnavigated",#30
#   
#   "number.of.ss3.diagnosed.unnavigated",#31
#   "number.of.ss0.diagnosed.neighbor_navigated", #32
#   "number.of.ss1.diagnosed.neighbor_navigated", #33
#   "number.of.ss2.diagnosed.neighbor_navigated", #34
#   "number.of.ss3.diagnosed.neighbor_navigated", #35
#   
#   "number.of.bc.onsets", #36
#   "number.of.screening.referrals.at.t",#37
#   "number.of.expired.diagnostic.referrals.at.t",#38
#   "number.of.expired.screening.referrals.at.t" #39
# )

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

colnames(control.df) <- dt_columns
colnames(intervention.df) <- dt_columns
colnames(noSocial_intervention.df) <- dt_columns

colnames(control_dt.df) <- diagnostic_event_columns
colnames(intervention_dt.df) <- diagnostic_event_columns
colnames(noSocial_intervention_dt.df) <- diagnostic_event_columns

#colnames(control_sc.df) <- screening_event_columns
#colnames(intervention_sc.df) <- screening_event_columns
#colnames(noSocial_intervention_sc.df) <- screening_event_columns

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
    m_number.of.dt.completed = mean(number.of.dt.completed),
    m_number.of.screen.completed = mean(number.of.screen.completed),
    
    m_number.of.hpos.agents = mean(number.of.hpos.agents),
    m_number.of.hneg.agents = mean(number.of.hneg.agents),
    m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
    m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
    m_nintros = mean(nintros),
    
    m_early.diagnosed.ratio = mean((number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases),
    m_late.diagnosed.ratio = mean((number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases),
    #m_number.of.expired.screening.referrals = mean(number.of.expired.screening.referrals.at.t),
    #m_number.of.expired.diagnostic.referrals = mean(number.of.expired.diagnostic.referrals.at.t)
    
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
    m_number.of.dt.completed = mean(number.of.dt.completed),
    m_number.of.screen.completed = mean(number.of.screen.completed),
    m_number.of.hpos.agents = mean(number.of.hpos.agents),
    m_number.of.hneg.agents = mean(number.of.hneg.agents),
    m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
    m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
    m_nintros = mean(nintros),
    m_early.diagnosed.ratio = mean((number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases),
    m_late.diagnosed.ratio = mean((number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases),
    #m_number.of.expired.screening.referrals = mean(number.of.expired.screening.referrals.at.t),
    #m_number.of.expired.diagnostic.referrals = mean(number.of.expired.diagnostic.referrals.at.t)
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
    m_number.of.screening.referrals.at.tscreening.referrals.at.t = mean(number.of.screening.referrals.at.t),
    m_number.of.screening.referrals = mean(number.of.screening.referrals),
    m_number.of.dt.completed = mean(number.of.dt.completed),
    m_number.of.screen.completed = mean(number.of.screen.completed),
    m_number.of.hpos.agents = mean(number.of.hpos.agents),
    m_number.of.hneg.agents = mean(number.of.hneg.agents),
    m_number.of.diagnosed.cases = mean(number.of.diagnosed.cases),
    m_number.of.positive.bc.agents = mean(number.of.positive.bc.agents),
    m_nintros = mean(nintros),
    m_early.diagnosed.ratio = mean((number.of.ss0.diagnosed+number.of.ss1.diagnosed)/number.of.diagnosed.cases),
    m_late.diagnosed.ratio = mean((number.of.ss2.diagnosed+number.of.ss3.diagnosed)/number.of.diagnosed.cases),
    #m_number.of.expired.screening.referrals = mean(number.of.expired.screening.referrals.at.t),
    #m_number.of.expired.diagnostic.referrals = mean(number.of.expired.diagnostic.referrals.at.t)
  )

#set colors for ggplot
con<-"Control"
ins<-"Clinical Navigation"
soc<-"Network Navigation"

#Calculates confidence intervals | used t-distribution
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

lower_cicalc <- function(data){
  x_bar <- mean(data)
  sigma <- sd(data)
  n.sim <- length(data)
  #error <- qnorm(0.975)*s/sqrt(n)
  A = qt(0.975, df=n.sim-1)*sigma/sqrt(n.sim)
  left <- x_bar - A
  return(round(left,digits=2))
}

upper_cicalc <- function(data){
  x_bar <- mean(data)
  sigma <- sd(data)
  n.sim <- length(data)
  #error <- qnorm(0.975)*s/sqrt(n)
  A = qt(0.975, df=n.sim-1)*sigma/sqrt(n.sim)
  right <- x_bar + A
  return(round(right,digits=2))
}

# The following two functions give upper and lower values for generating ribbon plots representing 95% confidence intervals
# passing an argument to `optional_denominator_column` returns the CI values for the column / optional_denominator_column
time_upper <- function(data, column, optional_denominator_column=NULL){
    subset <- subset(data, select = c("time", column))
    n_pos_by_time <- split(subset, subset$time)
    ymax <- c()
    
    for(i in 1:360){
      ymax = append(ymax, upper_cicalc(n_pos_by_time[[i]][[column]]))
    }
    return(ymax)
}

time_lower <- function(data, column){
    subset <- subset(data, select = c("time", column))
    n_pos_by_time <- split(subset, subset$time)
    ymin <- c()
    
    for(i in 1:360){
      ymin = append(ymin, lower_cicalc(n_pos_by_time[[i]][[column]]))
    }
    return(ymin)
}

### END SETUP
#Breast Cancer Prevalence (%)
ggplot()+
  theme_bw()+
  
  geom_line(data=control.df, alpha=0.1, aes(x=time/12, y=((number.of.positive.bc.agents/N)*100),color=con))+
  geom_ribbon(data=control.df[1:360,], alpha=0.1, aes(x=time/12, y=((number.of.positive.bc.agents/N)*100), 
                                              ymin = time_lower(control.df, "number.of.positive.bc.agents")/N * 100, 
                                              ymax = time_upper(control.df, "number.of.positive.bc.agents")/N * 100,
                                              color = con))+
  geom_line(data=control.df_mean_at_time, aes(x=time/12, y=((m_number.of.positive.bc.agents/N)*100),color=con))+
  
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time/12, y=((number.of.positive.bc.agents/N)*100),color=ins))+
  geom_ribbon(data=noSocial_intervention.df[1:360,], alpha=0.1, aes(x=time/12, y=((number.of.positive.bc.agents/N)*100), 
                                                      ymin = time_lower(noSocial_intervention.df, "number.of.positive.bc.agents")/N * 100, 
                                                      ymax = time_upper(noSocial_intervention.df, "number.of.positive.bc.agents")/N * 100,
                                                      color = ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time/12, y=((m_number.of.positive.bc.agents/N)*100),color=ins))+
  
  geom_line(data=intervention.df, alpha=0.1,aes(x=time/12, y=((number.of.positive.bc.agents/N)*100), color=soc))+
  geom_ribbon(data=intervention.df[1:360,], alpha=0.1, aes(x=time/12, y=((number.of.positive.bc.agents/N)*100), 
                                                                    ymin = time_lower(intervention.df, "number.of.positive.bc.agents")/N * 100, 
                                                                    ymax = time_upper(intervention.df, "number.of.positive.bc.agents")/N * 100,
                                                                    color = soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time/12, y=((m_number.of.positive.bc.agents/N)*100), color=soc))+
  
  labs(y="Breast Cancer Prevalence (%)")+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits= c("Control", "Clinical Navigation", "Network Navigation")
  )+
  xlab("Time (years)")+
  ylim(c(1,3))



#number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t

ratio_lower = function(data){
  temp <- data %>% mutate(ratio = data$number.of.diagnostic.referrals.at.t / data$number.of.screening.visits.at.t)
  subset <- subset(temp, select = c("time", "ratio"))
  n_pos_by_time <- split(subset, subset$time)
  ymin <- c()
  
  for(i in 1:360){
    ymin = append(ymin, lower_cicalc(n_pos_by_time[[i]][["ratio"]]))
  }
  return(ymin)
}

ratio_upper = function(data){
  temp <- data %>% mutate(ratio = data$number.of.diagnostic.referrals.at.t / data$number.of.screening.visits.at.t)
  subset <- subset(temp, select = c("time", "ratio"))
  n_pos_by_time <- split(subset, subset$time)
  ymax <- c()
  
  for(i in 1:360){
    ymax = append(ymax, upper_cicalc(n_pos_by_time[[i]][["ratio"]]))
  }
  return(ymax)
}

ggplot()+
  theme_bw()+
  geom_line(data=control.df, alpha=0.1, aes(x=time/12, y=number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t * 100, color = con))+
  geom_ribbon(data=control.df[1:360,], alpha=0.1, aes(x=time/12,
                                                           ymin = ratio_lower(control.df) * 100,
                                                           ymax = ratio_upper(control.df) * 100,
                                                           color = con))+
  geom_line(data=control.df_mean_at_time, aes(x=time/12, y=m_number.of.diagnostic.referrals.at.t/m_number.of.screening.visits.at.t* 100, color = con))+

  geom_line(data=noSocial_intervention.df,alpha=0.1, aes(x=time/12, y=number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t* 100,color=ins))+
  geom_ribbon(data=noSocial_intervention.df[1:360,], alpha=0.1, aes(x=time/12,
                                                           ymin = ratio_lower(noSocial_intervention.df) * 100,
                                                           ymax = ratio_upper(noSocial_intervention.df) * 100,
                                                           color = ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time/12, y=m_number.of.diagnostic.referrals.at.t/m_number.of.screening.visits.at.t* 100,color=ins))+

  geom_line(data=intervention.df, alpha=0.1, aes(x=time/12, y=number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t* 100, color=soc))+
  geom_ribbon(data=intervention.df[1:360,], alpha=0.1, aes(x=time/12,
                                                           ymin = ratio_lower(intervention.df) * 100,
                                                           ymax = ratio_upper(intervention.df) * 100,
                                                           color = soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time/12, y=m_number.of.diagnostic.referrals.at.t/m_number.of.screening.visits.at.t* 100, color=soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("Control", "Clinical Navigation", "Network Navigation")
  )+
  xlab("Time (years)")+
  ylab("Screening Visit Recall Rate (%)")+
  ylim(c(0,25))

#Hormone subtype breakdown (in terms of simple count of cases)
ggplot(control.df)+
  geom_line(aes(x=time/12, y=number.of.hpos.agents), alpha=0.1)+
  geom_line(aes(x=time/12, y=number.of.hneg.agents), alpha=0.1)+
  geom_line(aes(x=time/12, y=number.of.positive.bc.agents), alpha=0.1)+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, aes(x=time/12, y=m_number.of.hpos.agents))+
  geom_ribbon(data=control.df[1:360,], alpha=0.1, color = "black", aes(x=time/12, y= number.of.hpos.agents, 
                                                      ymin = time_lower(control.df, "number.of.hpos.agents"), 
                                                      ymax = time_upper(control.df, "number.of.hpos.agents"),
                                                      color = "black"))+
  geom_line(data = control.df_mean_at_time, aes(x=time/12, y=m_number.of.hneg.agents))+
  geom_ribbon(data=control.df[1:360,], alpha=0.1, color = "black", aes(x=time/12, y=number.of.hneg.agents, 
                                                      ymin = time_lower(control.df, "number.of.hneg.agents"), 
                                                      ymax = time_upper(control.df, "number.of.hneg.agents"),
                                                      color = "black"))+
  geom_line(data = control.df_mean_at_time, aes(x=time/12, y=m_number.of.positive.bc.agents))+
  geom_ribbon(data=control.df[1:360,], alpha=0.1, color = "black", aes(x=time/12, y=number.of.positive.bc.agents, 
                                                      ymin = time_lower(control.df, "number.of.positive.bc.agents"), 
                                                      ymax = time_upper(control.df, "number.of.positive.bc.agents")))+
  ylim(c(0, 150))+
  annotate(geom="text", x=25, y=115, label="All cases", col="black")+
  annotate(geom="text", x=25, y=90, label="Hormone-positive", col="black")+
  annotate(geom="text", x=25, y=33, label="Hormone-negative", col="black")+
  labs(x = "Time (years)", y = "Number of Cases")

  #Hormone negative to hormone positive cancer ratio
h_ratio_lower = function(data){
  temp <- data %>% mutate(h_ratio = data$number.of.hneg.agents / data$number.of.hpos.agents)
  subset <- subset(temp, select = c("time", "h_ratio"))
  n_pos_by_time <- split(subset, subset$time)
  ymin <- c()
  
  for(i in 1:360){
    ymin = append(ymin, lower_cicalc(n_pos_by_time[[i]][["h_ratio"]]))
  }
  return(ymin)
}

h_ratio_upper = function(data){
  temp <- data %>% mutate(h_ratio = data$number.of.hneg.agents / data$number.of.hpos.agents)
  subset <- subset(temp, select = c("time", "h_ratio"))
  n_pos_by_time <- split(subset, subset$time)
  ymax <- c()
  
  for(i in 1:360){
    ymax = append(ymax, upper_cicalc(n_pos_by_time[[i]][["h_ratio"]]))
  }
  return(ymax)
}
ggplot()+
  theme_bw()+
  geom_line(data=control.df, alpha=0.1, aes(x=time/12, y=number.of.hneg.agents/number.of.hpos.agents,color=con))+
  geom_line(data=control.df_mean_at_time, aes(x=time/12, y=m_number.of.hneg.agents/m_number.of.hpos.agents,color=con))+
  geom_ribbon(data=control.df[1:360,], alpha=0.1, aes(x=time/12,
                                                      ymin = h_ratio_lower(control.df),
                                                      ymax = h_ratio_upper(control.df),
                                                      color = con))+
  geom_line(data=noSocial_intervention.df, alpha=0.1, aes(x=time/12, y=number.of.hneg.agents/number.of.hpos.agents,color=ins))+
  geom_line(data=noSocial_intervention.df_mean_at_time, aes(x=time/12, y=m_number.of.hneg.agents/m_number.of.hpos.agents,color=ins))+
  geom_ribbon(data=noSocial_intervention.df[1:360,], alpha=0.1, aes(x=time/12,
                                                                    ymin = h_ratio_lower(noSocial_intervention.df),
                                                                    ymax = h_ratio_upper(noSocial_intervention.df),
                                                                    color = ins))+
  geom_line(data=intervention.df, alpha=0.1, aes(x=time/12, y=number.of.hneg.agents/number.of.hpos.agents, color=soc))+
  geom_line(data=intervention.df_mean_at_time, aes(x=time/12, y=m_number.of.hneg.agents/m_number.of.hpos.agents, color=soc))+
  geom_ribbon(data=intervention.df[1:360,], alpha=0.1, aes(x=time/12,
                                                           ymin = h_ratio_lower(intervention.df),
                                                           ymax = h_ratio_upper(intervention.df),
                                                           color = soc))+
  scale_color_manual("Scenario", values = c('green','red','blue'),
                     limits = c("Control", "Clinical Navigation", "Network Navigation")
                     )+
  ylab("Hormone Negative to Hormone Positive Ratio")+
  xlab("Time (years)")+
  ylim(c(0,1))
  