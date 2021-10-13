#Plots of burnin data for model calibration
#Bryan Brickman
#10/04/2021

# Libraries
rm(list=ls())

library(dplyr)
library(ggplot2)
library(network)
library(networkDynamic)

# Paths to data
bc_navigation_root <- '/project2/ahotton/bryanb/bc-navigation/dec9_navlength/bc-navigation/'
date<-"12:52:07_2021-10-05"
full_run_name <- paste0(date, '_burnin_runs)/12:52:07_2021-10-05_burnins')

# Read data and set meta-parameters
N<-5000         #number of agents
n.instances<-30 #number of runs 
run_length<-360 #number of time steps/months in run

control_list <- as.list(1:n.instances)
control_dt_list <- as.list(1:n.instances)
control_sc_list <- as.list(1:n.instances)

# Column names for data outputs
dt_columns <- c(
  #see `write.table` in https://github.com/khanna7/bc-navigation/blob/master/demography-reset.R for col names
  "time",    #TODO set to "time-step"
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

# Create lists
for (i in 1:n.instances){
  control_list[[i]] <- read.table(paste0(bc_navigation_root, date, 
                                         '_burnin_runs/', date, 
                                         '_burnins/data/', 
                                         i,".data"))
}

for (i in 1:n.instances){
  control_dt_list[[i]] <- read.table(paste0(bc_navigation_root, date, 
                                            '_burnin_runs/', date, 
                                            '_burnins/diagnostic_event_logs/', 
                                            i,"_diagnostic.events"))
}

for (i in 1:n.instances){
  control_sc_list[[i]] <- 
    read.table(paste0(bc_navigation_root, date, 
                      '_burnin_runs/', date, 
                      '_burnins/diagnostic_event_logs/', 
                      i,"_screening.events"))
}

# Check if data is of the right length. (Returns which instances are missing data)
which(unlist(lapply(control_list, nrow) != run_length))

control.df <- bind_rows(control_list[1:n.instances])

control_dt.df <- bind_rows(control_dt_list[1:n.instances])

colnames(control.df) <- dt_columns

colnames(control_dt.df) <- diagnostic_event_columns

#colnames(control_sc.df) <- screening_event_columns

control.df$source <- rep(1:n.instances, each=run_length)


# Compute incidence rate
control.df <- 
  control.df %>%
  mutate(number.of.bc.neg = N - number.of.positive.bc.agents)  

# Compute means across variables at given time
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

# Set colors for ggplot
con<-"Burnin"

# Calculates confidence intervals (t-distribution)
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

# The following two functions give upper and lower values for generating 
#     ribbon plots representing 95% confidence intervals
time_upper <- function(data, column){
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

# Breast Cancer Prevalence (%)
ggplot()+
  theme_bw()+
  
  geom_line(data=control.df, alpha=0.1, aes(x=time/12, y=((number.of.positive.bc.agents/N)*100),color=con))+
  geom_ribbon(data=control.df[1:360,], alpha=0.1, aes(x=time/12, y=((number.of.positive.bc.agents/N)*100), 
                                              ymin = time_lower(control.df, "number.of.positive.bc.agents")/N * 100, 
                                              ymax = time_upper(control.df, "number.of.positive.bc.agents")/N * 100,
                                              color = con))+
  geom_line(data=control.df_mean_at_time, aes(x=time/12, y=((m_number.of.positive.bc.agents/N)*100),color=con))+
  labs(y="Breast Cancer Prevalence (%)")+
  scale_color_manual("Scenario", values = c('black'),
                     limits= c("Burnin")
  )+
  xlab("Time (years)")#+
    #ylim(c(1,3))

# Cancer prevalence final timestep mean and 95% CI:
prev_upper_ci <- time_upper(control.df,"number.of.positive.bc.agents")/N * 100
prev_lower_ci <- time_lower(control.df,"number.of.positive.bc.agents")/N * 100
prev_mean <- mean(dplyr::filter(control.df, time == 360)$number.of.positive.bc.agents / N * 100)

cat(paste0("Prevalence (%): ", round(prev_mean, 2), 
           " 95%CI: ", "(", round(prev_lower_ci[360], 2), ", ", round(prev_upper_ci[360], 2), ")"))

# Screening Recall Rate
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
  scale_color_manual("Scenario", values = c('black'),
                     limits = c("Burnin")
  )+
  xlab("Time (years)")+
  ylab("Screening Visit Recall Rate (%)")+
  ylim(c(0,25))

# Recall rate final timestep mean and 95% CI:
recall_upper_ci <- ratio_upper(control.df)
recall_lower_ci <- ratio_lower(control.df)
recall_mean <- mean((dplyr::filter(control.df, time == 360)$number.of.diagnostic.referrals.at.t / 
                     dplyr::filter(control.df, time == 360)$number.of.screening.visits.at.t))

cat(paste0("Recall Rate (%): ", round(recall_mean, 2), 
           " 95%CI: ", "(", round(recall_lower_ci[360], 2), ", ", round(recall_upper_ci[360], 2), ")"))


# Hormone subtype breakdown (in terms of simple count of cases)
ggplot(control.df)+
  geom_line(aes(x=time/12, y=number.of.hpos.agents), alpha=0.1)+
  geom_line(aes(x=time/12, y=number.of.hneg.agents), alpha=0.1)+
  geom_line(aes(x=time/12, y=number.of.positive.bc.agents), alpha=0.1)+
  theme_bw()+
  geom_line(data = control.df_mean_at_time, 
            aes(x=time/12, y=m_number.of.hpos.agents)
            )+
  geom_ribbon(data=control.df[1:360,], alpha=0.1, color = "black", 
              aes(x=time/12, y= number.of.hpos.agents, 
                  ymin = time_lower(control.df, "number.of.hpos.agents"), 
                  ymax = time_upper(control.df, "number.of.hpos.agents"),
                  color = "black")
              )+
  geom_line(data = control.df_mean_at_time,
            aes(x=time/12, y=m_number.of.hneg.agents)
            )+
  geom_ribbon(data=control.df[1:360,], alpha=0.1, color = "black", 
              aes(x=time/12, y=number.of.hneg.agents, 
                  ymin = time_lower(control.df, "number.of.hneg.agents"), 
                  ymax = time_upper(control.df, "number.of.hneg.agents"),
                  color = "black")
              )+
  geom_line(data = control.df_mean_at_time, 
            aes(x=time/12, y=m_number.of.positive.bc.agents)
            )+
  geom_ribbon(data=control.df[1:360,], alpha=0.1, color = "black", 
              aes(x=time/12, y=number.of.positive.bc.agents, 
                  ymin = time_lower(control.df, "number.of.positive.bc.agents"), 
                  ymax = time_upper(control.df, "number.of.positive.bc.agents"))
              )+
  ylim(c(0, 150))+
  annotate(geom="text", x=25, y=115, label="All cases", col="black")+
  annotate(geom="text", x=25, y=90, label="Hormone-positive", col="black")+
  annotate(geom="text", x=25, y=33, label="Hormone-negative", col="black")+
  labs(x = "Time (years)", y = "Number of Cases")

  # Hormone negative to hormone positive cancer ratio
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
  scale_color_manual("Scenario", values = c('black'),
                     limits = c("Burnin")
                     )+
  ylab("Hormone Negative to Hormone Positive Ratio")+
  xlab("Time (years)")+
  ylim(c(0,1))
  
# Hormone ratio final timestep mean and 95% CI:
h_ratio_upper_ci <- h_ratio_upper(control.df)
h_ratio_lower_ci <- h_ratio_lower(control.df)
h_ratio_mean <- mean((dplyr::filter(control.df, time == 360)$number.of.hneg.agents / 
                       dplyr::filter(control.df, time == 360)$number.of.hpos.agents))

cat(paste0("Hormone Neg. to Hormone Pos. Ratio: ", round(h_ratio_mean, 2), 
           " 95%CI: ", "(", round(h_ratio_lower_ci[360], 2), ", ", round(h_ratio_upper_ci[360], 2), ")"))

    
# Age distributions: TODO
# age-at-end <- 

