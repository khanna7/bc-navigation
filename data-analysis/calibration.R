rm(list=ls())

library(dplyr)
library(ggplot2)

n.instances <- 30

dt_list <- as.list(1:n.instances)

dt_columns <- c(
  #see `write.table` in https://github.com/khanna7/bc-navigation/blob/master/demography-reset.R for col names
  "time",
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
  "number.of.ss3.diagnosed.neighbor_navigated" #35
  )

for (i in 1:n.instances){
  dt_list[[i]] <- read.table(paste0("../data/07_05_intervention100yr/data/", i,".data"))
}

which(unlist(lapply(dt_list, nrow) != 1200))

df <- bind_rows(dt_list[1:n.instances])
colnames(df) <- dt_columns
df$source <- rep(1:n.instances, each=1200)


# Compute means across variables at given time ----------

df_mean_at_time <- 
  df %>% 
  group_by(time) %>%
  summarise(m_number.of.screening.visits.at.t = mean(number.of.screening.visits.at.t),
            m_number.of.diagnostic.referrals.at.t = mean(number.of.diagnostic.referrals.at.t),
            m_number.of.dt.completed = mean(number.of.dt.completed),
            m_number.of.screen.completed = mean(number.of.screen.completed)
            )


# Plots ----------

ggplot(df, aes(x=time, y=number.of.screening.visits.at.t))+
  geom_line(alpha=0.1)+
  theme_bw()+
  geom_line(data = df_mean_at_time, aes(x=time, y=m_number.of.screening.visits.at.t))

ggplot(df, aes(x=time, y=number.of.diagnostic.referrals.at.t))+
  geom_line(alpha=0.1)+
  theme_bw()+
  geom_line(data = df_mean_at_time, aes(x=time, y=m_number.of.diagnostic.referrals.at.t))

ggplot(df, aes(x=time, y=number.of.diagnostic.referrals.at.t/number.of.screening.visits.at.t))+
  geom_line(alpha=0.1)+
  theme_bw()+
  geom_line(data = df_mean_at_time, aes(x=time, y=m_number.of.diagnostic.referrals.at.t/m_number.of.screening.visits.at.t))+
  ylim(c(0,1))

ggplot(df, aes(x=time, y=number.of.dt.completed/number.of.screen.completed))+
  geom_line(alpha=0.1)+
  theme_bw()+
  geom_line(data = df_mean_at_time, aes(x=time, y=m_number.of.dt.completed/m_number.of.screen.completed))
  
ggplot(df, aes(x=time, y=number.of.dt.completed))+
  geom_line(alpha=0.1)+
  theme_bw()

ggplot(df, aes(x=time, y=number.of.hpos.agents, 
               colour=as.factor(source)))+
  geom_line()+
  theme_bw()

ggplot(df, aes(x=time, y=number.of.hneg.agents, 
               colour=as.factor(source)))+
  geom_line()+
  theme_bw()

ggplot(df, aes(x=time, y=number.of.hneg.agents/number.of.hpos.agents, 
               colour=as.factor(source)))+
  geom_line()+
  theme_bw()

ggplot(df, aes(x=time, y=number.of.diagnosed.cases, 
               colour=as.factor(source)))+
  geom_line()+
  theme_bw()

ggplot(df, aes(x=time, y=number.of.positive.bc.agents, 
               colour=as.factor(source)))+
  geom_line()+
  theme_bw()

ggplot(df, aes(x=time, y=nintros, 
               colour=as.factor(source)))+
  geom_line()+
  theme_bw()

