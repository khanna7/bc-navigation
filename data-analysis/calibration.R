rm(list=ls())

library(dplyr)
library(ggplot2)

n.instances <- 30

dt_list <- as.list(1:n.instances)

dt_columns <- c(
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
  "time.until.diagnosis.unnavigate"
#  "number.of.diagnostic.referrals"
)

for (i in 1:n.instances){
  dt_list[[i]] <- read.table(paste0("../06_08_Burnin_data_round2/", i,".data"), col.names = dt_columns)
}

df <- bind_rows(dt_list[1:5])
df$source <- rep(1:5, each=360)

ggplot(df, aes(x=time, y=number.of.screening.referrals, 
               colour=as.factor(source)))+
  geom_line()+
  theme_bw()

ggplot(df, aes(x=time, y=number.of.screen.completed/number.of.screening.referrals, 
               colour=as.factor(source)))+
  geom_line()+
  theme_bw()

ggplot(df, aes(x=time, y=number.of.diagnostic.referrals, 
               colour=as.factor(source)))+
  geom_line()+
  theme_bw()

ggplot(df, aes(x=time, y=number.of.dt.completed/number.of.screen.completed, 
               colour=as.factor(source)))+
  geom_line()+
  theme_bw()
  
ggplot(df, aes(x=time, y=number.of.dt.completed, 
               colour=as.factor(source)))+
  geom_line()+
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
