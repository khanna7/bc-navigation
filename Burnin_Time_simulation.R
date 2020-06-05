##Time loop 

rm(list=ls())
setwd("~/projects/bc-navigation")

library(networkDynamic)

source('parameters.R')
source('estimation.R')
source('disease-progression.R')
source('clinical-engagement.R')
source('demography-reset.R')
source('diagnosis.R')
source('prob.R')

#load("estimation_net.RData")
#load("burnin.RData")
net.f <- net0_bip

activate.edges(net.f)
activate.vertices(net.f)

sim_time <-360 #length of simulation in months

start_time <- Sys.time()

setting1<-"burnin"
setting2<-"no social"

for (time in 1:sim_time){
  cat(time, '\n')
  cat("disease_progression", '\n')
  net.f <- disease_progression(net.f)
  cat("clinical_engagement", '\n')
  net.f <- clinical_engagement(net.f, setting1, setting2)
  cat("diagnosis", '\n')
  net.f <- diagnosis(net.f, setting2)
  cat("demography", '\n')
  net.f <- demography(net.f)
}

#save(net.f, file = "burnin.RData")

end_time <- Sys.time()
end_time - start_time
