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

load("estimation_net.RData")
net.f <- net0_bip

activate.edges(net.f)
activate.vertices(net.f)

#parameter_file <- "parameters.R"
sim_time <-360 #length of simulation in months

#estimation_net <- main_estimation(parameter_file)
start_time <- Sys.time()

for (time in 1:sim_time){
  cat(time, '\n')
  cat("disease_progression", '\n')
  net.f <- disease_progression(net.f)
  cat("clinical_engagement", '\n')
  net.f <- clinical_engagement(net.f)
  cat("diagnosis", '\n')
  net.f <- diagnosis(net.f)
  cat("demography", '\n')
  net.f <- demography(net.f)
}

end_time <- Sys.time()
end_time - start_time
  
vec<-c()
for(i in 1:network.size(net.f)){
  vec<-append(vec,length(get.edges(net.f,v=i)))
}
