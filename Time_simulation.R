##Time loop 

rm(list=ls())
setwd("~/projects/bc-navigation")

library(networkDynamic)

source('estimation.R')
main_estimation("parameters.R")

source('parameters.R')

source('simulate-disease-progression.R')
source('clinical-engagement.R')
source('Demography.R')
source('diagnosis.R')
source('prob.R')

load("estimation_net.RData")
net.f <- net0_bip

activate.edges(net.f)
activate.vertices(net.f)

#parameter_file <- "parameters.R"
sim_time <-120 #length of simulation in months

#estimation_net <- main_estimation(parameter_file)
start_time <- Sys.time()

for (time in 1:sim_time){
  
  net.f <- disease_progression(net.f)
  net.f <- clinical_engagement(net.f)
  net.f <- diagnosis(net.f)
  net.f <- demography(net.f)
}

end_time <- Sys.time()
end_time - start_time
  

