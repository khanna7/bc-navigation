  
##Time loop

rm(list=ls())
  #setwd("~/projects/bc-navigation")

library(networkDynamic)

source('parameters.R')
#source('estimation.R')
load("./estimation_net.RData")
source('disease-progression.R')
source('clinical-engagement.R')
source('demography-reset.R')
source('diagnosis.R')
source('prob.R')

#Enable/Disable Slurm
slurm = FALSE

#load("estimation_net.RData")
#load("burnin.RData")
net.f <- net0_bip

activate.edges(net.f)
activate.vertices(net.f)

sim_time <- 360 #length of simulation in months

start_time <- Sys.time()

control <- TRUE
institutional <- FALSE
social <- FALSE

for (time in 1:sim_time){
  cat("Timestep: ",time, '\n')
  cat("disease_progression", '\n')
  net.f <- disease_progression(net.f)
  cat("clinical_engagement", '\n')
  net.f <- clinical_engagement(net.f, institutional, social, control)
  cat("diagnosis", '\n')
  net.f <- diagnosis(net.f, social)
  cat("demography", '\n')
  net.f <- demography(net.f)
}

##comment this section and assign "filename" variable to run without slurm
if(slurm == TRUE){
  slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
} else{
  slurm_arrayid <- 1
}


numericid = as.numeric(slurm_arrayid)
netfilename = paste(numericid, ".RData", sep="")

save(net.f, file = netfilename)


end_time <- Sys.time()
end_time - start_time
