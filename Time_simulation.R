##Time loop

rm(list=ls())
##setwd("~/projects/bc-navigation")

library(networkDynamic)

source('parameters.R')
#source('estimation.R')
source('disease-progression.R')
source('clinical-engagement.R')
source('demography-reset.R')
source('diagnosis.R')
source('prob.R')

burninname = "3.RData"

#Slurm code included if true, local execution if false
slurm = FALSE

load(burninname)
#load("burnin.RData")
#net.f <- net0_bip

activate.edges(net.f)
activate.vertices(net.f)

#parameter_file <- "parameters.R"
sim_time <-360 #length of simulation in months

#estimation_net <- main_estimation(parameter_file)
start_time <- Sys.time()

setting1<-"institutional"
setting2<-"social"

#setting1<-"burnin"
#setting2<-"social effect off"

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



##comment this section and assign "filename" variable to run without slurm
if(slurm == TRUE){
  slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
 } 
else{
  slurm_arrayid <- 1
 }
numericid = as.numeric(slurm_arrayid)
netfilename = paste(numericid, ".RData", sep="")

save(net.f, file = netfilename)

##Timekeeping
end_time <- Sys.time()
end_time <- start_time
