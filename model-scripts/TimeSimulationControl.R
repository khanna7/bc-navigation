##Time loop

rm(list=ls())
setwd("/project2/khanna7/bryanb/bc-navigation/dec9_navlength/bc-navigation")

library(networkDynamic)

source('model-scripts/parameters.R')
#source('estimation.R')
source('model-scripts/disease-progression.R')
source('model-scripts/clinical-engagement.R')
source('model-scripts/demography-reset.R')
source('model-scripts/diagnosis.R')
source('model-scripts/prob.R')

burninname = "data/clean_burnin.RData"

#Slurm code included if true, local execution if false
slurm <- TRUE

load(burninname)
#load("burnin.RData")
#net.f <- net0_bip

activate.edges(net.f)
activate.vertices(net.f)

#parameter_file <- "model-scripts/parameters.R"
sim_time <- 360 #length of simulation in months

#estimation_net <- main_estimation(parameter_file)
start_time <- Sys.time()

control <- TRUE
institutional <- FALSE
social <- FALSE

cat("CONTROL RUN WITHOUT INTERVENTION", "\n \n")
for (time in 1:sim_time){
  cat(time, '\n')
  cat("\n", "Begin disease_progression.R", '\n \n')
  net.f <- disease_progression(net.f)
  cat("\n", "Begin clinical-engagement.R", '\n \n')
  net.f <- clinical_engagement(net.f, institutional, social, control)
  cat("Number currently navigated: ", length(which(net.f %v% "navigated" == 1)), "\n")
  cat("\n", "Begin diagnosis.R", '\n \n')
  net.f <- diagnosis(net.f, social)
  cat("\n", "Begin demography.R", '\n \n')
  net.f <- demography(net.f, slurm)
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

##Timekeeping
end_time <- Sys.time()
end_time <- start_time
