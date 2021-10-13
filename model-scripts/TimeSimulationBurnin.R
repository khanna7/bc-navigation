##Time loop

rm(list=ls())

project_dir = "/project2/ahotton/bryanb/bc-navigation/dec9_navlength/bc-navigation"
setwd(project_dir)

library(networkDynamic)

source('model-scripts/parameters.R')
#source('model-scripts/estimation.R') # estimation.R is used to generate a new time-0 network to feed into burnin generation
source('model-scripts/disease-progression.R')
source('model-scripts/clinical-engagement.R')
source('model-scripts/demography-reset.R')
source('model-scripts/diagnosis.R')
source('model-scripts/prob.R')

#Slurm code included if true, local execution if false
slurm <- TRUE

##Load estimated time = 0 network
load("data/menopause_age50_time_0_net.RData")
net.f <- net0_bip
activate.edges(net.f)
activate.vertices(net.f)

#parameter_file <- "model-scripts/parameters.R"
sim_time <- 360 #length of simulation in months

#estimation_net <- main_estimation(parameter_file)
start_time <- Sys.time()

control <- TRUE
institutional <- FALSE
social <- FALSE

#this should go in estimation, but i don't want to generate another burnin rn

set.vertex.attribute(net.f, "navigate_next_referral",0,)

set.vertex.attribute(net.f, "navigation_start_time", 0, )
set.vertex.attribute(net.f, "navigation_end_time", 0, )
set.vertex.attribute(net.f, "navigation_length", 0, )

set.vertex.attribute(net.f, "screening_referral_start_time", 0, )
set.vertex.attribute(net.f, "screening_referral_end_time", 0, )
set.vertex.attribute(net.f, "screening_referral_length", 0, )

set.vertex.attribute(net.f, "diagnostic_referral_start_time", 0, )
set.vertex.attribute(net.f, "diagnostic_referral_end_time", 0, )
set.vertex.attribute(net.f, "diagnostic_referral_length", 0, )

set.vertex.attribute(net.f, "screening_referral_expired", 0, )
set.vertex.attribute(net.f, "diagnostic_referral_expired", 0, )
set.vertex.attribute(net.f, "diagnostic_referral_length", 0, )

cat("CONTROL RUN WITHOUT INTERVENTION", "\n \n")
for (time_step in 1:sim_time){
  cat("\n","-------------------------------Begin Time Step",time_step, '-----------------------------------------\n')
  cat("\n", "Begin disease_progression.R", '\n \n')
  net.f <- disease_progression(net.f)
  cat("\n", "Begin clinical-engagement.R", '\n \n')
  net.f <- clinical_engagement(net.f, institutional, social, control, time_step)
  cat("Number currently navigated: ", length(which(net.f %v% "navigated" == 1)), "\n") # Should be 0
  cat("\n", "Begin diagnosis.R", '\n \n')
  net.f <- diagnosis(net.f, social, time_step)
  cat("\n", "Begin demography.R", '\n \n')
  net.f <- demography(net.f, slurm, time_step, sim_time)
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
