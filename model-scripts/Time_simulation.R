##Time loop
rm(list=ls())
setwd("/project2/khanna7/bryanb/bc-navigation/dec9_navlength/bc-navigation")

library(networkDynamic)
source('model-scripts/parameters.R')
source('model-scripts/disease-progression.R')
source('model-scripts/clinical-engagement.R')
source('model-scripts/demography-reset.R')
source('model-scripts/diagnosis.R')
source('model-scripts/prob.R')


burninname = "data/7-1-2021_menopause_at_50_burnin.RData"
  
#User Modifiable Parameters
slurm <- TRUE #Slurm code included if true, local execution if false (affects output file names)
#sim_time <- 360 #length of simulation in months
sim_time <- 360

#Intervention Specification
control <- FALSE
institutional <- TRUE
social <- TRUE

#Initialization
load(burninname)
#net.f <- net0_bip
activate.edges(net.f)
activate.vertices(net.f)
start_time <- Sys.time()

#Initialize New Vertex Attributes
#this should go in estimation, but i don't want to generate another burnin rn
set.vertex.attribute(net.f, "navigate_next_referral",0,)

set.vertex.attribute(net.f, "navigation_length", 0, )
set.vertex.attribute(net.f, "screening_referral_length", 0, )
set.vertex.attribute(net.f, "diagnostic_referral_length", 0, )

set.vertex.attribute(net.f, "screening_referral_expired", 0, )
set.vertex.attribute(net.f, "diagnostic_referral_expired", 0, )

#Main Loop
cat("INTERVENTION WITH SOCIAL NAVIGATION", "\n")
for (time_step in 1:sim_time){
  cat("\n","-------------------------------Begin Time Step",time_step, '-----------------------------------------\n')
  cat("\n", "Begin disease_progression.R", '\n \n')
  net.f <- disease_progression(net.f)
  cat("\n", "Begin clinical-engagement.R", '\n \n')
  net.f <- clinical_engagement(net.f, institutional, social, control, time_step)
  cat("Number currently navigated: ", length(which(net.f %v% "navigated" == 1)), "\n")
  cat("\n", "Begin diagnosis.R", '\n \n')
  net.f <- diagnosis(net.f, social, time_step)
  cat("\n", "Begin demography.R", '\n \n')
  net.f <- demography(net.f, slurm, time_step, sim_time)
    #DEBUG Print navigation lengths
    navigation.length <- net.f %v% "navigation_length"
    #navigation_start_time <- net.f %v% "navigation_start_time"
    #if(time_step > 1){
    #  hist(navigation.length[which(navigation.length > 0)], breaks = 50)
    #}
}

#Save RDat
##NOTE: set slurm = FALSE to run locally. This will output one run with 1.data and 1.RData outputs. Alternatively, comment this section out and assign "filename" variable to run without slurm and a unique filename (no overwriting 1.data/RData from previous runs)
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
