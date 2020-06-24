##Time loop 

rm(list=ls())
##setwd("~/projects/bc-navigation") ##Specific to Mickey's runs

library(networkDynamic)

source('parameters.R')
#source('estimation.R')
source('disease-progression.R')
source('clinical-engagement.R')
source('demography-reset.R')
source('diagnosis.R')
source('prob.R')

###Change this to chose a burnin
load("06_17_2020_Burnin_30_round1/RData/3.RData")
#load("burnin.RData")
#net.f <- net0_bip

activate.edges(net.f)
activate.vertices(net.f)

#parameter_file <- "parameters.R"
sim_time <-120 #length of simulation in months

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


######SLURM Intervention Network Output

##handling naming output file with environment variable from slurm (June 14 2020-$
##ref= https://sph.umich.edu/biostat/computing/cluster/examples/r.html
###https://stackoverflow.com/questions/6773342/variable-in-the-file-name-for-writ$


slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
numericid = as.numeric(slurm_arrayid)
intervention_filename = paste(numericid, "_intervention.RData", sep="")

save(net.f, file = intervention_filename)

######

end_time <- Sys.time()
end_time - start_time
