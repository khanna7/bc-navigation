#Demography module

demography <- function(net.f, slurm, time_step, sim_time){ 


  # Aging -------------------

  #Update age per time step
  age <- net.f %v% "age"
  age <- age + 1/12
  net.f %v% "age" <- age

  # Departures -------------------

  #Aging out of model
  age_mortality <- which(age >= 75)

  #Mortality from breast cancer
  disease.time <- net.f %v% "disease.time"
  diagnosis.time <- net.f %v% "diagnosis.time"
  cancer_death <- net.f %v% "cancer_death"

  bc_mortality <- which(cancer_death==1)

  #All cause mortality
  all_cause_mort <- array(NA, dim = 5000)
  for (i in seq(length(age))){
    if (age[i] < 55){
      all_cause_mort[i] <- rbinom(1,1, ac.mort.50)
    } else if (age[i] < 60){
      all_cause_mort[i] <- rbinom(1,1, ac.mort.55)
    } else if (age[i] < 65){
      all_cause_mort[i] <- rbinom(1,1, ac.mort.60)
    } else if (age[i] < 70){
      all_cause_mort[i] <- rbinom(1,1, ac.mort.65)
    } else if (age[i] < 75){
      all_cause_mort[i] <- rbinom(1,1, ac.mort.70)
    }
  }

  all_cause_ids <- which(all_cause_mort == 1)

  nodes.to.reset <-union(all_cause_ids,age_mortality)
  nodes.to.reset <-union(nodes.to.reset,bc_mortality)
  nintros<-length(nodes.to.reset)


  #####Ongoing project to change vertex IDs
  #vertex.ids<-net.f %v% "vertex.names"
  #
  #for (i in 1:length(nodes.to.reset)){
  #  vertex.ids[nodes.to.reset]
  #}
  #####Ongoing project to change vertex IDs

  cat("Number of new agents (nintros): ", nintros, "\n")
  if(nintros>0){

    #Initialize attributes for reset vertices

    ## set attributes for egos and alters
    set.vertex.attribute(net.f, "type", sample(c("n.ego", "n.alter"),1, prob = c(.1, .9)), nodes.to.reset)

    ##age
    reset_ages<-sample(50:74,length(nodes.to.reset),replace=T) #basic randomization.
    set.vertex.attribute(net.f, "age", reset_ages, nodes.to.reset)

    ## race: 100% of egos are black (=1); 95% of alters are black, 5% are other (=1)
    set.vertex.attribute(net.f, "race", 1,nodes.to.reset)
    set.vertex.attribute(net.f, "race", rbinom(n.alter, 1, percent.alters.black), nodes.to.reset)

    #obesity
    set.vertex.attribute(net.f, "bmi.ge.30", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "bmi.ge.30", rbinom(n, 1, prop.bmi.ge.30), nodes.to.reset)

    #menopausal status
    set.vertex.attribute(net.f, "meno.status", 0, nodes.to.reset)

    #First degree relative history
    set.vertex.attribute(net.f, "fd.rel", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "fd.rel", rbinom(length(nodes.to.reset), 1, prop.fd.rel), nodes.to.reset)

    #compute initial risk for each agent
    set.vertex.attribute(net.f, "bc_hpos_risk", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "bc_hneg_risk", 0, nodes.to.reset)

    #populate breast cancer status
    #create an attribute "bc.status": populate it based on the initial risk estimates
    initialized_bc <- rbinom(length(nodes.to.reset), 1, .015)
    subtype <- rep(0, length(nodes.to.reset))
    subtype[which(initialized_bc == 1)] <- sample(c(1,2), 1, prob = c(hpos_risk, 1-hpos_risk))
    set.vertex.attribute(net.f, "bc_status", initialized_bc, nodes.to.reset)

    # set vertex attribute for bc subtype
    set.vertex.attribute(net.f, "subtype", subtype, nodes.to.reset)

    #Set vertex attribute for symptom severity
    set.vertex.attribute(net.f, "symptom.severity", 0, nodes.to.reset) #All people initially non-symptomatic

    #Set vertex attribute for time since disease development
    set.vertex.attribute(net.f, "disease.time", 0, nodes.to.reset) #initially 0, includes agents without breast cancer
    #have to populate initial breast cancer population with some distribution

    #Set vertex attribute for regular pcp visits
    set.vertex.attribute(net.f, "reg.pcp.visitor", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "reg.pcp.visitor", rbinom(nodes.to.reset, 1, prop.pcp.visitor), nodes.to.reset)

    #set vertex attribute for whether or not a patient has received a diagnostic referral
    set.vertex.attribute(net.f, "diagnostic_referral", 0, nodes.to.reset)

    #set vertex attribute for whether or not a patient has received a screening referral
    set.vertex.attribute(net.f, "dt_complete", 0, nodes.to.reset)

    #set vertex attribute for whether or not a patient has received a screening referral
    set.vertex.attribute(net.f, "screening_referral", 0, nodes.to.reset)

    #set vertex attribute for whether or not a patient has received a screening referral
    set.vertex.attribute(net.f, "screen_result", 0, nodes.to.reset)

    #set vertex attribute for whether or not a patient has been navigated
    set.vertex.attribute(net.f, "navigated", 0, nodes.to.reset)

    set.vertex.attribute(net.f, "neighbor_navigated", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "neighborfp", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "antinavigated", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "screen_complete", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "diagnostic_test_complete", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "neighbor_navigated_roll", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "neighborfp_roll", 0, nodes.to.reset)
    
    set.vertex.attribute(net.f, "diagnostic_referral_counter", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "screening_referral_counter", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "diagnostic_visit_counter", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "screening_visit_counter", 0, nodes.to.reset)
    
    set.vertex.attribute(net.f, "diagnostic_referral_checker", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "diagnostic_visit_checker", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "screening_visit_checker", 0, nodes.to.reset)
    
    set.vertex.attribute(net.f, "bc_onsets", 0, nodes.to.reset)

    #set vertex attribute for whether a patient has been diagnosed
    set.vertex.attribute(net.f, "diagnosis", 0, nodes.to.reset)

    #set vertex attribute for disease time at diagnosis
    set.vertex.attribute(net.f, "diagnosis_time", 0, nodes.to.reset)

    #set vertex attribute for disease time at diagnosis
    set.vertex.attribute(net.f, "diagnosis_referral_time", 0, nodes.to.reset)

    #set vertex attribute for death due to cancer stage
    set.vertex.attribute(net.f, "cancer_death", 0, nodes.to.reset)
    
    #nav length
    set.vertex.attribute(net.f, "navigation_start_time", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "navigation_end_time", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "navigation_length", 0, nodes.to.reset)
    #referral expiration
      #screening
    set.vertex.attribute(net.f, "screening_referral_start_time", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "screening_referral_end_time", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "screening_referral_length", 0, nodes.to.reset)
    #set.vertex.attribute(net.f, "screening_referral_expired", 0, nodes.to.reset)
      #diagnostic
    set.vertex.attribute(net.f, "diagnostic_referral_start_time", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "diagnostic_referral_end_time", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "diagnostic_referral_length", 0, nodes.to.reset)
    #set.vertex.attribute(net.f, "diagnostic_referral_expired", 0, nodes.to.reset)
  }

  # Save object -----
  number.of.positive.bc.agents<-length(which(net.f %v% "bc_status"==1))
  number.of.hpos.agents<-length(which(net.f %v% "subtype"==1))
  number.of.hneg.agents<-length(which(net.f %v% "subtype"==2))
  number.of.diagnosed.cases<-length(which(net.f %v% "diagnosis"==1))
  number.of.navigated.agents<-length(which(net.f %v% "navigated"==1))

  number.of.diagnostic.referrals<-length(which(net.f %v% "diagnostic_referral"==1))
  number.of.screening.referrals<-length(which(net.f %v% "screening_referral"==1))

  number.of.screen.completed<-length(which(net.f %v% "screen_complete"==1))
  number.of.dt.completed<-length(which(net.f %v% "diagnostic_test_complete"==1))

  number.of.symptomatic<-length(which(net.f %v% "symptom.severity">0))

  number.of.diagnostic.referrals.at.t<-length(which(net.f %v% "diagnostic_referral_checker"==1))
  cat("Number of diagnostic referrals at t: ", number.of.diagnostic.referrals.at.t, "\n")

  number.of.screening.referrals.at.t<-length(which(net.f %v% "screening_referral_checker"==1))
  cat("\n","Number of screening referrals at t:", number.of.screening.referrals.at.t, "\n")
  
  number.of.screening.visits.at.t<-length(which(net.f %v% "screening_visit_checker"==1))
  cat("Number of screening visits at t: ", number.of.screening.visits.at.t, "\n")
  
  number.of.diagnostic.visits.at.t<-length(which(net.f %v% "diagnostic_visit_checker"==1))
  cat("Number of diagnostic visits at t: ", number.of.screening.visits.at.t, "\n")
  
  positives<-which(net.f %v% "bc_status"==1)
  diagnosed<-which(net.f %v% "diagnosis"==1)
  disease.time<-net.f %v% "disease.time"
  time.with.cancer<-median(disease.time[positives])
  diagnosis_time<-net.f %v% "diagnosis_time"
  time.until.diagnosis<-median(diagnosis_time[diagnosed])

  navigated<-which(net.f %v% "navigated"==1)
  unnavigated<-which(net.f %v% "navigated"==0)
  diagnosed_and_navigated<-intersect(navigated,diagnosed)
  time.until.diagnosis.navigated<-median(diagnosis_time[diagnosed_and_navigated])

  neighbor_navigated<-which(net.f %v% "neighbor_navigated"==1)
  neighbor_unnavigated<-which(net.f %v% "neighbor_navigated"==0)

  diagnosed_and_unnavigated<-intersect(unnavigated,diagnosed)
  diagnosed_and_unnavigated_completely<-intersect(diagnosed_and_unnavigated, neighbor_unnavigated)
  time.until.diagnosis.unnavigated<-median(diagnosis_time[diagnosed_and_unnavigated_completely])

  diagnosed_and_neighbor_navigated<-intersect(neighbor_navigated,diagnosed_and_unnavigated)
  time.until.diagnosis.neigbor.navigated<-median(diagnosis_time[diagnosed_and_neighbor_navigated])

  screen_complete<-net.f %v% "screen_complete"
  screen_complete[which(screen_complete==1)]<-0
  net.f %v% "screen_complete"<-screen_complete

  #dtOfOne = table(which(net.f %v% "diagnostic_test_complete" == 1))

  #browser()
  #Debugging dt_complete
  #cat("\n dt_complete: ", which(net.f %v% "diagnostic_test_complete" == 1), "\n")
  #cat(capture.output(dtOfOne, exclude = NULL,file="indices_of_dt1.txt",sep="\n",append=TRUE))
  #sink('dtEqualOne.txt', append=TRUE)
  #cat(dtOfOne)
  #sink()
  
      
  #dt_complete <- net.f %v% "diagnostic_test_complete_complete"
  dt_complete <- net.f %v% "diagnostic_test_complete"
  dt1_indices <- which(dt_complete == 1) #WROTE
  #false_positives<-which(net.f %v% "antinavigated"==1) #WILL NECESSARILY BE EMPTY BECAUSE THERE IS NO NAVIGATION IN THE BASELINE MODEL
  #dt_complete[false_positives]<-0
  dt_complete[dt1_indices] <- 0 #ADITYA WROTE SO IT IS NOT ONLY DEPENDENT ON FALSE POSITIVES
  net.f %v% "diagnostic_test_complete" <- dt_complete
  diagnostic_referral_checker <- net.f %v% "diagnostic_referral_checker"
  diagnostic_referral_checker[which(diagnostic_referral_checker==1)]<-0
  net.f %v% "diagnostic_referral_checker" <- diagnostic_referral_checker
  
  #Reset screening_referral_checker (because it tracks referrals given in this time step)
  #Don't try to print this or you will get a very difficult to diagnose bug where 5000x the normal amount of data is printed
  screening_referral_checker <- net.f %v% "screening_referral_checker"
  screening_referral_checker[which(screening_referral_checker==1)]<-0
  net.f %v% "screening_referral_checker" <- screening_referral_checker

  diagnostic_visit_checker <- net.f %v% "diagnostic_visit_checker"
  diagnostic_visit_checker[which(diagnostic_visit_checker==1)]<-0
  net.f %v% "diagnostic_visit_checker" <- diagnostic_visit_checker

  screening_visit_checker<- net.f %v% "screening_visit_checker"
  screening_visit_checker[which(screening_visit_checker==1)]<-0
  net.f %v% "screening_visit_checker"<-screening_visit_checker
  
  number.of.bc.onsets<-length(which(net.f %v% "bc_onsets"==1))
  net.f %v% "bc_onsets"<-rep(0,5000)
  
  #Navigation length at the last step
 
  #if(time_step == sim_time){ #sim_time is the length of the simulation (specified in Time_simulation.R) #this gave weird results when run on 01/05. Many repititions of timestep 360 in x.data
    navigation.start <- net.f %v% "navigation_start_time"
    #cat("NAV START: ", navigation.start, "\n")
    navigation.end <- net.f %v% "navigation_end_time"
    #   net.f %v% "navigation_length" <- navigation.end - navigation.start
    #navigation.length <- net.f %v% "navigation_length" #ok don't try to print that
  #}

  ss0<-which(net.f %v% "symptom.severity"==0)
  ss1<-which(net.f %v% "symptom.severity"==1)
  ss2<-which(net.f %v% "symptom.severity"==2)
  ss3<-which(net.f %v% "symptom.severity"==3)

  number.of.ss0.diagnosed<-length(intersect(diagnosed,ss0))
  number.of.ss0.diagnosed.navigated<-length(intersect(diagnosed_and_navigated,ss0))
  number.of.ss0.diagnosed.neighbor_navigated<-length(intersect(diagnosed_and_neighbor_navigated,ss0))
  number.of.ss0.diagnosed.unnavigated<-length(intersect(diagnosed_and_unnavigated_completely,ss0))

  number.of.ss1.diagnosed<-length(intersect(diagnosed,ss1))
  number.of.ss1.diagnosed.navigated<-length(intersect(diagnosed_and_navigated,ss1))
  number.of.ss1.diagnosed.neighbor_navigated<-length(intersect(diagnosed_and_neighbor_navigated,ss1))
  number.of.ss1.diagnosed.unnavigated<-length(intersect(diagnosed_and_unnavigated_completely,ss1))

  number.of.ss2.diagnosed<-length(intersect(diagnosed,ss2))
  number.of.ss2.diagnosed.navigated<-length(intersect(diagnosed_and_navigated,ss2))
  number.of.ss2.diagnosed.neighbor_navigated<-length(intersect(diagnosed_and_neighbor_navigated,ss2))
  number.of.ss2.diagnosed.unnavigated<-length(intersect(diagnosed_and_unnavigated_completely,ss2))

  number.of.ss3.diagnosed<-length(intersect(diagnosed,ss3))
  number.of.ss3.diagnosed.navigated<-length(intersect(diagnosed_and_navigated,ss3))
  number.of.ss3.diagnosed.neighbor_navigated<-length(intersect(diagnosed_and_neighbor_navigated,ss3))
  number.of.ss3.diagnosed.unnavigated<-length(intersect(diagnosed_and_unnavigated_completely,ss3))

  number.of.expired.diagnostic.referrals.at.t <- length(which(net.f %v% "diagnostic_referral_expired" == 1))
  number.of.expired.screening.referrals.at.t <- length(which(net.f %v% "screening_referral_expired" == 1))
  
  #Navigation length at the last step
  
#if(time_step == sim_time){ #sim_time is the length of the simulation (specified in Time_simulation.R) #this gave weird results when run on 01/05. Many repititions of timestep 360 in x.data
  #navigation.start <- net.f %v% "navigation_start_time"
  #cat("NAV START: ", navigation.start, "\n") #DEBUG
  #navigation.end <- net.f %v% "navigation_end_time"
  #net.f %v% "navigation_length" <- navigation.end - navigation.start
 # navigation.length <- net.f %v% "navigation_length" #ok don't try to print that
  #}

  navigation.length <- net.f %v% "navigation_length" #ok don't try to print that
  nav_start <- net.f %v% "navigation_start_time"
  nav_end <- net.f %v% "navigation_end_time"

##handling naming output file with environment variable from slurm (June 14 2020-Bryan)
##ref= https://sph.umich.edu/biostat/computing/cluster/examples/r.html
###https://stackoverflow.com/questions/6773342/variable-in-the-file-name-for-write-tabl$


if(slurm == TRUE){
  slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
} else{ 
  slurm_arrayid <- 1
}
numericid = as.numeric(slurm_arrayid)
filename = paste(numericid, ".data", sep="")

  write.table(cbind(time_step,
                    nintros, #deaths
                    number.of.positive.bc.agents,
                    number.of.hpos.agents,
                    number.of.hneg.agents,
                    number.of.diagnosed.cases,

                    number.of.diagnostic.referrals,
                    number.of.screening.referrals,
                    number.of.screen.completed,
                    number.of.dt.completed,
                    number.of.symptomatic,
                    number.of.navigated.agents,

                    time.with.cancer,
                    time.until.diagnosis,
                    time.until.diagnosis.navigated,
                    time.until.diagnosis.unnavigated,
                    time.until.diagnosis.neigbor.navigated,
                    number.of.diagnostic.referrals.at.t,

                    number.of.screening.visits.at.t,#19

                    number.of.ss0.diagnosed,#20
                    number.of.ss1.diagnosed,#21
                    number.of.ss2.diagnosed,#22
                    number.of.ss3.diagnosed,#23

                    number.of.ss0.diagnosed.navigated,#24
                    number.of.ss1.diagnosed.navigated,#25
                    number.of.ss2.diagnosed.navigated,#26
                    number.of.ss3.diagnosed.navigated,#27

                    number.of.ss0.diagnosed.unnavigated,#28
                    number.of.ss1.diagnosed.unnavigated,#29
                    number.of.ss2.diagnosed.unnavigated,#30
                    number.of.ss3.diagnosed.unnavigated,#31

                    number.of.ss0.diagnosed.neighbor_navigated, #32
                    number.of.ss1.diagnosed.neighbor_navigated, #33
                    number.of.ss2.diagnosed.neighbor_navigated, #34
                    number.of.ss3.diagnosed.neighbor_navigated, #35
                    number.of.bc.onsets,  #36
                    number.of.screening.referrals.at.t,#37
                    number.of.expired.diagnostic.referrals.at.t,#38
                    number.of.expired.screening.referrals.at.t#39
                    ),

              file=filename,
              append=TRUE,
              col.names=FALSE,
              row.names=FALSE
  )

  return(net.f)
}
