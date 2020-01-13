## module for clinical engagement

#baseline screening
# Load libraries ---------------------------

library(ergm)

#Initialize function

clinical_engagement <- function(net.f){
  
  ## get individual attributes 
  pop_size <- network.size(net.f)
  age <- net.f %v% "age"
  symptom.severity <- net.f %v% "symptom.severity"
  ss <- net.f %v% "symptom.severity"
  time_since_pcp <- net.f %v% "time_since_pcp"
  reg.pcp.visitor <- net.f %v% "reg.pcp.visitor"
  disease.time <- net.f %v% "disease.time"
  diagnostic_referral <- net.f %v% "diagnostic_referral"
  screening_referral <- net.f %v% "screening_referral"
  diagnosis <- net.f %v% "diagnosis"
  bc_status <- net.f %v% "bc_status"
  diagnosis_time <- net.f %v% "diagnosis_time"
  neighbornav <- net.f %v% "neighbornav"
  neighborfp <- net.f %v% "neighborfp"
  navigated <- net.f %v% "navigated"
  antinavigated <- net.f %v% "antinavigated"
  screen_complete <- net.f %v% "screen_complete"
  dt_complete <- net.f %v% "diagnostic_test_complete"
  
  attrib_mtrx<-cbind(symptom.severity,
                     reg.pcp.visitor,
                     navigated,
                     antinavigated,
                     neighbornav,
                     neighborfp)
  
  all_agents<-which(net.f %v% "diagnosis" == 0)
  for (agent in all_agents){
    agent_data<-attrib_mtrx[agent,]
    #cat("clinical eng agent #:", agent, '\n')
    #Stage 3: simulate pcp visits
    #first: symptomatic agents without referrals roll for dt referrals
    if(diagnostic_referral[agent]==0 &
       screening_referral[agent]==0 &
       ss[agent]>0){
       diagnostic_referral[agent]<-rbinom(1,1,prob(agent_data,"dt"))
    }
    
    #second: all agents without referrals roll for sm referrals
    if(diagnostic_referral[agent]==0 &
       screening_referral[agent]==0){
      screening_referral[agent]<-rbinom(1,1,prob(agent_data,"sm"))
    }
    
    #stage 4: simulate navigation
    else if((navigated[agent]==0) &
            (dt_complete[agent]==0) &
            (screen_complete[agent]==0) &
            (diagnostic_referral[agent]==1 |
             screening_referral[agent]==1)
            ){
      navigated[agent]<-rbinom(1,1,.5) #currently default value
    }
  }
  net.f %v% "navigated" <- navigated
  net.f %v% "screening_referral" <- screening_referral
  net.f %v% "diagnostic_referral" <- diagnostic_referral

return(net.f)
}