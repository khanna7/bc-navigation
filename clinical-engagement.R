## module for clinical engagement

#baseline screening
# Load libraries ---------------------------

library(ergm)

#Initialize function

clinical_engagement <- function(net.f, settings1, settings2){
  #this function simulates clinic visits
  
  ## get individual attributes 
  pop_size <- 5000
  age <- net.f %v% "age"
  symptom.severity <- net.f %v% "symptom.severity"
  ss <- net.f %v% "symptom.severity"
  time_since_pcp <- net.f %v% "time_since_pcp"
  reg.pcp.visitor <- net.f %v% "reg.pcp.visitor"
  diagnostic_referral <- net.f %v% "diagnostic_referral"
  screening_referral <- net.f %v% "screening_referral"
  diagnosis <- net.f %v% "diagnosis"
  bc_status <- net.f %v% "bc_status"
  diagnosis_time <- net.f %v% "diagnosis_time"
  neighbor_navigated <- net.f %v% "neighbor_navigated"
  neighborfp <- net.f %v% "neighborfp"
  navigated <- net.f %v% "navigated"
  antinavigated <- net.f %v% "antinavigated"
  screen_complete <- net.f %v% "screen_complete"
  dt_complete <- net.f %v% "diagnostic_test_complete"
  diagnostic_referral_counter <- net.f %v% "diagnostic_referral_counter"
  screening_referral_counter <- net.f %v% "screening_referral_counter"
  
  if((time==1)&(settings1!="burnin")){
    navigated <- rbinom(length(net.f %v% "navigated"),1,0.02)
  }
  
  attrib_mtrx<-cbind(symptom.severity,
                     reg.pcp.visitor,
                     navigated,
                     antinavigated,
                     neighbor_navigated,
                     neighborfp)
  
  all_agents<-which(net.f %v% "diagnosis" == 0)
  
  for (agent in all_agents){
    agent_data<-attrib_mtrx[agent,]
    #first: symptomatic agents without referrals roll for dt referrals
    if(diagnostic_referral[agent]==0 &
       screening_referral[agent]==0 &
       ss[agent]>0){
       diagnostic_referral[agent]<-rbinom(1,1,prob(agent_data,"dt"))
       diagnostic_referral_counter[agent]<-diagnostic_referral_counter[agent]+diagnostic_referral[agent]
    }
    
    #second: all agents without referrals roll for sm referrals
    if(diagnostic_referral[agent]==0 &
       screening_referral[agent]==0){
      screening_referral[agent]<-rbinom(1,1,prob(agent_data,"sm"))
      screening_referral_counter[agent]<-screening_referral_counter[agent]+screening_referral[agent]
    }
   
    if(settings1=="institutional"){
       #simulate navigation
      if((navigated[agent]==0) &
         (dt_complete[agent]==0) &
         (screen_complete[agent]==0) &
         (diagnostic_referral[agent]==1 |
          screening_referral[agent]==1)
         ){
        navigated[agent]<-rbinom(1,1,0.2) #random institutional navigation
      }
    }
    
    if(settings2=="social"){ 
      if((navigated[agent]==0) &
         (dt_complete[agent]==0) &
         (screen_complete[agent]==0) &
         (diagnostic_referral[agent]==1 |
          screening_referral[agent]==1) &
         (neighbor_navigated[agent]==1) #key component
      ){
        navigated[agent]<-rbinom(1,1,0.514) #social navigation
      }
    }
  }
  net.f %v% "diagnostic_referral_counter" <- diagnostic_referral_counter
  net.f %v% "screening_referral_counter" <- screening_referral_counter
  net.f %v% "navigated" <- navigated
  net.f %v% "screening_referral" <- screening_referral
  net.f %v% "diagnostic_referral" <- diagnostic_referral

return(net.f)
}