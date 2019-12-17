## module for clinical engagement

#baseline screening
# Load libraries ---------------------------

library(ergm)

#Initialize function

sim_clinical_engagement <- function(estimation_net){
  
  # Load data ---------------------------
  
  net0_bip <- estimation_net
  
  ## get individual attributes 
  pop_size <- network.size(net0_bip)
  age <- net0_bip %v% "age"
  symptom.severity <- net0_bip %v% "symptom.severity"
  time_since_pcp <- net0_bip %v% "time_since_pcp"
  reg.pcp.visitor <- net0_bip %v% "reg.pcp.visitor"
  disease.time <- net0_bip %v% "disease.time"
  diagnostic_referral <- net0_bip %v% "diagnostic_referral"
  screening_referral <- net0_bip %v% "screening_referral"
  diagnosis <- net0_bip %v% "diagnosis"
  bc_status <- net0_bip %v% "bc_status"
  diagnosis_time <- net0_bip %v% "diagnosis_time"
  diagnostic_referral_time<-net0_bip %v% "diagnostic_referral_time"
  screening_referral_time<- net0_bip %v% "screening_referral_time"
  neighbornav <- net0_bip %v% "neighbornav"
  neighborfp <- net0_bip %v% "neighborfp"
  navigated <- net0_bip %v% "navigated"
  antinavigated <- net0_bip %v% "antinavigated"
  diagnostic_referral_time<-net0_bip %v% "diagnostic_referral_time"
  screening_referral_time<- net0_bip %v% "screening_referral_time"
  screen_complete <- net0_bip %v% "screen_complete"
  dt_complete <- net0_bip %v% "diagnostic_test_complete"
  
  ss<-symptom.severity #ss
  
  prob<-function(agent,test,ref_vs_test){
    
    time_component<-0.0033 #time
    
    if(ss<=1 & test=="sm"){symptom_severity_component<-1}
    if(ss>=2 & test=="sm"){symptom_severity_component<-3}
    
    if(ss==0 & test=="dt"){symptom_severity_component<-1}
    if(ss==1 & test=="dt"){symptom_severity_component<-5}
    if(ss==2 & test=="dt"){symptom_severity_component<-10.8}
    if(ss==3 & test=="dt"){symptom_severity_component<-12.2}
    
    if(reg.pcp.visitor[agent]==1){regular_pcp_visitor_component<-14} #same as adherence
    if(reg.pcp.visitor[agent]==0){regular_pcp_visitor_component<-1}
    
    if(navigated[agent]==1){regular_pcp_visitor_component<-(3.63-2.04*reg.pcp.visitor[agent])}
    if(navigated[agent]==0){regular_pcp_visitor_component<-1}
    
    if(antinavigated[agent]==1){antinavigated_component<-1/14}
    if(antinavigated[agent]==0){antinavigated_component<-1}
    
    if(neighbornav[agent]==1){neighbornav_component<-3.8}
    if(neighbornav[agent]==0){neighbornav_component<-1}
    
    if(neighborfp[agent]==1){neighborantinav_component<-1/3.8}
    if(neighborfp[agent]==0){neighborantinav_component<-1}
    
    if(ref_vs_test=="test"){time_component<-0.0035}
    
    return(
        min(time_component*
        symptom_severity_component*
        regular_pcp_visitor_component*
        navigated_component*
        antinavigated_component*
        neighbornav_component*
        neighborantinav_component,1)
        )
  }
  
  all_agents<-which(diagnosis[agent]==0)
  for (agent in all_agents){
    #Stage 3: simulate pcp visits
    #first: symptomatic agents without referrals roll for dt referrals
    if(diagnostic_referral[agent]==0 &
       screening_referral[agent]==0 &
       ss[agent]>0){
       diagnostic_referral[agent]<-rbinom(1,1,prob(agent,"dt","ref"))
    }
    
    #second: all agents without referrals roll for sm referrals
    if(diagnostic_referral[agent]==0 &
       screening_referral[agent]==0){
      screening_referral[agent]<-rbinom(1,1,prob(agent,"sm","ref"))
    }
    
    #stage 4: simulate navigation
    else if((navigated[agent]==0) &
            (dt_complete[agent]==0) &
            (screen_complete[agent]=0) &
            (diagnostic_referral[agent]==1 |
             screening_referral[agent]==1)
            ){
      navigated[agent]<-rbinom(1,1,.5) #currently default value
    }
  }
  net0_bip %v% "navigated" <- navigated
  net0_bip %v% "screening_referral" <- screening_referral
  net0_bip %v% "screening_referral_time" <- screening_referral_time
  net0_bip %v% "diagnostic_referral" <- diagnostic_referral
  net0_bip %v% "diagnostic_referral_time" <- diagnostic_referural_time
}