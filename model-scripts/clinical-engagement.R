##
## This Module Gives Agents Rolls for Referrals and Navigation ##
##
#Load dependencies 
library(ergm)

#Initialize function
clinical_engagement <- function(net.f, institutional, social, control, time_step){
  ####Pull in individual attributes 
  
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
  screening_referral_checker <- net.f %v% "screening_referral_checker"
  number_navigated_at_t <- 0
  rolls_for_navigation <- 0
  
  navigation_start_time <- net.f %v% "navigation_start_time"
  navigation_length <- net.f %v% "navigation_length"
  
  #referral lengths
  screening_referral_length <- net.f %v% "screening_referral_length"
  diagnostic_referral_length <- net.f %v% "diagnostic_referral_length"
  
  #Social Navigation
  navigate_next_referral <- net.f %v% "navigate_next_referral"
  
  
  ####Create Attribute matrix (Mickey's solution) 
  
  attrib_mtrx<-cbind(symptom.severity,
                     reg.pcp.visitor,
                     navigated,
                     antinavigated,
                     neighbor_navigated,
                     neighborfp)
  
  ####Isolate Undiagnosed Agent Subpopulation 
  
  all_agents<-which(net.f %v% "diagnosis" == 0)
  
  ####Begin Referral Logic
  for (agent in all_agents){
    agent_data<-attrib_mtrx[agent,]
    
    #DIRECT DIAGNOSTIC REFERRALS
    ##Give symptomatic agents without referrals roll for diagnostic test referrals 
    if(diagnostic_referral[agent]==0 &
       screening_referral[agent]==0 &
       ss[agent]>0){
      diagnostic_referral[agent]<-rbinom(1,1,prob(agent_data,"dt",0))
      diagnostic_referral_counter[agent]<-diagnostic_referral_counter[agent]+diagnostic_referral[agent]
      if(diagnostic_referral[agent] == 1){ #navigate direct-diagnosis agents
        diagnostic_referral_length[agent] <- 0
        if(institutional == TRUE){
          navigated[agent]<-rbinom(1,1, prob_institutional_navigation) #random institutional navigation
          rolls_for_navigation <- rolls_for_navigation + 1
        }
        if(social == TRUE){
          if(navigate_next_referral[agent] == 1){
            navigated[agent] <- 1
            navigate_next_referral[agent] <- rbinom(1,1,.5) #This gives a 50% chance for social navigation lasting for 2 referrals
            cat("----------------------------Direct-diagnosis social navigation hit------------------------------")
          }
        }
        ##Start navigation, diagnostic referral clocks
        if(navigated[agent]==1){
          navigation_start_time[agent] <- time_step
          number_navigated_at_t <- number_navigated_at_t + navigated[agent]
        }
      }
    }
    
    #SCREENING REFERRALS (Diagnostic referrals given during screening referral completion in diagnosis.R)
    #Give all agents without referrals roll for sm referrals 
    if(diagnostic_referral[agent]==0 &
       screening_referral[agent]==0){
      screening_referral[agent]<-rbinom(1,1,prob(agent_data,"sm",0))
      screening_referral_counter[agent]<-screening_referral_counter[agent]+screening_referral[agent]
      if(screening_referral[agent]==1){ #Limiting navigation rolls to the step where a referral is given
        screening_referral_checker[agent] <- 1
        screening_referral_length[agent] <- 0
        if(social == TRUE){
          if(navigated[agent] == 0 & #This is where social navigation kicks in
             navigate_next_referral[agent] == 1){
            navigated[agent] <- 1
            cat("------------Neighbor Navigation hit--------------------\n")
            navigate_next_referral[agent] <- rbinom(1,1,.5) #This gives a 50% chance for social navigation lasting for 2 referrals
            number_navigated_at_t <- number_navigated_at_t + navigated[agent]
          }
        }
        if(institutional == TRUE){	#simulate institutional navigation
          if(isTRUE((navigated[agent]==0) &
                    (dt_complete[agent]==0) &
                    (screen_complete[agent]==0) 
          )){
            navigated[agent]<-rbinom(1,1, prob_institutional_navigation) #random institutional navigation
            rolls_for_navigation <- rolls_for_navigation + 1
            number_navigated_at_t <- number_navigated_at_t + navigated[agent]
          }
        }
      }
      #Give a navigation roll to all unnavigated agents with outstanding referrals AND neighbor navigation 
      ##OLD SOCIAL NAVIGATION
      # if(social == TRUE){ 
      #    if(isTRUE((navigated[agent]==0) &
      #       (dt_complete[agent]==0) &
      #       (screen_complete[agent]==0) &
      #       (diagnostic_referral[agent]==1 |
      #        screening_referral[agent]==1) &
      #       (neighbor_navigated[agent]==1) #key component
      #    )){
      #      navigated[agent]<-rbinom(1,1,prob_social_navigation) #social navigation
      #    }
      # }
    }
  }
  
  #Debug output 
  cat("Number navigated at time t: ", number_navigated_at_t, "\n")
  cat("Rolls for navigation at time t: ", rolls_for_navigation, "\n")
  cat("Agents navigated per roll: ", number_navigated_at_t/rolls_for_navigation, "\n")
  
  ####Update the network 
  net.f %v% "diagnostic_referral_counter" <- diagnostic_referral_counter
  net.f %v% "screening_referral_counter" <- screening_referral_counter
  net.f %v% "navigated" <- navigated
  net.f %v% "screening_referral" <- screening_referral
  net.f %v% "diagnostic_referral" <- diagnostic_referral
  
  net.f %v% "screening_referral_checker" <- screening_referral_checker 
  net.f %v% "diagnostic_referral_length" <- diagnostic_referral_length
  net.f %v% "screening_referral_length" <- screening_referral_length
  
  net.f %v% "navigate_next_referral" <- navigate_next_referral
  
  return(net.f)
}
