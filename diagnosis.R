## module for clinical engagement

#baseline screening
# Load libraries ---------------------------

library(ergm)

#Initialize function

screen_test_diagnose <- function(estimation_net){
  
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
  navigated <- net0_bip %v% "navigated"
  type <-net0_bip %v% "type"
  diagnostic_referral_time<-net0_bip %v% "diagnostic_referral_time"
  screening_referral_time<- net0_bip %v% "screening_referral_time"
  screen_complete <- net0_bip %v% "screen_complete"
  dt_complete <- net0_bip %v% "diagnostic_test_complete"
  screen_result<- net0_bip %v% "screen_result"
  diagnostic_test_result<- net0_bip %v% "diagnostic_test_result"
  neighbor_navigated <- net0_bip %v% "neighbor_navigated"
  
  #probability list
  p_false_negative_sm <- 0.012/100 #the probability of a false negative screening mammogram result
  p_false_positive_sm <- 0.0815 #the probability of a false positive screening mammogram result
  
  #screen_delay <- 2 #the amount of months delay for a screening mammogram.
                    #Yami says it will vary, for now setting it to two months.
  
  all_agents<-which(diagnosis==0)
  for (agent in all_agents){
    #first deal with navigated and unnavigated screening mammogram patients
    #second deal with navigated diagnostic test patients
    if(screening_referral[agent]==1){
     
        screen_complete[agent]<-rbinom(1,1,prob(agent,"sm", "test"))
        
        if(screen_complete[agent]==1){
          if(bc_status[agent]==1){
            screen_result[agent]<-rbinom(1,1,(1-p_false_negative_sm))
          }
          else if(bc_status[agent]==0){
            screen_result[agent]<-rbinom(1,1,p_false_positive_sm)
          }
        }
      }
    }
    
    if (diagnostic_referral[agent]==1){
        dt_complete[agent]<-rbinom(1,1,prob(agent,"dt","test"))
        
        if(dt_complete[agent]==1){
          if(bc_status[agent]==1){
            diagnosis[agent]<-1
            diagnosis_time[agent]<-disease.time[agent]
          }
        }
        #social effect of navigation
        if(dt_complete[agent]==1){
          if(bc_status[agent]==0){
            antinavigated[agent]<-1
              neighbors<-get.neighborhood(net0_bip,agent)
              for (neighbor in neighbors){
                neighborfp[neighbor]<-rbinom(1,1,0.5)
              }
          }
        }
    }
  
  #inputting diagnostic test referrals to positive screening mammogram pts.
  if(screen_result[agent]==1){
    diagnostic_referral[agent]<-1
  }
  
  navigated_agents<-which(navigated==1)
  for (agent in navigated_agents){
    neighbors<-get.neighborhood(net0_bip,agent)
    for (neighbor in neighbors){
      neighbor_navigated[neighbor]<-rbinom(1,1,0.72)
    }
  }
  net0_bip %v% "navigated" <- navigated
  
  net0_bip %v% "diagnosis" <- diagnosis
  net0_bip %v% "diagnosis_time" <- diagnosis_time
  
  net0_bip %v% "diagnostic_test_result" <- diagnostic_test_result
  net0_bip %v% "diagnostic_referral" <- diagnostic_referral
  net0_bip %v% "diagnostic_test_complete" <- dt_complete
  net0_bip %v% "diagnostic_referral_time" <- diagnostic_referral_time
  
  net0_bip %v% "screen_result" <- screen_result
  net0_bip %v% "screening_referral" <- screening_referral
  net0_bip %v% "screen_complete" <- screen_complete
  net0_bip %v% "screening_referral_time" <- screening_referral_time
  
  net0_bip %v% "neighbor_navigated" <- screening_referral_time
  net0_bip %v% "neighborfp" <- screening_referral_time
}