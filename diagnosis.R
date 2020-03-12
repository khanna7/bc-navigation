## module for clinical engagement

#baseline screening
# Load libraries ---------------------------

library(ergm)

#Initialize function

diagnosis <- function(net.f){
  
  ## get individual attributes 
  pop_size <- network.size(net.f)
  age <- net.f %v% "age"
  symptom.severity <- net.f %v% "symptom.severity"
  time_since_pcp <- net.f %v% "time_since_pcp"
  reg.pcp.visitor <- net.f %v% "reg.pcp.visitor"
  disease.time <- net.f %v% "disease.time"
  diagnostic_referral <- net.f %v% "diagnostic_referral"
  screening_referral <- net.f %v% "screening_referral"
  diagnosis <- net.f %v% "diagnosis"
  bc_status <- net.f %v% "bc_status"
  diagnosis_time <- net.f %v% "diagnosis_time"
  navigated <- net.f %v% "navigated"
  type <-net.f %v% "type"
  screen_complete <- net.f %v% "screen_complete"
  dt_complete <- net.f %v% "diagnostic_test_complete"
  screen_result<- net.f %v% "screen_result"
  diagnostic_test_result<- net.f %v% "diagnostic_test_result"
  neighbor_navigated <- net.f %v% "neighbor_navigated"
  neighborfp <- net.f %v% "neighborfp"
  antinavigated <- net.f %v% "antinavigated"
  neighbornav <- net.f %v% "neighbornav"
  
  #probability list
  p_false_negative_sm <- 0.012/100 #the probability of a false negative screening mammogram result
  p_false_positive_sm <- 0.0815 #the probability of a false positive screening mammogram result
  
  #screen_delay <- 2 #the amount of months delay for a screening mammogram.
                     #Yami says it will vary, for now setting it to two months.
  
  attrib_mtrx<-cbind(symptom.severity,
                     reg.pcp.visitor,
                     navigated,
                     antinavigated,
                     neighbor_navigated,
                     neighborfp)
  
  all_agents<-which(diagnosis==0)
  navigated_agents<-which(navigated==1)
  
  for (agent in all_agents){
    agent_data<-attrib_mtrx[agent,]
    
    #first deal with navigated and unnavigated screening mammogram patients
    #second deal with navigated diagnostic test patients
    if(screening_referral[agent]==1){
     
      screen_complete[agent]<-rbinom(1,1,prob(agent_data,"sm"))
      
      if(screen_complete[agent]==1){
        screening_referral[agent]<-0 #reset the referral
        if(bc_status[agent]==1){
          screen_result[agent]<-rbinom(1,1,(1-p_false_negative_sm))
        }
        else if(bc_status[agent]==0){
          screen_result[agent]<-rbinom(1,1,p_false_positive_sm)
        }
        
        #inputting diagnostic test referrals to positive screening mammogram pts.
        if(screen_result[agent]==1){
          diagnostic_referral[agent]<-1
        }
      }
    }
 
    if(diagnostic_referral[agent]==1){
    
      dt_complete[agent]<-rbinom(1,1,prob(agent_data,"dt"))
      
      if(dt_complete[agent]==1){
        diagnostic_referral[agent]<-0 #reset the referral
        if(bc_status[agent]==1){
          diagnosis[agent]<-1
          diagnosis_time[agent]<-disease.time[agent]
        }
        else if(bc_status[agent]==0){
          antinavigated[agent]<-1
          neighbors<-get.neighborhood(net.f,agent)
          for(neighbor in neighbors){
              neighborfp[neighbor]<-rbinom(1,1,0.5)
          }
        }
      }
    }
  }
  
  #commented out for burnin------------
  
  #for (agent in navigated_agents){
  #  neighbors<-get.neighborhood(net.f,agent)
  #  for (neighbor in neighbors){
  #    neighbor_navigated[neighbor]<-rbinom(1,1,0.72)
  #  }
  #}
  
  #commented out for burnin------------
  
  net.f %v% "navigated" <- navigated
  
  net.f %v% "diagnosis" <- diagnosis
  net.f %v% "diagnosis_time" <- diagnosis_time
  
  net.f %v% "diagnostic_test_result" <- diagnostic_test_result
  net.f %v% "diagnostic_referral" <- diagnostic_referral
  net.f %v% "diagnostic_test_complete" <- dt_complete
  
  net.f %v% "screen_result" <- screen_result
  net.f %v% "screening_referral" <- screening_referral
  net.f %v% "screen_complete" <- screen_complete
  
  net.f %v% "neighbor_navigated" <- neighbor_navigated
  net.f %v% "neighborfp" <- neighborfp
  
return(net.f)
}