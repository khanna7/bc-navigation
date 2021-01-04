## module for clinical engagement

# Load libraries ---------------------------

library(ergm)

#Initialize function

source("model-scripts/parameters.R")

diagnosis <- function(net.f, social){
  
  ## get individual attributes 
  pop_size <- 5000
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
  neighbor_navigated_roll <- net.f %v% "neighbor_navigated_roll"
  neighborfp_roll <- net.f %v% "neighborfp_roll"
  
  diagnostic_visit_counter <- net.f %v% "diagnostic_visit_counter"
  screening_visit_counter <- net.f %v% "screening_visit_counter"
  
  diagnostic_referral_checker <- net.f %v% "diagnostic_referral_checker"
  diagnostic_visit_checker <- net.f %v% "diagnostic_visit_checker"
  screening_visit_checker <- net.f %v% "screening_visit_checker"
  
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
    
    ####1. Screening Mammograms##########################
    
    if(screening_referral[agent]==1){
     
      #roll to see if they complete the visit
      screen_complete[agent]<-rbinom(1,1,prob(agent_data,"sm"))
      screening_visit_counter[agent]<-screen_complete[agent]+screening_visit_counter[agent]
      
      #if they completed, process their results
      if(screen_complete[agent]==1){
        screening_referral[agent]<-0 #reset the referral
        screening_visit_checker[agent]<-1
        
        if(bc_status[agent]==1){
          screen_result[agent]<-rbinom(1,1,(1-p_false_negative_sm))
        }
        else if(bc_status[agent]==0){
          screen_result[agent]<-rbinom(1,1,p_false_positive_sm)
#TODO stop navigation here 
	  navigated[agent] <- 0
        }
        
        #inputting diagnostic test referrals to positive screening mammogram pts.
        if(screen_result[agent]==1){
          diagnostic_referral[agent]<-1
          diagnostic_referral_checker[agent]<-1
        }
      }
      #conclude screening mammograms
    }
    
    
    ####2. Diagnostic Tests##########################
 
    if(diagnostic_referral[agent]==1){
    
      #roll to see if they complete the visit
      dt_complete[agent]<-rbinom(1,1,prob(agent_data,"dt"))
      diagnostic_visit_counter[agent]<-dt_complete[agent]+diagnostic_visit_counter[agent]
      
      #if they completed, process their results
      if(dt_complete[agent]==1){
        diagnostic_referral[agent]<-0
        diagnostic_visit_checker[agent]<-1
#TODO stop navigation here (either way navigation ends with diagnostic testing)
	navigated[agent] <- 0
        if(bc_status[agent]==1){
          antinavigated[agent]<-0
          diagnosis[agent]<-1
          diagnosis_time[agent]<-disease.time[agent]
        }
        
        else if(bc_status[agent]==0){
          antinavigated[agent]<-1
          neighbors<-get.neighborhood(net.f,agent)
          for(neighbor in neighbors){
            if(neighborfp_roll[agent]==0){
              neighborfp[neighbor]<-rbinom(1,1,p_neighbor_fp)
              neighborfp_roll[agent]<-1
            }
          }
        }
        
      }
    }
    #conclude diagnostic tests
  }
  #conclude all appointments
  
  #introducing social navigation 
  if(social == TRUE){
    primary_edge<- net.f %e% "primary edge"
    
    for (agent in navigated_agents){
      if(neighbor_navigated_roll[agent]==0){
        
        agent_edges<-get.edgeIDs(net.f, v=agent)
        primary_edge_indices<-which(primary_edge[agent_edges]==1)
        if(length(primary_edge_indices)>0){
          primary_edges<-get.edges(net.f,v=agent)[1:length(primary_edge_indices)]
          neighbors<-c()
          
          for(i in 1:length(primary_edge_indices)){
            neighbors<-append(neighbors,primary_edges[[i]][[1]])
          }
          
          for (neighbor in neighbors){
            neighbor_navigated[neighbor]<-rbinom(1,1,p_neighbor_navigated)
            if(isTRUE((navigated[agent]==0) &
                      (dt_complete[agent]==0) &
                      (screen_complete[agent]==0) &
                      (diagnostic_referral[agent]==1 |
                       screening_referral[agent]==1) &
                      (neighbor_navigated[agent]==1) #key component
            )){
              navigated[agent]<-rbinom(1,1,prob_social_navigation) #social navigation
            }
          }
        }
      }
      neighbor_navigated_roll[agent]<-1
      #cat(length(which(net.f %v% "navigated"==1)))
    }
  }
  
  
  net.f %v% "screening_visit_checker" <- screening_visit_checker
  net.f %v% "diagnostic_visit_checker" <- diagnostic_visit_checker
  net.f %v% "diagnostic_referral_checker" <- diagnostic_referral_checker
  
  net.f %v% "screening_visit_counter" <- screening_visit_counter
  net.f %v% "diagnostic_visit_counter" <- diagnostic_visit_counter
  
  net.f %v% "neighborfp_roll" <- neighborfp_roll
  
  net.f %v% "neighbor_navigated_roll" <- neighbor_navigated_roll
  
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
  
  net.f %v% "diagnostic_visit_counter" <- diagnostic_visit_counter
  net.f %v% "screening_visit_counter" <- screening_visit_counter
  
return(net.f)
}
