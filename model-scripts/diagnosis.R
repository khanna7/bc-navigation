##
## Diagnostic process modelling module ##
##
# Load Dependencies 

library(ergm)
source("model-scripts/parameters.R")


#Initialize Function
diagnosis <- function(net.f, social, time_step){
  
  ####Pull In Individual Attributes 
  
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
  
  navigation_start_time <- net.f %v% "navigation_start_time"
  navigation_end_time <- net.f %v% "navigation_end_time"
  navigation_length <- net.f %v% "navigation_length"
  
  diagnostic_referral_counter <- net.f %v% "diagnostic_referral_counter"
  screening_referral_counter <- net.f %v% "screening_referral_counter"
  
  #referral expiration
  screening_referral_length <- net.f %v% "screening_referral_length"
  diagnostic_referral_length <- net.f %v% "diagnostic_referral_length"
  
  screening_referral_expired <- net.f %v% "screening_referral_expired"
  diagnostic_referral_expired <- net.f %v% "diagnostic_referral_expired"
  screening_referral_expired_at_t <- 0
  diagnostic_referral_expired_at_t <- 0
  
  #Social Navigation
  navigate_next_referral <- net.f %v% "navigate_next_referral"
  
  #cat("Non-0 diagnostic_ref_length: ",diagnostic_referral_length[which(net.f %v% "diagnostic_referral_length" != 0)],"\n")
  cat("Non-0 total care length: ",screening_referral_length[which(net.f %v% "diagnostic_referral_length" != 0)] + diagnostic_referral_length[which(net.f %v% "diagnostic_referral_length" != 0)],"\n")
  
  ####Create Attribute Matrix (Mickey's solution) 
  
  attrib_mtrx<-cbind(symptom.severity,
                     reg.pcp.visitor,
                     navigated,
                     antinavigated,
                     neighbor_navigated,
                     neighborfp)
  
  ####Isolate Undiagnosed and Navigated Subpopulations 
  
  all_agents <- which(diagnosis==0)
  navigated_agents <- which(navigated==1)
  
  for(nav_agent in navigated_agents){
    navigation_length[nav_agent] <- navigation_length[nav_agent] + 1
  }
  ####Begin Diagnostic Logic
  for (agent in all_agents){
    agent_data<-attrib_mtrx[agent,]
    current_timestep_social_nav_counter <- 0
    ####Check for referral expiration 
    #Increments lengths of navigation and referrals if still active
    if(screening_referral[agent] == 1){
      screening_referral_length[agent] <- screening_referral_length[agent] + 1
    }
    if(screening_referral[agent] == 0){
      screening_referral_length[agent] <- 0
    }
    
    if(diagnostic_referral[agent] == 1){
      diagnostic_referral_length[agent] <- diagnostic_referral_length[agent] + 1
    }
    if(diagnostic_referral[agent] == 0){
      diagnostic_referral_length[agent] <- 0
    }
    
    #Referrals expire if longer than referral_max_length
    if(screening_referral_length[agent] > 12){
      
      #Log Agent Information at time of SCREENING referral completion (expiration in this case)
      if(slurm == TRUE){
        slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
      } else{ 
        slurm_arrayid <- 1
      }
      numericid = as.numeric(slurm_arrayid)
      filename = paste(numericid, "_screening.events", sep="")
      
      #This writes one line per end
      write.table(cbind(time_step,
                        agent,
                        -1,#screening_referral_length[agent],
                        1, #screening_referral_expired[agent],
                        FALSE, #Length of referral < 2 months
                        symptom.severity[agent],
                        navigated[agent], #1 if agent currently navigated
                        as.numeric(slurm_arrayid), #instance number
                        bc_status[agent],#breast cancer status
                        neighbor_navigated[agent]), 
                  
                  file=filename,
                  append=TRUE,
                  col.names=FALSE,
                  row.names=FALSE)
      
      #measure number of expirations (recorded and reset each step in demography_reset.R)
      screening_referral_expired[agent] <- screening_referral_expired[agent] + 1
      screening_referral[agent] <- 0
      screening_referral_length[agent] <- 0
      
      #navigation also expires if applicable
      navigated[agent] <- 0
      navigation_length[agent] <- 0
      neighbor_navigated_roll[agent]<-0
    }
    
    #Diagnostic Referral Expiration
    if(diagnostic_referral_length[agent] > 12){
      
      diagnostic_referral_expired[agent] <- diagnostic_referral_expired[agent] + 1
      diagnostic_referral[agent] <- 0
      
      #Log Agent Information at time of DIAGNOSTIC referral completion (expiration in this case)
      if(slurm == TRUE){
        slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
      } else{ 
        slurm_arrayid <- 1
      }
      numericid = as.numeric(slurm_arrayid)
      filename = paste(numericid, "_diagnostic.events", sep="")
      
      #This writes one line per end
      write.table(cbind(time_step,
                        agent,
                        -1,#diagnostic_referral_length[agent],
                        1,#diagnostic_referral_expired[agent],
                        FALSE, #Length < 2 months
                        symptom.severity[agent],
                        navigated[agent],
                        screening_referral_length[agent],#screening_referral_length
                        screening_referral_length[agent] + diagnostic_referral_length[agent],#total_care_time[agent]
                        as.numeric(slurm_arrayid), #instance number
                        bc_status[agent],#breast cancer status
                        neighbor_navigated[agent]), 
                  
                  file=filename,
                  append=TRUE,
                  col.names=FALSE,
                  row.names=FALSE)
      
      diagnostic_referral_length[agent] <- 0
      #navigation also expires if applicable
      navigated[agent] <- 0
      navigation_length[agent] <- 0
      neighbor_navigated_roll[agent]<-0
      #TODO check to see if I need to reset diagnostic_referral_length here (or other vars?)
    }
    
    ####Simulate Screening and Diagnostic Testing 
    
    ####Screening Mammograms
    if(screening_referral[agent]==1){
      
      #roll to see if they complete the visit
      screen_complete[agent]<-rbinom(1,1,prob(agent_data,"sm",0)) #NOTE: prob() defined in prob.R
      screening_visit_counter[agent]<-screen_complete[agent]+screening_visit_counter[agent]
      
      #if they completed, process their results
      if(screen_complete[agent]==1){
        screening_referral[agent]<-0 #reset the referral
        screening_visit_checker[agent]<-1
        
        #Log Agent Information at time of SCREENING referral completion
        if(slurm == TRUE){
          slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
        } else{ 
          slurm_arrayid <- 1
        }
        numericid = as.numeric(slurm_arrayid)
        filename = paste(numericid, "_screening.events", sep="")
        
        #This writes one line per end
        write.table(cbind(time_step,
                          agent,
                          screening_referral_length[agent],#screening_referral_length
                          0, #screening_referral_expired[agent],
                          screening_referral_length[agent] <= 2, #Length of referral < 2 months
                          symptom.severity[agent],
                          navigated[agent], #1 if agent currently navigated
                          as.numeric(slurm_arrayid), #instance number
                          bc_status[agent],#breast cancer status
                          neighbor_navigated[agent]), 
                    
                    file=filename,
                    append=TRUE,
                    col.names=FALSE,
                    row.names=FALSE)
        
        if(bc_status[agent]==1){
          screen_result[agent]<-rbinom(1,1,(1-p_false_negative_sm))
        }
        else if(bc_status[agent]==0){
          screen_result[agent]<-rbinom(1,1,p_false_positive_sm)
        }
        if(screen_result[agent]==0){
          navigated[agent] <- 0
          navigation_length[agent] <- 0
          neighbor_navigated_roll[agent]<-0
        }
        #inputting diagnostic test referrals to positive screening mammogram pts.
        if(screen_result[agent]==1){
          diagnostic_referral[agent]<-1
          diagnostic_referral_length[agent] <- 0
          diagnostic_referral_checker[agent]<-1
          diagnostic_referral_counter[agent]<-diagnostic_referral_counter[agent]+diagnostic_referral[agent]
          
        }
      }
      #conclude screening mammograms
    }
    
    ####Diagnostic Tests
    if(diagnostic_referral[agent]==1 & diagnostic_referral_length[agent]!=0){ #don't allow screening and diagnosis in the same time step
      
      #roll to see if they complete the visit
      dt_complete[agent]<-rbinom(1,1,prob(agent_data,"dt", diagnostic_referral_length[agent]))
      diagnostic_visit_counter[agent]<-dt_complete[agent]+diagnostic_visit_counter[agent]
      
      #if they completed, process their results
      if(dt_complete[agent]==1){
        
        #Log Agent Information at time of DIAGNOSTIC referral completion
        if(slurm == TRUE){
          slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
        } else{ 
          slurm_arrayid <- 1
        }
        numericid = as.numeric(slurm_arrayid)
        filename = paste(numericid, "_diagnostic.events", sep="")
        
        #This writes one line per end
        write.table(cbind(time_step,
                          agent,
                          diagnostic_referral_length[agent],
                          0,#diagnostic_referral_expired[agent],
                          diagnostic_referral_length[agent] <= 2,
                          symptom.severity[agent],
                          navigated[agent],
                          screening_referral_length[agent],#screening_referral_length
                          screening_referral_length[agent] + diagnostic_referral_length[agent],#total_care_time[agent]
                          as.numeric(slurm_arrayid),#instance number
                          bc_status[agent],#breast cancer status
                          neighbor_navigated[agent]), 
                    
                    
                    
                    file=filename,
                    append=TRUE,
                    col.names=FALSE,
                    row.names=FALSE)
        
        #Reset Clinical attributes
        diagnostic_referral[agent]<-0
        diagnostic_visit_checker[agent]<-1
        #End navigation
        navigated[agent] <- 0
        navigation_length[agent] <- 0
        neighbor_navigated_roll[agent]<-0
        
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
    
  }
  ###Implement social navigation 
  if(social == TRUE){
    primary_edge<- net.f %e% "primary edge"
    for (agent in navigated_agents){
      if(neighbor_navigated_roll[agent]==0){ #Navigated agents only get one roll to navigate neighbors
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
            if(isTRUE((navigated[neighbor]==0) &
                      neighbor_navigated[neighbor]==1 #key component
            )){
              navigate_next_referral[neighbor]<-rbinom(1,1,prob_social_navigation) #social navigation
              if(navigate_next_referral[neighbor] == 1){
                current_timestep_social_nav_counter <- current_timestep_social_nav_counter + 1
                cat("\n ------------ SOCIAL NAVIGATION HIT --------------\n")
                #cat("Number expected to be socially navigated: ", , "\n")
              } 
            }
          }
        }
        neighbor_navigated_roll[agent]<-1 #This labels the navigated agent so they only get one roll
        #cat(length(which(net.f %v% "navigated"==1)))
      }
    }
  }
  
  ####Update the network 
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
  
  net.f %v% "navigation_length" <- navigation_length
  
  net.f %v% "diagnostic_referral_counter" <- diagnostic_referral_counter
  net.f %v% "screening_referral_counter" <- screening_referral_counter
  
  #referral expirations
  net.f %v% "screening_referral_length" <- screening_referral_length
  net.f %v% "diagnostic_referral_length" <- diagnostic_referral_length
  
  net.f %v% "screening_referral_expired" <- screening_referral_expired
  net.f %v% "diagnostic_referral_expired" <- diagnostic_referral_expired
  
  net.f %v% "navigate_next_referral" <- navigate_next_referral
  cat("-----Local  NNR: ", current_timestep_social_nav_counter, "\n")
  cat("-----Global NNR: ", sum(net.f %v% "navigate_next_referral"))
  
  
  return(net.f)
}
