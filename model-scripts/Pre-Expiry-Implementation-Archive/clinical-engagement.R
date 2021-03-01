## module for clinical engagement

#baseline screening
# Load libraries ---------------------------

library(ergm)

#Initialize function

clinical_engagement <- function(net.f, institutional, social, control, time_step){
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
  screening_referral_checker <- net.f %v% "screening_referral_checker"
  number_navigated_at_t <- 0
  rolls_for_navigation <- 0
  
  navigation_start_time <- net.f %v% "navigation_start_time"
  navigation_end_time <- net.f %v% "navigation_end_time"
  

  attrib_mtrx<-cbind(symptom.severity,
                     reg.pcp.visitor,
                     navigated,
                     antinavigated,
                     neighbor_navigated,
                     neighborfp)
  
  all_agents<-which(net.f %v% "diagnosis" == 0)
  #cat("NAV START: ", net.f %v% "navigation_start_time", "\n")
  
  for (agent in all_agents){
    agent_data<-attrib_mtrx[agent,]
    #first: symptomatic agents without referrals roll for dt referrals
    if(diagnostic_referral[agent]==0 &
       screening_referral[agent]==0 &
       ss[agent]>0){
       diagnostic_referral[agent]<-rbinom(1,1,prob(agent_data,"dt"))
       diagnostic_referral_counter[agent]<-diagnostic_referral_counter[agent]+diagnostic_referral[agent]
      if(diagnostic_referral[agent] == 1){ #navigate direct-diagnosis agents
        navigated[agent]<-rbinom(1,1, prob_institutional_navigation) #random institutional navigation
        rolls_for_navigation <- rolls_for_navigation + 1
        number_navigated_at_t <- number_navigated_at_t + navigated[agent]
        ##NEW 
        if(navigated[agent]==1){
          navigation_start_time[agent] <- time_step
        }
      }
        }
    
    #second: all agents without referrals roll for sm referrals
    if(diagnostic_referral[agent]==0 &
        screening_referral[agent]==0){
      	  screening_referral[agent]<-rbinom(1,1,prob(agent_data,"sm"))
      	  screening_referral_counter[agent]<-screening_referral_counter[agent]+screening_referral[agent]
	        if(screening_referral[agent]==1){ #Limiting navigation rolls to the step where a referral is given
	          screening_referral_checker[agent] <- 1
	          if(institutional == TRUE){
       	            	#simulate navigation
		          if(isTRUE((navigated[agent]==0) &
       			          (dt_complete[agent]==0) &
       		         (screen_complete[agent]==0) &
       		      (diagnostic_referral[agent]==1 |
       		        screening_referral[agent]==1)
       		         )){
	                    navigated[agent]<-rbinom(1,1, prob_institutional_navigation) #random institutional navigation
	                    rolls_for_navigation <- rolls_for_navigation + 1
	                    number_navigated_at_t <- number_navigated_at_t + navigated[agent]
	                     if(navigated[agent]==1){
	                       navigation_start_time[agent] <- time_step
	                     }
	        #cat("navigated[agent]:", navigated[agent], "\n")
        	#cat("prob_institutional_navigation:", prob_institutional_navigation, "\n")
        	
	        #cat(net.f %v% "diagnostic_test_complete",file="dt.complete.txt",sep="\n",append=TRUE)
        	#cat(capture.output(table(net.f %v% "diagnostic_test_complete", exclude = 			   NULL)),file="dt.complete.txt",sep="\n",append=TRUE)
        
        	           }
	            }
        }
   }
    
    if(social == TRUE){ 
       if(isTRUE((navigated[agent]==0) &
          (dt_complete[agent]==0) &
          (screen_complete[agent]==0) &
          (diagnostic_referral[agent]==1 |
           screening_referral[agent]==1) &
          (neighbor_navigated[agent]==1) #key component
       )){
         navigated[agent]<-rbinom(1,1,prob_social_navigation) #social navigation
         if(navigated[agent]==1){
           navigation_start_time[agent] <- time_step
         }
       }
    }
  }
  cat("Number navigated at time t: ", number_navigated_at_t, "\n")
  cat("Rolls for navigation at time t: ", rolls_for_navigation, "\n")
  cat("Agents navigated per roll: ", number_navigated_at_t/rolls_for_navigation, "\n")
  #cat(capture.output(table(net.f %v% "diagnostic_test_complete", exclude = NULL)),file="dt.complete.txt",sep="\n",append=TRUE)
  
  net.f %v% "diagnostic_referral_counter" <- diagnostic_referral_counter
  net.f %v% "screening_referral_counter" <- screening_referral_counter
  net.f %v% "navigated" <- navigated
  net.f %v% "screening_referral" <- screening_referral
  net.f %v% "diagnostic_referral" <- diagnostic_referral
  net.f %v% "screening_referral_checker" <- screening_referral_checker
  
  net.f %v% "navigation_start_time" <- navigation_start_time
  net.f %v% "navigation_end_time" <- navigation_end_time
  
return(net.f)
}
