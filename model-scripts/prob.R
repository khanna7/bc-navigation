source("model-scripts/parameters.R")

prob<-function(agent_data,test,diagnostic_referral_length){
  
  #probability<-0.0034 #time component
  probability <- 0.0034
  
  #Changing probability so 80% of women finish diagnostic referrals before 2 months/60 days
  if(test == "dt" & (diagnostic_referral_length == 1 | diagnostic_referral_length == 2 )){
    probability <- 0.025
  }

  agent_symptom_severity<-agent_data[1]
  agent_regular_pcp_visitor<-agent_data[2]
  agent_navigated<-agent_data[3]
  agent_antinavigated<-agent_data[4]
  agent_neighbor_navigated<-agent_data[5]
  agent_neighbor_false_positive<-agent_data[6]

  if(agent_symptom_severity>=2 & test=="sm"){probability<-probability*screening_mammogram_symptomatic_assumption}
  
  if(agent_symptom_severity==1 & test=="dt"){probability<-probability*diagnostic_testing_SS1_assumption}
  else if(agent_symptom_severity==2 & test=="dt"){probability<-probability*diagnostic_testing_SS2_calculation}
  else if(agent_symptom_severity==3 & test=="dt"){probability<-probability*diagnostic_testing_SS3_calculation}
  
  if(agent_regular_pcp_visitor==1){probability<-probability*regular_pcp_visitor_oddsratio} 
  #same as being up to date on mammograms in paper
  
  if(agent_navigated==1 & test=="sm"){probability<-probability*screening_mammogram_navigated_oddsratio}
  if(agent_navigated==1 & test=="dt"){probability<-probability*diagnostic_test_navigated_oddsratio}
  
  if(agent_antinavigated==1){probability<-probability*antinavigation_assumption}
  
  if(agent_neighbor_navigated==1){probability<-probability*neighbor_navigation_oddsratio}
  
  if(agent_neighbor_false_positive==1){probability<-probability*neighbor_false_positive_assumption}
  
  return(min(probability,1))
}
