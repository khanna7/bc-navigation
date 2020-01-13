prob<-function(agent_data,test){
  #start_time <- Sys.time()
  
  #symptom_severity_component<-1
  #regular_pcp_visitor_component<-1
  #navigated_component<-1
  #antinavigated_component<-1
  #neighbornav_component<-1
  #neighborantinav_component<-1
  
  probability<-0.0034 #time component
  
  if(agent_data[1]>=2 & test=="sm"){probability<-probability*3}
  
  if(agent_data[1]==1 & test=="dt"){probability<-probability*5}
  else if(agent_data[1]==2 & test=="dt"){probability<-probability*10.2}
  else if(agent_data[1]==3 & test=="dt"){probability<-probability*11.5}
  
  if(agent_data[2]==1){probability<-probability*14} #same as adherence
  
  if(agent_data[3]==1){probability<-probability*(3.63-2.04*agent_data[2])}
  
  if(agent_data[4]==1){probability<-probability*0.07142857}
  
  if(agent_data[5]==1){probability<-probability*3.8}
  
  if(agent_data[6]==1){probability<-probability*0.2631579}
  
  #if(ss_agent>=2 & test=="sm"){probability<-probability*3}
  #
  #if(ss_agent==1 & test=="dt"){probability<-probability*5}
  #else if(ss_agent==2 & test=="dt"){probability<-probability*10.2}
  #else if(ss_agent==3 & test=="dt"){probability<-probability*11.5}
  #
  #if(reg.pcp.visitor[agent]==1){probability<-probability*14} #same as adherence
  #
  #if(navigated[agent]==1){probability<-probability*(3.63-2.04*reg.pcp.visitor[agent])}
  #
  #if(antinavigated[agent]==1){probability<-probability*0.07142857}
  #
  #if(neighbornav[agent]==1){probability<-probability*3.8}
  #
  #if(neighborfp[agent]==1){probability<-probability*0.2631579}
  
  #if(ref_vs_test=="test"){time_component<-0.0035}
  #end_time <- Sys.time()
  #cat(end_time - start_time,'\n') 
  return(min(probability,1))
}