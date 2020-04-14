prob<-function(agent_data,test){
  
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
  
  return(min(probability,1))
}