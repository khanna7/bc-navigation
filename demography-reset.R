
#Demography module

demography <- function(net.f){
  
  # Aging -------------------
  
  #Update age per time step
  age <- net.f %v% "age"
  age <- age + 1/12
  net.f %v% "age" <- age
  
  # Departures -------------------
  
  #Aging out of model
  age_mortality <- which(age >= 75)
  
  #Mortality from breast cancer
  disease.time <- net.f %v% "disease.time"
  diagnosis.time <- net.f %v% "diagnosis.time"
  cancer_death <- net.f %v% "cancer_death"
  
  bc_mortality <- which(cancer_death==1)
  
  #All cause mortality
  all_cause_mort <- array(NA, dim = 5000)
  for (i in seq(length(active.nodes))){
    if (age[active.nodes[i]] < 55){
      all_cause_mort[i] <- rbinom(1,1, ac.mort.50)
    } else if (age[active.nodes[i]] < 60){
      all_cause_mort[i] <- rbinom(1,1, ac.mort.55)
    } else if (age[active.nodes[i]] < 65){
      all_cause_mort[i] <- rbinom(1,1, ac.mort.60)
    } else if (age[active.nodes[i]] < 70){
      all_cause_mort[i] <- rbinom(1,1, ac.mort.65)
    } else if (age[active.nodes[i]] < 75){
      all_cause_mort[i] <- rbinom(1,1, ac.mort.70)
    }
  }
  all_cause_ids <- which(all_cause_mort == 1)
  
  nodes.to.reset <-union(all_cause_ids,age_mortality)
  nodes.to.reset <-union(nodes.to.reset,bc_mortality)
  nintros<-length(nodes.to.reset)
  cat(nintros, "new agents", "\n")
  if(nintros>0){
    
    #Initialize attributes for reset vertices
    
    ## set attributes for egos and alters
    set.vertex.attribute(net.f, "type", sample(c("n.ego", "n.alter"),1, prob = c(.1, .9)), nodes.to.reset)
    
    ##age
    set.vertex.attribute(net.f, "age", min.age, nodes.to.reset) #should age be randomized
    
    ## race: 100% of egos are black (=1); 95% of alters are black, 5% are other (=1) 
    set.vertex.attribute(net.f, "race", 1,nodes.to.reset)
    set.vertex.attribute(net.f, "race", rbinom(n.alter, 1, percent.alters.black), nodes.to.reset)
    
    #obesity
    set.vertex.attribute(net.f, "bmi.ge.30", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "bmi.ge.30", rbinom(n, 1, prop.bmi.ge.30), nodes.to.reset)
    
    #menopausal status
    set.vertex.attribute(net.f, "meno.status", 0, nodes.to.reset )
    
    #First degree relative history
    set.vertex.attribute(net.f, "fd.rel", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "fd.rel", rbinom(length(nodes.to.reset), 1, prop.fd.rel), nodes.to.reset)
    
    #compute initial risk for each agent
    set.vertex.attribute(net.f, "bc_hpos_risk", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "bc_hneg_risk", 0, nodes.to.reset)
    
    #populate breast cancer status
    #create an attribute "bc.status": populate it based on the initial risk estimates
    initialized_bc <- rbinom(length(nodes.to.reset), 1, .015)
    subtype <- rep(0, length(nodes.to.reset))
    subtype[which(initialized_bc == 1)] <- sample(c(1,2), 1, prob = c(hpos_risk, 1-hpos_risk))
    set.vertex.attribute(net.f, "bc_status", initialized_bc, nodes.to.reset)
    
    # set vertex attribute for bc subtype
    set.vertex.attribute(net.f, "subtype", subtype, nodes.to.reset)
    
    #Set vertex attribute for symptom severity
    set.vertex.attribute(net.f, "symptom.severity", 0, nodes.to.reset) #All people initially non-symptomatic
    
    #Set vertex attribute for time since disease development
    set.vertex.attribute(net.f, "disease.time", 0, nodes.to.reset) #initially 0, includes agents without breast cancer
    #have to populate initial breast cancer population with some distribution
    
    #Set vertex attribute for regular pcp visits
    set.vertex.attribute(net.f, "reg.pcp.visitor", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "reg.pcp.visitor", rbinom(nodes.to.reset, 1, prop.pcp.visitor), nodes.to.reset)
    
    #set vertex attribute for whether or not a patient has received a diagnostic referral
    set.vertex.attribute(net.f, "diagnostic_referral", 0, nodes.to.reset)
    
    #set vertex attribute for whether or not a patient has received a screening referral
    set.vertex.attribute(net.f, "dt_complete", 0, nodes.to.reset)
    
    #set vertex attribute for whether or not a patient has received a screening referral
    set.vertex.attribute(net.f, "screening_referral", 0, nodes.to.reset)
    
    #set vertex attribute for whether or not a patient has received a screening referral
    set.vertex.attribute(net.f, "screen_result", 0, nodes.to.reset)
    
    #set vertex attribute for whether or not a patient has been navigated
    set.vertex.attribute(net.f, "navigated", 0, nodes.to.reset)
    
    set.vertex.attribute(net.f, "neighbor_navigated", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "neighborfp", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "antinavigated", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "screen_complete", 0, nodes.to.reset)
    set.vertex.attribute(net.f, "diagnostic_test_complete", 0, nodes.to.reset)
    
    #set vertex attribute for whether a patient has been diagnosed
    set.vertex.attribute(net.f, "diagnosis", 0, nodes.to.reset)
    
    #set vertex attribute for disease time at diagnosis
    set.vertex.attribute(net.f, "diagnosis_time", 0, nodes.to.reset)
    
    #set vertex attribute for disease time at diagnosis
    set.vertex.attribute(net.f, "diagnosis_referral_time", 0, nodes.to.reset)
    
    #set vertex attribute for death due to cancer stage
    set.vertex.attribute(net.f, "cancer_death", 0, nodes.to.reset)
  }
  
  # Save object -----
  alive_agents<-which(is.active(net.f, v = 1:network.size(net.f), at = time))
  deceased_agents<-which(!is.active(net.f, v = 1:network.size(net.f), at = time))
  number.of.alive.agents<-length(alive_agents)
  number.of.deceased.agents<-length(deceased_agents)
  number.of.positive.bc.agents<-length(intersect(which(net.f %v% "bc_status"==1),alive_agents))
  number.of.diagnosed.cases<-length(which(net.f %v% "diagnosis"==1))
  number.of.navigated.agents<-length(which(net.f %v% "navigated"==1))
  
  number.of.diagnostic.referrals<-length(which(net.f %v% "diagnostic_referral"==1))
  number.of.screening.referrals<-length(which(net.f %v% "screening_referral"==1))
  
  number.of.screen.completed<-length(which(net.f %v% "screen_complete"==1))
  number.of.dt.completed<-length(which(net.f %v% "diagnostic_test_complete"==1))
  
  number.of.symptomatic<-length(which(net.f %v% "symptom.severity">0))
  #length.of.disease.time<-length(which(net.f %v% "disease">0)))
  #number.of.
  
  write.table(cbind(time,
                    nintros, #births
                    number.of.alive.agents,
                    number.of.deceased.agents,
                    number.of.positive.bc.agents,
                    number.of.diagnosed.cases,
                    number.of.navigated.agents,
                    
                    number.of.diagnostic.referrals,
                    number.of.screening.referrals,
                    
                    number.of.screen.completed,
                    number.of.dt.completed,
                    
                    number.of.symptomatic),
              file="bc.dem.burnin.fixed5000.for360.social_off.unipartite.data", ## to note total number of new infections
              append=TRUE,
              col.names=FALSE,
              row.names=FALSE
  )
  
  return(net.f) 
}