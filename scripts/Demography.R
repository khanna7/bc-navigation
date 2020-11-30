
#Demography module

demography <- function(net.f){
 
# Aging -------------------
  
  #Update age per time step 
  age <- net.f %v% "age"
  age <- age + 1/12
  
  screening_referral_time<- net.f %v% "screening_referral_time"
  diagnostic_referral_time<- net.f %v% "diagnostic_referral_time"
  
  screening_referral_time<-screening_referral_time + 1
  diagnostic_referral_time<-diagnostic_referral_time + 1
  
  net.f %v% 'age' <- age
  net.f %v% "screening_referral_time"<-screening_referral_time
  net.f %v% "diagnostic_referral_time"<-diagnostic_referral_time
  

# Departures -------------------
  #Aging out of model
  age_mortality <- which(age >= 75 & is.active(net.f, v = 1:network.size(net.f), at=time))
  
  if (length(age_mortality > 0)){
    net.f <- deactivate.vertices(net.f, onset = time, terminus = Inf, v=age_mortality)
    age_mortality_edges  <- list() #array(dim = length(age_mortality))
    for (i in seq(length(age_mortality))){
      age_mortality_edges <- list(age_mortality_edges, 
                                  list(get.edgeIDs.active(net.f, v =age_mortality[i], at = time)))
    }
    age_mortality_edges <- unlist(age_mortality_edges)
    if (length(age_mortality_edges) > 0 ){
      net.f <- deactivate.edges(net.f, onset = time, terminus = Inf, e = age_mortality_edges)
    }
  }

  #Activation of remaining network 
  node.active <- is.active(net.f, v =1:network.size(net.f), at = time)
  active.nodes <- which(node.active)
  popsize.temp <- sum(node.active)
  if (popsize.temp == 0) break 
  
  #Mortality from breast cancer
  #Yami thinks we should exclude mortality from best cancer
  
  disease.time <- net.f %v% "disease.time"
  diagnosis.time <- net.f %v% "diagnosis.time"
  late.diag.mortality <- 60 #for now, need to empirically determine - indicates max lifetime after onset of breast cancer 

  #This will depend on subtype and diagnosis time, in addition to disease time
  bc_mortality <- which(disease.time==late.diag.mortality &
                          diagnosis.time >= 12 & 
                          is.active(net.f, v =1:network.size(net.f), at = time))
  
  if (length(bc_mortality) > 0){
    net.f <- set.vertex.attribute(net.f, "time.of.death", time, v=bc_mortality)
    net.f <- deactivate.vertices(net.f, onset = time, terminus = Inf, v = bc_mortality)
  
    bc_mortality_edges <- list()
    for (i in bc_mortality){
      bc_mortality_edges <- list(bc_mortality_edges, list(get.edgeIDs.active(net.f, i, at - time)))
    }
    bc_mortality_edges <- unlist(bc_mortality_edges)
      
    if (length(bc_mortality_edges) > 0){
      net.f <- deactivate.edges(net.f, onset = time, terminus = Inf, e = bc_mortality_edges)
    }
  }

  #Update remaining active nodes
  node.active <- is.active(net.f, v = 1:network.size(net.f), at = time)
  active.nodes <- which(node.active)
  popsize_temp <- sum(node.active)

  if (popsize_temp == 0) break


  #Update active nodes
  node.active <- is.active(net.f, v = 1:network.size(net.f), at = time)
  active.nodes <- which(node.active)
  popsize.temp <- sum(node.active)
  if(popsize.temp == 0) break

  #All cause mortality 
  all_cause_mort <- array(NA, dim = length(popsize.temp))
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
  
  if (length(all_cause_ids) > 0){
    net.f <- set.vertex.attribute(net.f, "time.of.death", time, v=all_cause_ids)
    net.f <- deactivate.vertices(net.f, onset = time, terminus = Inf, v = all_cause_ids)
    all_cause_edges = array(dim = length(all_cause_ids))  
    all_cause_edges <- list()
    for (i in length(all_cause_ids)){
      all_cause_edges <-  list(all_cause_edges, list(get.edgeIDs.active(net.f, all_cause_ids[i], at = time)))
    }
    all_cause_edges <- unlist(all_cause_edges)
    if (length(all_cause_edges) > 0){
      net.f <- deactivate.edges(net.f, onset = time, terminus = Inf, e = all_cause_edges) # unlist? 
    }
  }
  
  #Update remaining active nodes
  node.active <- is.active(net.f, v = 1:network.size(net.f), at = time)
  active.nodes <- which(node.active)
  popsize_temp <- sum(node.active)
  
  if (popsize_temp == 0) break 
  
# Entries -------------------  
  
  nintros <- rpois(1, popsize_temp*.001) 
  
  #Add new vertices to the network
  net.f <- add.vertices(net.f, nintros)
  nodes.to.activate <- which(is.na(net.f %v% 'age'))
  print(c(length(nodes.to.activate), "nodes to activate"))
  
  #Initialize attributes for newly added vertices
  
  ## set attributes for egos and alters
  set.vertex.attribute(net.f, "type", sample(c("n.ego", "n.alter"),1, prob = c(.1, .9)), nodes.to.activate)
  
  ##age
  set.vertex.attribute(net.f, "age", min.age, nodes.to.activate)
  
  ## race: 100% of egos are black (=1); 95% of alters are black, 5% are other (=1) 
  set.vertex.attribute(net.f, "race", 1,nodes.to.activate)
  set.vertex.attribute(net.f, "race", rbinom(n.alter, 1, percent.alters.black), nodes.to.activate)
  
  #obesity
  set.vertex.attribute(net.f, "bmi.ge.30", 0, nodes.to.activate)
  set.vertex.attribute(net.f, "bmi.ge.30", rbinom(n, 1, prop.bmi.ge.30), nodes.to.activate)
  
  #menopausal status
  set.vertex.attribute(net.f, "meno.status", 0, nodes.to.activate )
  
  #First degree relative history
  set.vertex.attribute(net.f, "fd.rel", 0, nodes.to.activate)
  set.vertex.attribute(net.f, "fd.rel", rbinom(length(nodes.to.activate), 1, prop.fd.rel), nodes.to.activate)
  
  # compute initial risk for each agent
  set.vertex.attribute(net.f, "bc_hpos_risk", 0, nodes.to.activate)
  set.vertex.attribute(net.f, "bc_hneg_risk", 0, nodes.to.activate)
  
  # populate breast cancer status
  #create an attribute "bc.status": populate it based on the initial risk estimates
  initialized_bc <- rbinom(length(nodes.to.activate), 1, .015)
  subtype <- rep(0, length(nodes.to.activate))
  subtype[which(initialized_bc == 1)] <- sample(c(1,2), 1, prob = c(hpos_risk, 1-hpos_risk))
  set.vertex.attribute(net.f, "bc_status", initialized_bc, nodes.to.activate) 
  
  # set vertex attribute for bc subtype
  set.vertex.attribute(net.f, "subtype", subtype, nodes.to.activate)
  
  #Set vertex attribute for symptom severity
  set.vertex.attribute(net.f, "symptom.severity", 0, nodes.to.activate) #All people initially non-symptomatic
  
  #Set vertex attribute for time since disease development
  set.vertex.attribute(net.f, "disease.time", 0, nodes.to.activate) #initially 0, includes agents without breast cancer
  #have to populate initial breast cancer population with some distribution
  
  #Set vertex attribute for regular pcp visits
  set.vertex.attribute(net.f, "reg.pcp.visitor", 0, nodes.to.activate)
  set.vertex.attribute(net.f, "reg.pcp.visitor", rbinom(nodes.to.activate, 1, prop.pcp.visitor), nodes.to.activate)
  
  #set vertex attribute for whether or not a patient has received a diagnostic referral
  set.vertex.attribute(net.f, "diagnostic_referral", 0, nodes.to.activate)
  
  #set vertex attribute for whether or not a patient has received a screening referral
  set.vertex.attribute(net.f, "dt_complete", 0, nodes.to.activate)
  
  #set vertex attribute for whether or not a patient has received a screening referral
  set.vertex.attribute(net.f, "screening_referral", 0, nodes.to.activate)
  
  #set vertex attribute for whether or not a patient has received a screening referral
  set.vertex.attribute(net.f, "screen_result", 0, nodes.to.activate)
  
  #set vertex attribute for whether or not a patient has been navigated
  set.vertex.attribute(net.f, "navigated", 0, nodes.to.activate)
  
  set.vertex.attribute(net.f, "neighbornav", 0, nodes.to.activate)
  set.vertex.attribute(net.f, "neighborfp", 0, nodes.to.activate)
  set.vertex.attribute(net.f, "antinavigated", 0, nodes.to.activate)
  set.vertex.attribute(net.f, "screen_complete", 0, nodes.to.activate)
  set.vertex.attribute(net.f, "diagnostic_test_complete", 0, nodes.to.activate)
  
  #set vertex attribute for whether a patient has been diagnosed
  set.vertex.attribute(net.f, "diagnosis", 0, nodes.to.activate)
  
  #set vertex attribute for disease time at diagnosis
  set.vertex.attribute(net.f, "diagnosis_time", 0, nodes.to.activate)
  
  #set vertex attribute for disease time at diagnosis
  set.vertex.attribute(net.f, "diagnosis_referral_time", 0, nodes.to.activate)
  
  # Save object -----
  
  number.of.positive.bc.agents<-length(which(net.f %v% "bc_status"==1))
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
                    number.of.positive.bc.agents,
                    number.of.diagnosed.cases,
                    number.of.navigated.agents,
                    
                    number.of.diagnostic.referrals,
                    number.of.screening.referrals,
                    
                    number.of.screen.completed,
                    number.of.dt.completed,
                    
                    number.of.symptomatic),
              file="bc.dem.data", ## to note total number of new infections
              append=TRUE,
              col.names=FALSE,
              row.names=FALSE
              )
  
return(net.f) 
}
