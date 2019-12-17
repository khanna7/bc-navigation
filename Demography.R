
#Demography module

demography <- function(net0_bip){
 
# Aging -------------------
  
  #Update age per time step 
  age <- net0_bip %v% "age"
  age <- age + 1/12
  screening_referral_time<-screening_referral_time + 1
  diagnostic_referral_time<-diagnostic_referral_time + 1
  net0_bip %v% 'age' <- age
  
# Departures -------------------
  #Aging out of model
  age_mortality <- which(age >= 75 & is.active(net0_bip, v = 1:network.size(net0_bip), at=time))
  
  if (length(age_mortality > 0)){
    net0_bip <- deactivate.vertices(net0_bip, onset = time, terminus = Inf, v=age_mortality)
    age_mortality_edges  <- list() #array(dim = length(age_mortality))
    for (i in seq(length(age_mortality))){
      age_mortality_edges <- list(age_mortality_edges, list(get.edgeIDs.active(net0_bip, v =age_mortality[i], at = time)))
    }
    age_mortality_edges <- unlist(age_mortality_edges)
    if (length(age_mortality_edges) > 0 ){
      net0_bip <- deactivate.edges(net0_bip, onset = time, terminus = Inf, e = age_mortality_edges)
    }
  }
   
  #Activation of remaining network 
  node.active <- is.active(net0_bip, v =1:network.size(net0_bip), at = time)
  active.nodes <- which(node.active)
  popsize.temp <- sum(node.active)
  if (popsize.temp == 0) break 
  
  #Mortality from breast cancer
  #Yami thinks we should exclude mortality from best cancer
  
  disease.time <- net0_bip %v% "disease.time"
  diagnosis.time <- net0_bip %v% "diagnosis.time"
  late.diag.mortality <- 100 #for now, need to empirically determine - indicates max lifetime after onset of breast cancer 
  
  #This will depend on subtype and diagnosis time, in addition to disease time
  bc_mortality <- which (disease.time == late.diag.mortality & diagnosis.time >= 12 & is.active(net0_bip, v =1:network.size(net0_bip), at = time))
  
  if (length(bc_mortality) > 0){
    net0_bip <- set.vertex.attribute(net0_bip, "time.of.death", time, v =bc_mortality)
    net0_bip <- deactivate.vertices(net0_bip, onset = time, terminus = Inf, v = bc_mortality)
  
    bc_mortality_edges <- list()
    for (i in bc_mortality){
      bc_mortality_edges <- list(bc_mortality_edges, list(get.edgeIDs.active(net0_bip, i, at - time)))
    }
    bc_mortality_edges <- unlist(bc_mortality_edges)
      
    if (length(bc_mortality_edges) > 0){
      net0_bip <- deactivate.edges(net0_bip, onset = time, terminus = Inf, e = bc_mortality_edges)
    }
  }

  #Update remaining active nodes
  node.active <- is.active(net0_bip, v = 1:network.size(net0_bip), at = time)
  active.nodes <- which(node.active)
  popsize_temp <- sum(node.active)

  if (popsize_temp == 0) break


  #Update active nodes
  node.active <- is.active(net0_bip, v = 1:network.size(net0_bip), at = time)
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
    net0_bip <- set.vertex.attribute(net0_bip, "time.of.death", time, v=all_cause_ids)
    net0_bip <- deactivate.vertices(net0_bip, onset = time, terminus = Inf, v = all_cause_ids)
    all_cause_edges = array(dim = length(all_cause_ids))
    
    all_cause_edges <- list()
    for (i in length(all_cause_ids)){
      all_cause_edges <-  list(all_cause_edges, list(get.edgeIDs.active(net0_bip, all_cause_ids[i], at = time)))
    }
    all_cause_edges <- unlist(all_cause_edges)
    if (length(all_cause_edges) > 0){
      net0_bip <- deactivate.edges(net0_bip, onset = time, terminus = Inf, e = all_cause_edges) # unlist? 
    }
  }
  
  #Update remaining active nodes
  node.active <- is.active(net0_bip, v = 1:network.size(net0_bip), at = time)
  active.nodes <- which(node.active)
  popsize_temp <- sum(node.active)
  
  if (popsize_temp == 0) break 
  
# Entries -------------------  

  nintros <- rpois(1, popsize_temp*.001) 
  
  #Add new vertices to the network
  net0_bip <- add.vertices(net0_bip, nintros)
  nodes.to.activate <- which(is.na(net0_bip %v% 'age'))
  print(c(length(nodes.to.activate), "nodes to activate"))
  
  #Initialize attributes for newly added vertices
  
  ## set attributes for egos and alters
  set.vertex.attribute(net0_bip, "type", sample(c("n.ego", "n.alter"),1, prob = c(.1, .9)), nodes.to.activate)
  
  ##age
  set.vertex.attribute(net0_bip, "age", min.age, nodes.to.activate)
  
  ## race: 100% of egos are black (=1); 95% of alters are black, 5% are other (=1) 
  set.vertex.attribute(net0_bip, "race", 1,nodes.to.activate)
  set.vertex.attribute(net0_bip, "race", rbinom(n.alter, 1, percent.alters.black), nodes.to.activate)
  
  #obesity
  set.vertex.attribute(net0_bip, "bmi.ge.30", 0, nodes.to.activate)
  set.vertex.attribute(net0_bip, "bmi.ge.30", rbinom(n, 1, prop.bmi.ge.30), nodes.to.activate)
  
  #menopausal status
  set.vertex.attribute(net0_bip, "meno.status", 0, nodes.to.activate )
  
  #First degree relative history
  set.vertex.attribute(net0_bip, "fd.rel", 0, nodes.to.activate)
  set.vertex.attribute(net0_bip, "fd.rel", rbinom(length(nodes.to.activate), 1, prop.fd.rel), nodes.to.activate)
  
  # compute initial risk for each agent
  set.vertex.attribute(net0_bip, "bc_hpos_risk", 0, nodes.to.activate)
  set.vertex.attribute(net0_bip, "bc_hneg_risk", 0, nodes.to.activate)
  
  # populate breast cancer status
  #create an attribute "bc.status": populate it based on the initial risk estimates
  initialized_bc <- rbinom(length(nodes.to.activate), 1, .015)
  subtype <- rep(0, length(nodes.to.activate))
  subtype[which(initialized_bc == 1)] <- sample(c(1,2), 1, prob = c(hpos_risk, 1-hpos_risk))
  set.vertex.attribute(net0_bip, "bc_status", initialized_bc, nodes.to.activate) 
  
  # set vertex attribute for bc subtype
  set.vertex.attribute(net0_bip, "subtype", subtype, nodes.to.activate)
  
  #Set vertex attribute for symptom severity
  set.vertex.attribute(net0_bip, "symptom.severity", 0, nodes.to.activate) #All people initially non-symptomatic
  
  #Set vertex attribute for time since disease development
  set.vertex.attribute(net0_bip, "disease.time", 0, nodes.to.activate) #initially 0, includes agents without breast cancer
  #have to populate initial breast cancer population with some distribution
  
  #Set vertex attribute for regular pcp visits
  set.vertex.attribute(net0_bip, "reg.pcp.visitor", 0, nodes.to.activate )
  set.vertex.attribute(net0_bip, "reg.pcp.visitor", rbinom(nodes.to.activate, 1, prop.pcp.visitor), nodes.to.activate)
  
  ## separate egos and alters, and classify alters as first degree relative, 
  ## relative, not-related
  ## for egos, all are black, for alters 95% are black
  
  #set vertex attribute for how long patient has had breast cancer
  set.vertex.attribute(net0_bip, "disease.time", 0, nodes.to.activate)
  
  #set vertex attribute for how long since patient's last pcp visit
  set.vertex.attribute(net0_bip, "time_since_pcp", 0, nodes.to.activate)
  
  #added oct 2, 2019.
  #set vertex attribute for whether or not a patient has received a screening mammogram referral
  set.vertex.attribute(net0_bip, "screening_referral", 0, nodes.to.activate)
  
  #set vertex attribute for whether or not a patient has received a diagnostic referral
  set.vertex.attribute(net0_bip, "diagnostic_referral", 0, nodes.to.activate)
  
  #set vertex attribute for whether a patient has been diagnosed
  set.vertex.attribute(net0_bip, "diagnosis", 0, nodes.to.activate)
  
  #set vertex attribute for disease time at diagnosis
  set.vertex.attribute(net0_bip, "diagnosis_time", 0, nodes.to.activate)
  
  
}