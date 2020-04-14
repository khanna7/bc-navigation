# Estimation

rm(list=ls())
library(network)
library(sna)
library(ergm)
library(networkDynamic)
library(tergm)

source("parameters.R")

# Estimate unipartite ERGM ----

#initalize network
n0 <- network.initialize(n, directed=F, bipartite=FALSE)

#network parameters
deg.spec <- 2:5 #expanding further will lead to excessive model correlation.
formation <- ~edges+degree(deg.spec)
target.stats <- c(n.edges,ego.alter.deg.nodes[deg.spec+1])

formation.n0 <- update.formula(formation, n0~.)
constraints <- ~.
##############################

#Launching the network
fit_n0 <- ergm(formation.n0,
               target.stats=target.stats,
               constraints=constraints,
               eval.loglik=FALSE,
               verbose=FALSE,
               control=control.ergm(MCMLE.maxit=500)
               )

net0_bip <- simulate(fit_n0)
############################

#Summary stats and degree distribution of the network
summary(net0_bip ~ degree(0:20))
degdist.net0_bip <- degreedist(net0_bip)
length(degdist.net0_bip)
names(degdist.net0_bip)

deg.seq <- degree(net0_bip, gmode = "graph")
deg.seq.ego <- deg.seq[1:n.ego] #degrees of egos
table(deg.seq.ego)/sum(table(deg.seq.ego))*100

deg.seq.alter <- deg.seq[(n.ego+1):n] #degrees of egos
table(deg.seq.alter)/sum(table(deg.seq.alter))*100
#######################################################

#Initialize Population ----

#modify edge distribution to fit data better
set.edge.attribute(net0_bip, "primary edge",  0)
primary_edge <- net0_bip %e% "primary edge"

edges_vector<-c()
for(i in 1:network.size(net0_bip)){
  edges_vector<-append(edges_vector,length(get.edges(net0_bip,v=i)))
}

normally_connected_nodes<-which(edges_vector<=5)
normal_edges<-get.edgeIDs(net0_bip, v=normally_connected_nodes)
set.edge.attribute(net0_bip, 
                   e=normal_edges,
                   attrname = "primary edge", 1)

over_connected_nodes<-which(edges_vector>6)
for (node in over_connected_nodes){
  node_edges<-get.edgeIDs(net0_bip,v=node)
  chosen_edge_index<-sample(node_edges,1)
  set.edge.attribute(net0_bip, 
                     e=chosen_edge_index,
                     attrname = "primary edge", 1)
}
############


## set attributes for egos and alters
net0_bip %v% "type" <- rep(c("ego", "alter"),c(n.ego, n.alter))

##age
net0_bip %v% "age" <- sample(min.age:max.age, n, replace = TRUE)  

## race: 100% of egos are black (=1); 95% of alters are black, 5% are other (=1) 
set.vertex.attribute(net0_bip, "race", 1, (1:n.ego))
set.vertex.attribute(net0_bip, "race", rbinom(n.alter, 1, percent.alters.black),((n.ego+1):n))
table(net0_bip %v% "race")

#obesity
set.vertex.attribute(net0_bip, "bmi.ge.30", 0)
set.vertex.attribute(net0_bip, "bmi.ge.30", rbinom(n, 1, prop.bmi.ge.30))

#menopausal status
meno.status <- rep(0, n)

meno.age <- which(net0_bip %v% "age" >= 60)
for (i in meno.age){
  meno.status[i] <- 1
}

set.vertex.attribute(net0_bip, "meno.status", meno.status)

#First degree relative history
set.vertex.attribute(net0_bip, "fd.rel", 0)
set.vertex.attribute(net0_bip, "fd.rel", rbinom(n, 1, prop.fd.rel))

# compute initial risk for each agent
set.vertex.attribute(net0_bip, "bc_hpos_risk", 0)
set.vertex.attribute(net0_bip, "bc_hneg_risk", 0)

# populate breast cancer status
#create an attribute "bc.status": populate it based on the initial risk estimates
set.vertex.attribute(net0_bip, "bc_status", 0) 

# set vertex attribute for bc subtype
set.vertex.attribute(net0_bip, "subtype", 0)

#Set vertex attribute for symptom severity
set.vertex.attribute(net0_bip, "symptom.severity", 0) #All people initially non-symptomatic

#Set vertex attribute for time since disease development
set.vertex.attribute(net0_bip, "disease.time", 0) 
#initially 0, includes agents without breast cancer

#Set vertex attribute for regular pcp visits
set.vertex.attribute(net0_bip, "reg.pcp.visitor", 0)
set.vertex.attribute(net0_bip, "reg.pcp.visitor", rbinom(n, 1, prop.pcp.visitor))

#set vertex attribute for how long patient has had breast cancer
set.vertex.attribute(net0_bip, "disease.time", 0)

#set vertex attribute for whether or not a patient has received a diagnostic referral
set.vertex.attribute(net0_bip, "diagnostic_referral", 0)

#set vertex attribute for whether or not a patient has received a screening referral
set.vertex.attribute(net0_bip, "dt_complete", 0)

#set vertex attribute for whether or not a patient has received a screening referral
set.vertex.attribute(net0_bip, "screening_referral", 0)

#set vertex attribute for whether or not a patient has received a screening referral
set.vertex.attribute(net0_bip, "screen_result", 0)

#set vertex attribute for whether or not a patient has been navigated
set.vertex.attribute(net0_bip, "navigated", 0)
net0_bip %v% "navigated"<- rbinom(length(net0_bip %v% "navigated"),1,0.02)

set.vertex.attribute(net0_bip, "neighbor_navigated", 0)
set.vertex.attribute(net0_bip, "neighborfp", 0)
set.vertex.attribute(net0_bip, "antinavigated", 0)
set.vertex.attribute(net0_bip, "screen_complete", 0)
set.vertex.attribute(net0_bip, "diagnostic_test_complete", 0)
set.vertex.attribute(net0_bip, "screen_result_roll", 0)
set.vertex.attribute(net0_bip, "neighborfp_roll", 0)
set.vertex.attribute(net0_bip, "diagnostic_referral_counter", 0)
set.vertex.attribute(net0_bip, "screening_referral_counter", 0)
set.vertex.attribute(net0_bip, "diagnostic_visit_counter", 0)
set.vertex.attribute(net0_bip, "screening_visit_counter", 0)

#set vertex attribute for whether a patient has been diagnosed
set.vertex.attribute(net0_bip, "diagnosis", 0)

#set vertex attribute for disease time at diagnosis
set.vertex.attribute(net0_bip, "diagnosis_time", 0)

#set vertex attribute for disease time at diagnosis
set.vertex.attribute(net0_bip, "diagnosis_referral_time", 0)

#set vertex attribute for death due to cancer stage
set.vertex.attribute(net0_bip, "cancer_death", 0)

# Save object -----
save(net0_bip, file = "estimation_net.RData")

