# Estimation

# Top Matter ----

rm(list=ls())
library(network)
library(sna)
library(ergm)
library(networkDynamic)

#parameter_file <- "test_parameters.R" #comment out for test
#source(parameter_file, echo=T)

main_estimation <- function(parameter_file){
source(parameter_file)
# Estimate unipartite ERGM ----

#initalize network
n0 <- network.initialize(n, directed=F, bipartite=FALSE)

# network parameters
deg.spec <- 2:5
formation <- ~edges+degree(deg.spec)
#formation <- ~edges
target.stats <- c(n.edges,ego.alter.deg.nodes[deg.spec+1])

formation.n0 <- update.formula(formation, n0~.)
constraints <- ~.

fit_n0 <- ergm(formation.n0,
               target.stats=target.stats,
               constraints=constraints,
               eval.loglik=FALSE,
               verbose=FALSE,
               control=control.ergm(MCMLE.maxit=500)
               )

#save(fit_n0, file = "fit_n0_exp.RData")
#save(fit_n0, file = "fit_n0_exp3.RData")
#save(fit_n0, file = "fit_n0_exp4.RData")

#mcmc.diagnostics(fit_n0)

#size.of.timestep<-30 #30 days.
#duration.1000 <- (2221+1000)/size.of.timestep
duration<-999999999
dissolution <- ~offset(edges)
theta.diss <- log(duration-1)

theta.form <- fit_n0$coef
#theta.form[1] <- theta.form[1] - theta.diss

net0_bip <- simulate(fit_n0,
                     formation=formation, dissolution=dissolution,
                     coef.form=theta.form, coef.diss=theta.diss)
summary(net0_bip ~ degree(0:5))
# gof tests
ego.alter.deg.dis*100

degdist.net0_bip <- degreedist(net0_bip)
length(degdist.net0_bip)
names(degdist.net0_bip)

deg.seq <- degree(net0_bip, gmode = "graph")
deg.seq.ego <- deg.seq[1:n.ego] #degrees of egos
table(deg.seq.ego)/sum(table(deg.seq.ego))*100

deg.seq.alter <- deg.seq[(n.ego+1):n] #degrees of egos
table(deg.seq.alter)/sum(table(deg.seq.alter))*100

# Initialize Population ----

## set attributes for egos and alters
net0_bip %v% "type" <- rep(c("ego", "alter"),
                           c(n.ego, n.alter))

##age
net0_bip %v% "age" <- sample(min.age:max.age, n, replace = TRUE)  

## race: 100% of egos are black (=1); 95% of alters are black, 5% are other (=1) 
set.vertex.attribute(net0_bip, "race", 1, (1:n.ego))
set.vertex.attribute(net0_bip, "race", rbinom(n.alter, 1, percent.alters.black), 
                     ((n.ego+1):n)
                    )
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

# classify the BC states = 1 as hormone positive or hormone negative
# for (bc_status == 1){
    # horm.post based on menopause and obesity
#}

#Set vertex attribute for symptom severity
set.vertex.attribute(net0_bip, "symptom.severity", 0) #All people initially non-symptomatic

#Set vertex attribute for time since disease development
set.vertex.attribute(net0_bip, "disease.time", 0) #initially 0, includes agents without breast cancer
                                                           #have to populate initial breast cancer population with some distribution

#Set vertex attribute for regular pcp visits
set.vertex.attribute(net0_bip, "reg.pcp.visitor", 0)
set.vertex.attribute(net0_bip, "reg.pcp.visitor", rbinom(n, 1, prop.pcp.visitor))

## separate egos and alters, and classify alters as first degree relative, 
## relative, not-related
## for egos, all are black, for alters 95% are black

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
#net0_bip %v% "navigated"<- rbinom(length(net0_bip %v% "navigated"),1,0.02)

set.vertex.attribute(net0_bip, "neighbor_navigated", 0)
set.vertex.attribute(net0_bip, "neighborfp", 0)
set.vertex.attribute(net0_bip, "antinavigated", 0)
set.vertex.attribute(net0_bip, "screen_complete", 0)
set.vertex.attribute(net0_bip, "diagnostic_test_complete", 0)

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
return(net0_bip)

}