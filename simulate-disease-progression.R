# Load libraries ---------------------------

library(ergm)

main_disease_progression <- function(estimation_net){
# This function computes the final probability, 
  # at each time step
  # for an agent to develop breast cancer at that time
  # and updates breast cancer statuses based on that time.
  
  #source("estimation.R")

# Load data ---------------------------

#load("/project/khanna7/abbyskwara/repos/bc-navigation/estimation_net.RData")
load("estimation_net.RData")
net0_bip <- estimation_net

# Check data ---------------------------

net0_bip
list.vertex.attributes(net0_bip)


# Compute individual risk of disease ---------------------------

  ## assign base risk
  pop_size <- network.size(net0_bip)

  age <- net0_bip %v% "age" #age
  bc_hpos_risk <- net0_bip %v% "bc_hpos_risk" #hormone+ risk 
  bc_hneg_risk <- net0_bip %v% "bc_hneg_risk" #hormone- risk
  bmi.ge.30 <- net0_bip %v% "bmi.ge.30"# obesity
  meno.status <- net0_bip %v% "meno.status" #menopausal status
  bc_status <- net0_bip %v% "bc_status" #breast cancer status
  subtype <- net0_bip %v% "subtype" #cancer subtype
  fd.rel <- net0_bip %v% "fd.rel" #first-degree relative
  disease.time <- net0_bip %v% "disease.time" #time of BC onset

  # update menopausal status based on age
     for (agent in 1:pop_size){
       if (age[agent] > 60)
         meno.status[agent] <- 1
     }
  
 
  
  net0_bip %v% "meno.status" <- meno.status

     # update menopausal status based on age
  #confused how this can't just be fixed with a >=
     meno <- which(age[agent] == 60)
     for (agent in meno){
       meno.status[agent] <- 1
     }
     
     net0_bip %v% "meno.status" <- meno.status

     
     # based on age, assign risk
     for (agent in 1:pop_size){
       # risk for hormone+ and - cancers is computed 
       # across three age categories: <60, 60-70, >70
       if (age[agent] < 60) {
         bc_hpos_risk[agent] <- bc.risk.50to60 * hpos_risk
         bc_hneg_risk[agent] <- bc.risk.50to60 * (1-hpos_risk)
       }
       else if (age[agent] >= 70){
         bc_hpos_risk[agent] <- bc.risk.70to75 * hpos_risk
         bc_hneg_risk[agent] <- bc.risk.70to75 * (1-hpos_risk)
       }
       else {
         bc_hpos_risk[agent] <- bc.risk.60to70 * hpos_risk
         bc_hneg_risk[agent] <- bc.risk.60to70 * (1-hpos_risk)
       }
     }
     
     #Based on having first degree relatives with BC, update risk 
     for (agent in 1:pop_size){
       if (fd.rel[agent] ==1){
         bc_hpos_risk[agent] <- bc_hpos_risk[agent] * fd.rel.risk
         bc_hneg_risk[agent] <- bc_hneg_risk[agent] * fd.rel.risk
       }
     }
    
     # based on obesity status, update risk
      for (agent in 1:pop_size){
        if(bmi.ge.30[agent] == 1){
          if (meno.status[agent] ==1){
            bc_hpos_risk[agent] <- bc_hpos_risk[agent] * obesity_hpos_postmp
            bc_hneg_risk[agent] <- bc_hneg_risk[agent] * obesity_hneg_postmp
          }
          else {
            bc_hpos_risk[agent] <- bc_hpos_risk[agent] * obesity_hpos_premp
            bc_hneg_risk[agent] <- bc_hneg_risk[agent] * obesity_hneg_premp
          }
        }
      }
  
     net0_bip %v% "bc_hpos_risk" <- bc_hpos_risk # we should compute this attribute at time 0
     net0_bip %v% "bc_hneg_risk" <- bc_hneg_risk
     
     cat("H-positive risk: ", 
         table(net0_bip %v% "bc_hpos_risk"), "\n")
     cat("H-negative risk: ",
         table(net0_bip %v% "bc_hneg_risk"), "\n")
     
     #Update time since development of disease
     
     for (agent in 1:pop_size){
       if (bc_status[agent] == 1){
         disease.time[agent] <- disease.time[agent] + 1 
         if (disease.time[agent] >= 12 & disease.time[agent] < 24) {
          symptom.severity[agent] <- 1
         } else if (disease.time[agent] >= 24 & disease.time[agent] < 36){
           symptom.severity[agent] <- 2
         } else if(disease.time[agent] > 36 ){
           symptom.severity[agent] <- 3
         }
        
       }
     }
     
     # based on risk, assign breast cancer status
     
     for (agent in 1:pop_size){
       if (bc_status[agent] == 0){
         bc_status_update <- rmultinom(1, 1, 
                  c(bc_hpos_risk[agent], bc_hneg_risk[agent], max(0, 1-(bc_hpos_risk[agent] + bc_hneg_risk[agent])))) #multinomial roll for hpos cancer, hneg cancer, and not hpos or hneg cancer
         if (bc_status_update[3] == 0) {    # if agent got breast cancer of either type
           bc_status[agent] <- 1
           subtype[agent] <- which(bc_status_update ==1) #1 = hpos, 2 = hneg
         }
       }
     }
     
     net0_bip %v% "bc_status" <- bc_status # assign new breast cancer status 
     net0_bip %v% "subtype" <- subtype   # assign breast cancer subtype for newly developed cases

     cat("New BC onsets: ", length(which(net0_bip %v% "bc_status" == 1)))
     cat("\n")
     cat("BC status: ", table(net0_bip %v% "bc_status"), "\n")
     cat("BC subtype: ", table(net0_bip %v% "subtype"), "\n")
     cat("\n")
     # simulate progression of disase
}   
