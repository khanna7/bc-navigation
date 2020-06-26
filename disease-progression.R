# Load libraries ---------------------------

library(ergm)

disease_progression <- function(net.f){
# This function computes the final probability, 
  # at each time step
  # for an agent to develop breast cancer at that time
  # and updates breast cancer statuses based on that time.

# Compute individual risk of disease ---------------------

  ## assign base risk
  pop_size <- 5000

  age <- net.f %v% "age" #age
  bc_hpos_risk <- net.f %v% "bc_hpos_risk" #hormone+ risk 
  bc_hneg_risk <- net.f %v% "bc_hneg_risk" #hormone- risk
  bmi.ge.30 <- net.f %v% "bmi.ge.30"# obesity
  meno.status <- net.f %v% "meno.status" #menopausal status
  bc_status <- net.f %v% "bc_status" #breast cancer status
  subtype <- net.f %v% "subtype" #cancer subtype
  fd.rel <- net.f %v% "fd.rel" #first-degree relative
  disease.time <- net.f %v% "disease.time" #time of BC onset
  symptom.severity <- net.f %v% "symptom.severity" #severity of breast cancer
  cancer_death <- net.f %v% "cancer_death" #whether or not the agent has died of breast cancer

  # update menopausal status based on age
     for (agent in 1:pop_size){
       if (age[agent] >= 60){
         meno.status[agent] <- 1
       }
     }
  
  net.f %v% "meno.status" <- meno.status
     
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
       if (fd.rel[agent]==1){
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

     net.f %v% "bc_hpos_risk" <- bc_hpos_risk
     net.f %v% "bc_hneg_risk" <- bc_hneg_risk
     
     cat("H-positive risk: ", table(net.f %v% "bc_hpos_risk"), "\n")
     cat("H-negative risk: ", table(net.f %v% "bc_hneg_risk"), "\n")
     
     #Update time since development of disease
   
     for (agent in 1:pop_size){
       if (bc_status[agent] == 1){
         disease.time[agent] <- disease.time[agent] + 1 
         if (disease.time[agent] >= 12 & disease.time[agent] < 24){
          symptom.severity[agent] <- 1
         } else if (disease.time[agent] >= 24 & disease.time[agent] < 36){
           symptom.severity[agent] <- 2
           if(cancer_death[agent]==0){cancer_death[agent] <- rbinom(1,1,stage_2_cancer_death)} #stage 2 cancer death
         } else if(disease.time[agent] >= 36){
           symptom.severity[agent] <- 3
           if(cancer_death[agent]==0){cancer_death[agent] <- rbinom(1,1,stage_3_cancer_death)} #stage 4 cancer death
         }
       }
     }
     
     # based on risk, assign breast cancer status
     
     for (agent in 1:pop_size){
       if (bc_status[agent] == 0){
         bc_status_update <- rmultinom(1, 1, 
                  c(bc_hpos_risk[agent],
                    bc_hneg_risk[agent],
                    max(0, 1-(bc_hpos_risk[agent] + bc_hneg_risk[agent]))))
         #multinomial roll for hpos cancer, hneg cancer, and not hpos or hneg cancer
         if (bc_status_update[3] == 0){    # if agent got breast cancer of either type
           bc_status[agent] <- 1
           subtype[agent] <- which(bc_status_update ==1) #1 = hpos, 2 = hneg
         }
       }
     }
     
     net.f %v% "disease.time" <- disease.time
     net.f %v% "symptom.severity"<-  symptom.severity
     net.f %v% "bc_status" <- bc_status # assign new breast cancer status 
     net.f %v% "subtype" <- subtype   # assign breast cancer subtype for newly developed cases
     net.f %v% "cancer_death"<-cancer_death

     cat("New BC onsets: ", length(which(net.f %v% "bc_status" == 1)))
     cat("\n")
     cat("BC status: ", table(net.f %v% "bc_status"), "\n")
     cat("BC subtype: ", table(net.f %v% "subtype"), "\n")
     cat("\n")
     # simulate progression of disase

return(net.f)
}   
