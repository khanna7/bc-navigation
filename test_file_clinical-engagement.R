#testing stuff.

rm(list=ls())
setwd("~/projects/bc-navigation")

library('testthat')
rm(list=ls())
library(network)
library(sna)
library(ergm)
library(networkDynamic)
library(tergm)

source("parameters.R")

test.disease.progression<-function(pop_size,
                                   age,
                                   fd.rel,
                                   bmi.ge.30,
                                   bc_status,
                                   disease.time,
                                   meno.status, 
                                   bc_hpos_risk,
                                   bc_hneg_risk,
                                   symptom.severity,
                                   cancer_death){
  
  all_agents<-which(net.f %v% "diagnosis" == 0)
  
  for (agent in all_agents){
    agent_data<-attrib_mtrx[agent,]
    #first: symptomatic agents without referrals roll for dt referrals
    if(diagnostic_referral[agent]==0 &
       screening_referral[agent]==0 &
       ss[agent]>0){
      diagnostic_referral[agent]<-rbinom(1,1,prob(agent_data,"dt"))
      diagnostic_referral_counter[agent]<-diagnostic_referral_counter[agent]+diagnostic_referral[agent]
    }
    
    #second: all agents without referrals roll for sm referrals
    if(diagnostic_referral[agent]==0 &
       screening_referral[agent]==0){
      screening_referral[agent]<-rbinom(1,1,prob(agent_data,"sm"))
      screening_referral_counter[agent]<-screening_referral_counter[agent]+screening_referral[agent]
    }
    
    #simulate navigation
    #else if((navigated[agent]==0) &
    #        (dt_complete[agent]==0) &
    #        (screen_complete[agent]==0) &
    #        (diagnostic_referral[agent]==1 |
    #         screening_referral[agent]==1) &
    #        (neighbor_navigated[agent]==1)
    #        ){
    #  navigated[agent]<-rbinom(1,1,0.514) #currently default value
    #}
  }
  
  result<-c(meno.status, 
            bc_hpos_risk,
            bc_hneg_risk,
            symptom.severity)
  
  return(result)
}

###################################~~~~~~TEST 1 of 5~~~~~~~#######################################

#synthetic network data

test_pop_size<-5
test_age<-c(50,59,60,61,74)
test_fd.rel<-c(1,0,1,0,1)
test_bmi.ge.30<-c(0,1,0,1,0)
test_bc_status<-c(1,1,1,1,0)
test_disease.time<-c(0,12,24,36,48)
test_meno.status<-c(0,0,0,0,0)
test_bc_hpos_risk<-c(0,0,0,0,0)
test_bc_hneg_risk<-c(0,0,0,0,0)
test_symptom.severity<-c(0,0,0,0,0)
test_cancer_death<-c(0,0,0,0,0)

test_vector<-test.disease.progression(test_pop_size,
                                      test_age,
                                      test_fd.rel,
                                      test_bmi.ge.30,
                                      test_bc_status,
                                      test_disease.time,
                                      test_meno.status, 
                                      test_bc_hpos_risk,
                                      test_bc_hneg_risk,
                                      test_symptom.severity,
                                      test_cancer_death)

result_vector<-c(0.000000e+00, 0.000000e+00,
                 1.000000e+00, 1.000000e+00,
                 1.000000e+00, 2.246796e-04,
                 1.176175e-04, 3.116832e-04,
                 2.907648e-04, 3.241123e-04,
                 6.711208e-05, 4.774417e-05,
                 9.310017e-05, 6.123367e-05,
                 9.681275e-05, 0.000000e+00,
                 1.000000e+00, 2.000000e+00,
                 3.000000e+00, 0.000000e+00)


test_that("disease_progression test 1", {
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
})

###################################~~~~~~TEST 2 of 5~~~~~~~#######################################

#synthetic network data

test_pop_size<-5
test_age<-c(50,59,60,61,74)
test_fd.rel<-c(1,1,1,0,1)
test_bmi.ge.30<-c(0,1,1,1,0)
test_bc_status<-c(1,1,0,1,0)
test_disease.time<-c(0,12,25,35,48)
test_meno.status<-c(0,0,0,0,0)
test_bc_hpos_risk<-c(0,0,0,0,0)
test_bc_hneg_risk<-c(0,0,0,0,0)
test_symptom.severity<-c(0,0,0,0,0)
test_cancer_death<-c(0,0,0,0,0)

test_vector<-test.disease.progression(test_pop_size,
                                      test_age,
                                      test_fd.rel,
                                      test_bmi.ge.30,
                                      test_bc_status,
                                      test_disease.time,
                                      test_meno.status, 
                                      test_bc_hpos_risk,
                                      test_bc_hneg_risk,
                                      test_symptom.severity,
                                      test_cancer_death)

result_vector<-c(0.000000e+00, 0.000000e+00,
                 1.000000e+00, 1.000000e+00,
                 1.000000e+00, 2.246796e-04,
                 1.752501e-04, 4.332396e-04,
                 2.907648e-04, 3.241123e-04,
                 6.711208e-05, 7.113881e-05,
                 9.123816e-05, 6.123367e-05,
                 9.681275e-05, 0.000000e+00,
                 1.000000e+00, 0.000000e+00,
                 3.000000e+00, 0.000000e+00)


test_that("disease_progression test 1", {
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
})

###################################~~~~~~TEST 3 of 5~~~~~~~#######################################

#synthetic network data

test_pop_size<-5
test_age<-c(50,40,61,61,70)
test_fd.rel<-c(1,0,0,0,1)
test_bmi.ge.30<-c(0,0,0,1,0)
test_bc_status<-c(1,0,1,1,0)
test_disease.time<-c(0,0,24,0,48)
test_meno.status<-c(0,0,0,0,0)
test_bc_hpos_risk<-c(0,0,0,0,0)
test_bc_hneg_risk<-c(0,0,0,0,0)
test_symptom.severity<-c(0,0,0,0,0)
test_cancer_death<-c(0,0,0,0,0)

test_vector<-test.disease.progression(test_pop_size,
                                      test_age,
                                      test_fd.rel,
                                      test_bmi.ge.30,
                                      test_bc_status,
                                      test_disease.time,
                                      test_meno.status, 
                                      test_bc_hpos_risk,
                                      test_bc_hneg_risk,
                                      test_symptom.severity,
                                      test_cancer_death)

result_vector<-c(0.000000e+00, 0.000000e+00,
                 1.000000e+00, 1.000000e+00,
                 1.000000e+00, 2.246796e-04,
                 1.507917e-04, 2.091833e-04,
                 2.907648e-04, 3.241123e-04,
                 6.711208e-05, 4.504167e-05,
                 6.248333e-05, 6.123367e-05,
                 9.681275e-05, 0.000000e+00,
                 0.000000e+00, 2.000000e+00,
                 0.000000e+00, 0.000000e+00)


test_that("disease_progression test 1", {
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
})
###################################~~~~~~TEST 4 of 5~~~~~~~#######################################
#synthetic network data

test_pop_size<-5
test_age<-c(54,55,61,70,74)
test_fd.rel<-c(1,0,1,1,1)
test_bmi.ge.30<-c(0,1,1,1,0)
test_bc_status<-c(1,0,0,0,0)
test_disease.time<-c(0,12,14,30,40)
test_meno.status<-c(0,0,0,0,0)
test_bc_hpos_risk<-c(0,0,0,0,0)
test_bc_hneg_risk<-c(0,0,0,0,0)
test_symptom.severity<-c(0,0,0,0,0)
test_cancer_death<-c(0,0,0,0,0)

test_vector<-test.disease.progression(test_pop_size,
                                      test_age,
                                      test_fd.rel,
                                      test_bmi.ge.30,
                                      test_bc_status,
                                      test_disease.time,
                                      test_meno.status, 
                                      test_bc_hpos_risk,
                                      test_bc_hneg_risk,
                                      test_symptom.severity,
                                      test_cancer_death)

result_vector<-c(0.000000e+00, 0.000000e+00,
                 1.000000e+00, 1.000000e+00,
                 1.000000e+00, 2.246796e-04,
                 1.176175e-04, 4.332396e-04,
                 4.505160e-04, 3.241123e-04,
                 6.711208e-05, 4.774417e-05,
                 9.123816e-05, 9.487650e-05,
                 9.681275e-05, 0.000000e+00,
                 0.000000e+00, 0.000000e+00,
                 0.000000e+00, 0.000000e+00)


test_that("disease_progression test 1", {
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
})
###################################~~~~~~TEST 5 of 5~~~~~~~#######################################
#synthetic network data

test_pop_size<-5
test_age<-c(50,50,60,60,70)
test_fd.rel<-c(1,0,1,0,0)
test_bmi.ge.30<-c(0,1,0,1,1)
test_bc_status<-c(1,1,1,1,1)
test_disease.time<-c(0,12,36,36,48)
test_meno.status<-c(0,0,0,0,0)
test_bc_hpos_risk<-c(0,0,0,0,0)
test_bc_hneg_risk<-c(0,0,0,0,0)
test_symptom.severity<-c(0,0,0,0,0)
test_cancer_death<-c(0,0,0,0,0)

test_vector<-test.disease.progression(test_pop_size,
                                      test_age,
                                      test_fd.rel,
                                      test_bmi.ge.30,
                                      test_bc_status,
                                      test_disease.time,
                                      test_meno.status, 
                                      test_bc_hpos_risk,
                                      test_bc_hneg_risk,
                                      test_symptom.severity,
                                      test_cancer_death)

result_vector<-c(0.000000e+00, 0.000000e+00,
                 1.000000e+00, 1.000000e+00,
                 1.000000e+00, 2.246796e-04,
                 1.176175e-04, 3.116832e-04,
                 2.907648e-04, 3.023598e-04,
                 6.711208e-05, 4.774417e-05,
                 9.310017e-05, 6.123367e-05,
                 6.367550e-05, 0.000000e+00,
                 1.000000e+00, 3.000000e+00,
                 3.000000e+00, 3.000000e+00)

test_that("disease_progression test 1", {
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
  expect_equal(result_vector,
               test_vector)
})

