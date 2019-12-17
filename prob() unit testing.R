library(curl)
library(network)
library(ergm)
library(statnet)
library(stringr)
library(devtools)
library(testthat)

prob<-function(agent,test,ref_vs_test){
  
  time_component<-5 #p_t
  
  if(test=="sm"){test_component<-10} #p_sm
  else if(test=="dt"){test_component<-20} #p_dx
  
  navigated_component<-100 #p_nav
  antinavigated_component<-1000 #p_antinav
  neighbornav_component<-10000 #p_nnav
  neighborantinav_component<-100000 #p_nantinav
  
  if(ref_vs_test=="ref"){
    ref_vs_test_component<-1000000
    test_component<-ss[agent]*test_component} #ss*p_sm or ss*p_dx
  else if(ref_vs_test=="test"){
    time_component<-5 #setting p_t to 0
    if(test=="dt"){ref_vs_test_component<-2000000} #p_dtcomp
    if(test=="sm"){ref_vs_test_component<-3000000} #p_smcomp
  }
  
  symptom_severity_component<-10000000 #p_ss
  regular_pcp_visitor_component<-100000000 #p_rpcp
  
  return(time_component #p_t or 0
         +test_component #p_dx or p_sm
         +navigated_component*navigated[agent] #p_nav
         +antinavigated_component*antinavigated[agent] #p_antinav
         +neighbornav_component*neighbornav[agent] #p_nnav
         +neighborantinav_component*neighborfp[agent] #p_nantinav
         +ref_vs_test_component #0 or p_dtcomp or p_smcomp
         +symptom_severity_component*ss[agent] #p_ss*ss
         +regular_pcp_visitor_component) #p_rpcp
}

#Testing below:
agent<-1
ss<-c(3,2,1,0)
navigated<-c(1,1,0,0)
antinavigated<-c(1,0,1,0)
neighbornav<-c(1,1,1,0)
neighborfp<-c(1,0,0,0)


test_that("prob unit testing", {
  expect_equal(prob(agent,"dt","ref"), 131111165)
  expect_equal(prob(agent,"sm","ref"), 131111135)
  expect_equal(prob(agent,"dt","test"), 132111125)
  expect_equal(prob(agent,"sm","test"), 133111115)
  })

#excuse me sir i am  losing it.

#  expect_equal(prob(agent,"dt","ref"), 2)
#  expect_equal(prob(agent,"dt","ref"), 3)
#  expect_equal(prob(agent,"dt","ref"), 4)
#  expect_equal(prob(agent,"dt","ref"), 5)
#  })

