library(sna)

rm(list=ls())
load("3.RData") #burnin network

ls()

net.f.burnin <- net.f
identical(net.f, net.f.burnin)

palette(c("black", "steelblue1", "darkolivegreen", "darkorange2",
"gray60", "darkslateblue"))

pdf(file="burnin-network.pdf")
gplot(net.f.burnin, gmode="graph", displayisolates=FALSE,
      usearrows=FALSE, 
      #vertex.cex=0.75,
      edge.col = "gray",
      vertex.cex = 1.25*c(net.f.burnin %v% "symptom.severity"),
      vertex.col = "steelblue1")
dev.off()

load("07_02_2020_intervention30/RData/1.RData")
net.f.intervention <- net.f

list.vertex.attributes(net.f.intervention)

table(net.f.intervention %v% "dt_complete", 
      exclude = NULL)

table(net.f.intervention %v% "diagnostic_test_complete_complete", 
      exclude = NULL)

table(net.f.intervention %v% "diagnostic_test_complete", # in resetting the diagnostic_test_complete vertex attribute
      exclude = NULL)

table(net.f.intervention %v% "screen_complete", 
      exclude = NULL)

table(net.f.intervention %v% "screening_referral", 
      exclude = NULL)

table(net.f.intervention %v% "diagnostic_referral", 
      exclude = NULL)
