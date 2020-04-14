# Parameters

# Demographic parameters ----

min.age <- 50
max.age <- 74
percent.alters.black <- 0.95
percent.alters.nonblack <- 1-percent.alters.black

# Network Parameters ----

n.ego <- 500 #non-derived
n.alter <- 4500 #non-derived
n <- 5000 #derived

ego.alter.deg.dis <- c(4,17,25,21,16,7,0)/100 #from the alter data  
ego.alter.deg <- 0:(length(ego.alter.deg.dis) - 1) #derived

ego.alter.deg.nodes <- n*ego.alter.deg.dis #derived

n.edges <- sum(ego.alter.deg.nodes*ego.alter.deg) #derived
mean.deg <- n.edges/(n*2) #derived

deg.min <- 0
deg.max <- 6

# Disease Risk Parameters ----

## base risk by age (from SEER reports -- 
## (data were given in 10-year risks, and need to be converted to appropriate units here)
bc.risk.50to60 <- 2.35/(100*10*12) #nonderived
bc.risk.60to70 <- 3.26/(100*10*12) #nonderived
bc.risk.70to75 <- 3.39/(100*10*12) #nonderived

## proportion hormone positive
prop.hormone.positive <- 77/100 #SEER data from Yami

## age at postmenopause
age.at.postmp <- 60 #ASSUMPTION

## bmi and disease risk (Munsell et al, 2014)
obesity_hpos_premp <- 0.78 #nonderived
obesity_hpos_postmp<- 1.39 #nonderived
obesity_hneg_premp <- 1.06 #nonderived
obesity_hneg_postmp <- 0.98 #nonderived

## obesity 
prop.bmi.ge.30 <- 0.50 #ASSUMPTION

## relative risk for obesity, pre and post menopausal (wang, 2016), not used 
#obesity_post <- 1.11 # rr of obesity in post menopausal women
#obesity_pre <- 0.99 # rr of obesity in pre menopausal women

## Cancer subtype distribution
hpos_risk <- .77    #77 percent of cancers among AA were hormone positive, Howlader 2014

## First degree relative history
prop.fd.rel <- .124   # 12.4% of women had a first degree relative with breast cancer, Braithwite
fd.rel.risk <- 1.49  # relative risk of women who had first degree relative with breast cancer 

## Screening parameters
prop.pcp.visitor <- .84

#All cause mortality rates by age, per time step 
ac.mort.50 <- .06146/12 
ac.mort.55 <- .09755/12
ac.mort.60 <- .13294/12
ac.mort.65 <- .17714/12
ac.mort.70 <- .25317/12