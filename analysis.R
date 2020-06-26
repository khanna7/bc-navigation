

#write.table(cbind(time,
#                  nintros, #deaths
#                  number.of.positive.bc.agents,
#                  4number.of.hpos.agents,
#                  number.of.hneg.agents,
#                  6number.of.diagnosed.cases,
#                  
#                  number.of.diagnostic.referrals,
#                  8number.of.screening.referrals,
#                  
#                  number.of.screen.completed,
#                  10number.of.dt.completed,
#                  
#                  number.of.symptomatic,
#                  12number.of.navigated.agents,
#
#                  time.with.cancer,
#                  14time.until.diagnosis),
#            file="bc.dem.burnin.fixed5000.for360.social_off.unipartite.data", ## to note total number of new infections
#            append=TRUE,
#            col.names=FALSE,
#            row.names=FALSE
#)

install.packages('ggplot2')
library('ggplot2')

analysis<-read.table("intervention.120.05262020.data")
median(analysis[,17]/(analysis[,17]+analysis[,18])) #.91
median(analysis[,17]+analysis[,18]) #115

analysis<-read.table("2intervention.120.05262020.data")
median(analysis[,17]/(analysis[,17]+analysis[,18])) #.93
median(analysis[,17]+analysis[,18]) #112

analysis_off<-read.table("burnin.360.05192020.data")
analysis_off<-analysis_off[361:720,]
analysis_social<-read.table("social.intervention.360.05192020.data")
analysis_institutional<-read.table("institutional.intervention.360.05192020.data")
analysis_institutional<-analysis_institutional[6:365,]
analysis_both<-read.table("full.intervention.360.05192020.data")
analysis_both<-analysis_both[361:720,]

analysis_high<-read.table("high.navigation.intervention.120.05192020.data")
analysis_null<-read.table("all.off.intervention.120.05192020.data")
analysis_current1<-read.table("current.navigation.intervention.120.05192020.data")
analysis_current2<-read.table("2current.navigation.intervention.120.05192020.data")
analysis_current3<-read.table("3current.navigation.intervention.120.05192020.data")

analysis_socialoff1<-read.table("1social.off.current.navigation.intervention.120.05192020.data")
analysis_socialoff2<-read.table("2ssocial.off.current.navigation.intervention.120.05192020.data")
analysis_socialoff3<-read.table("3social.off.current.navigation.intervention.120.05192020.data")

analysis_highsocial<-read.table("highsocial.navigation.intervention.120.05192020.data")

###Big Mega Analysis
data_compiled_14<-c()
data_compiled_15<-c()
data_compiled_16<-c()

for(i in 1:30){
  new_data<-read.table(paste0("data_2020-05-26/", i, ".data"))
  data_compiled_14<-append(data_compiled_14, median(new_data[,14][!is.na(new_data[,14])]))
  data_compiled_15<-append(data_compiled_15, median(new_data[,15][!is.na(new_data[,15])]))
  data_compiled_16<-append(data_compiled_16, median(new_data[,16][!is.na(new_data[,16])]))
}

data_compiled_14<-c()
data_compiled_15<-c()
data_compiled_16<-c()

for(i in 1:30){
  new_data<-read.table(paste0("data_2020-05-26/", i, ".data"))
  data_compiled_14<-append(data_compiled_14, mean(new_data[,14][!is.na(new_data[,14])]))
  data_compiled_15<-append(data_compiled_15, mean(new_data[,15][!is.na(new_data[,15])]))
  data_compiled_16<-append(data_compiled_16, mean(new_data[,16][!is.na(new_data[,16])]))
}

boxplot(data_compiled_14,data_compiled_15,data_compiled_16)

t.test(data_compiled_14,data_compiled_15)
t.test(data_compiled_15,data_compiled_16)

test_data_14<-analysis_null[,14]
test_data_15<-analysis_null[,15]
test_data_16<-analysis_null[,16]

t.test(test_data_14, data_compiled_14)

##06012


data_compiled_2<-c()
data_compiled_3<-c()
data_compiled_4<-c()
data_compiled_5<-c()
data_compiled_6<-c()

data_compiled_7<-c()
data_compiled_8<-c()
data_compiled_9<-c()
data_compiled_10<-c()

data_compiled_14<-c()
data_compiled_15<-c()
data_compiled_16<-c()
data_compiled_17<-c()
data_compiled_18<-c()

good_data_2<-c()
good_data_3<-c()
good_data_4<-c()
good_data_5<-c()
good_data_6<-c()
good_data_7<-c()
good_data_8<-c()
good_data_9<-c()
good_data_10<-c()
good_data_14<-c()
good_data_15<-c()
good_data_16<-c()
good_data_17<-c()
good_data_18<-c()

good_list<-list(good_data_2,
                good_data_3,
                good_data_4,
                good_data_5,
                good_data_6,
                good_data_7,
                good_data_8,
                good_data_9,
                good_data_10,
                good_data_14,
                good_data_15,
                good_data_16,
                good_data_17,
                good_data_18)

for(i in 1:30){
  new_data<-read.table(paste0("06_17_2020_Burnin_30_round1/data/", i, ".data")) 
  
  data_compiled_2<-append(data_compiled_2, mean(new_data[,2][!is.na(new_data[,2])]))
  data_compiled_3<-append(data_compiled_3, mean(new_data[,3][!is.na(new_data[,3])]))
  data_compiled_4<-append(data_compiled_4, mean(new_data[,4][!is.na(new_data[,4])]))
  data_compiled_5<-append(data_compiled_5, mean(new_data[,5][!is.na(new_data[,5])]))
  data_compiled_6<-append(data_compiled_6, mean(new_data[,6][!is.na(new_data[,6])]))
  data_compiled_7<-append(data_compiled_7, mean(new_data[,7][!is.na(new_data[,7])]))
  data_compiled_8<-append(data_compiled_8, mean(new_data[,8][!is.na(new_data[,8])]))
  data_compiled_9<-append(data_compiled_9, mean(new_data[,9][!is.na(new_data[,9])]))
  data_compiled_10<-append(data_compiled_10, mean(new_data[,10][!is.na(new_data[,10])]))
  
  
  data_compiled_14<-append(data_compiled_14, mean(new_data[,14][!is.na(new_data[,14])]))
  data_compiled_15<-append(data_compiled_15, mean(new_data[,15][!is.na(new_data[,15])]))
  data_compiled_16<-append(data_compiled_16, mean(new_data[,16][!is.na(new_data[,16])]))
  data_compiled_17<-append(data_compiled_17, mean(new_data[,17][!is.na(new_data[,17])]))
  data_compiled_18<-append(data_compiled_18, mean(new_data[,18][!is.na(new_data[,18])]))
}

for(i in 1:30){
  data_compiled_list<-list(data_compiled_2,
                           data_compiled_3,
                           data_compiled_4,
                           data_compiled_5,
                           data_compiled_6,
                           
                           data_compiled_7,
                           data_compiled_8,
                           data_compiled_9,
                           data_compiled_10,
                           
                           data_compiled_14,
                           data_compiled_16,
                           data_compiled_17,
                           data_compiled_18)
  
  for(j in 1:length(data_compiled_list)){
    cutoff<-IQR(data_compiled_list[[j]])
    mean_examined<-data_compiled_list[[j]][i]
    median_examined<-median(data_compiled_list[[j]])
    if(between(mean_examined,median_examined-cutoff,median_examined+cutoff)){
      #cat(i, "file", "\n")
      #cat(j, "variable", "\n")
      good_list[[j]]<-append(good_list[[j]],i)
    }
  }
}

Reduce(intersect,good_list)

count_vec<-1:30
for(j in 1:30){count_vec[j]<-0}

for(k in 1:30){
  for(i in 1:length(good_list)){
    if(is.element(k,good_list[[i]])){
      count_vec[k]<-count_vec[k]+1
    }
  }
}

boxplot(data_compiled_2, main= "deaths") #nintros
boxplot(data_compiled_3, main="positive agents") #positive agents
boxplot(data_compiled_4,main="hpos") #hpos
boxplot(data_compiled_5,main="hneg") #hneg
boxplot(data_compiled_6,main="diagnosed cases") #diagnosed casess
boxplot(data_compiled_7,main="dt referrals") #diag referrerals
boxplot(data_compiled_8,main="screen referrals") #screen referrals
boxplot(data_compiled_9,main="screen completions") #dt complete
boxplot(data_compiled_10, main = "dt completions") #screen complete
boxplot(data_compiled_17, main = "dt referrals at t")
boxplot(data_compiled_18, main = "screens at t")



analysis_high<-read.table("1highburnin3.120.06192020")
analysis_null<-read.table("1offburnin3.120.06192020")
analysis_current1<-read.table("1burnin3.120.06192020")
analysis_current2<-read.table("2burnin3.120.06192020")
analysis_current3<-read.table("3burnin3.120.06192020")
analysis_current3<-analysis_current3[121:240,]

boxplot(analysis_null[,14],
        analysis_null[,15],
        analysis_null[,16],
        analysis_null[,17])

boxplot(analysis_high[,14],
        analysis_high[,15],
        analysis_high[,16],
        analysis_high[,17])

boxplot(analysis_current1[,14],
        analysis_current1[,15],
        analysis_current1[,16],
        analysis_current1[,17])

boxplot(analysis_current2[,14],
        analysis_current2[,15],
        analysis_current2[,16],
        analysis_current2[,17])

boxplot(analysis_current3[,14],
        analysis_current3[,15],
        analysis_current3[,16],
        analysis_current3[,17])

analysis_current14<-append(analysis_current1[,14],analysis_current2[,14])
analysis_current14<-append(analysis_current14,analysis_current3[,14])

analysis_current15<-append(analysis_current1[,15],analysis_current2[,15])
analysis_current15<-append(analysis_current15,analysis_current3[,15])

analysis_current16<-append(analysis_current1[,16],analysis_current2[,16])
analysis_current16<-append(analysis_current16,analysis_current3[,16])

analysis_current17<-append(analysis_current1[,17],analysis_current2[,17])
analysis_current17<-append(analysis_current17,analysis_current3[,17])

boxplot(analysis_current14, analysis_current15, analysis_current16, analysis_current17)

boxplot(analysis_null[,14],
        
        analysis_high[,14],
        analysis_high[,15],
        analysis_high[,16],
        analysis_high[,17],
        analysis_current14,
        analysis_current15, 
        analysis_current16, 
        analysis_current17
        )


analysis_current20<-append(analysis_current1[,20],analysis_current2[,20])
analysis_current20<-append(analysis_current20,analysis_current3[,20])

analysis_current24<-append(analysis_current1[,24],analysis_current2[,24])
analysis_current24<-append(analysis_current24,analysis_current3[,24])

analysis_current27<-append(analysis_current1[,27],analysis_current2[,27])
analysis_current27<-append(analysis_current27,analysis_current3[,27])

analysis_current31<-append(analysis_current1[,31],analysis_current2[,31])
analysis_current31<-append(analysis_current31,analysis_current3[,31])

analysis_current6<-append(analysis_current1[,6],analysis_current2[,6])
analysis_current6<-append(analysis_current6,analysis_current3[,6])
#Create data
names <- c(rep("A. No Intervention", 1) , rep("B. Total Diagnosed", 1) , 
           rep("B. Navigated", 1), rep("B. Neighbor Nav", 1), rep("B. No Nav", 1), rep("C. Total Diagnosed", 1),
           rep("C. Navigated", 1), rep("C. Neighbor Nav", 1), rep("C. No Nav", 1))
value <- c(sum(analysis_null[,20])/sum(analysis_null[,6])*100,
           sum(analysis_high[,20])/sum(analysis_high[,6])*100,
           sum(analysis_high[,24])/sum(analysis_high[,6])*100,
           sum(analysis_high[,27])/sum(analysis_high[,6])*100,
           sum(analysis_high[,31])/sum(analysis_high[,6])*100,
           sum(analysis_current20)/sum(analysis_current6)*100,
           sum(analysis_current24)/sum(analysis_current6)*100, 
           sum(analysis_current27)/sum(analysis_current6)*100, 
           sum(analysis_current31)/sum(analysis_current6)*100)
data <- data.frame(names,value)


myColors <-c("green", "red","red","red","red", "blue","blue","blue","blue")

# Build the plot
barplot(data$value ~ data$names, 
        col=myColors , 
        ylab="Percentage of diagnoses at Symptom Severity 0 (%)" , xlab="types of interventions")

# Add a legend
legend("topleft", legend = c("No Intervention","5x Institutional Navigation Parameter", "Current Intervention Parameters") , 
       col = c("green", "red", "blue") ,
       bty = "n", pch=20 , pt.cex = 3, cex = 1)


analysis_current21<-append(analysis_current1[,21],analysis_current2[,21])
analysis_current21<-append(analysis_current21,analysis_current3[,21])

analysis_current25<-append(analysis_current1[,25],analysis_current2[,25])
analysis_current25<-append(analysis_current25,analysis_current3[,25])

analysis_current28<-append(analysis_current1[,28],analysis_current2[,28])
analysis_current28<-append(analysis_current28,analysis_current3[,28])

analysis_current32<-append(analysis_current1[,32],analysis_current2[,32])
analysis_current32<-append(analysis_current32,analysis_current3[,32])

analysis_current6<-append(analysis_current1[,6],analysis_current2[,6])
analysis_current6<-append(analysis_current6,analysis_current3[,6])
#Create data
names <- c(rep("A. No Intervention", 120) , rep("B. Total Diagnosed", 120) , 
           rep("B. Navigated", 120), rep("B. Neighbor Nav", 120), rep("B. No Nav", 120), rep("C. Total Diagnosed", 360),
           rep("C. Navigated", 360), rep("C. Neighbor Nav", 360), rep("C. No Nav", 360))
value <- c(analysis_null[,21]/analysis_null[,6]*100,
           analysis_high[,21]/analysis_high[,6]*100,
           analysis_high[,25]/analysis_high[,6]*100,
           analysis_high[,28]/analysis_high[,6]*100,
           analysis_high[,32]/analysis_high[,6]*100,
           analysis_current21/analysis_current6*100,
           analysis_current25/analysis_current6*100, 
           analysis_current28/analysis_current6*100, 
           analysis_current32/analysis_current6*100)
data <- data.frame(names,value)


myColors <-c("green", "red","red","red","red", "blue","blue","blue","blue")

# Build the plot
boxplot(data$value ~ data$names, 
        col=myColors , 
        ylab="percentage at Symptom Severity 1 (%)" , xlab="types of interventions")

# Add a legend
legend("topleft", legend = c("No Intervention","5x Institutional Navigation Parameter", "Current Intervention Parameters") , 
       col = c("green", "red", "blue") ,
       bty = "n", pch=20 , pt.cex = 3, cex = 1)


analysis_current22<-append(analysis_current1[,22],analysis_current2[,22])
analysis_current22<-append(analysis_current22,analysis_current3[,22])

analysis_current26<-append(analysis_current1[,26],analysis_current2[,26])
analysis_current26<-append(analysis_current26,analysis_current3[,26])

analysis_current29<-append(analysis_current1[,29],analysis_current2[,29])
analysis_current29<-append(analysis_current29,analysis_current3[,29])

analysis_current33<-append(analysis_current1[,33],analysis_current2[,33])
analysis_current33<-append(analysis_current33,analysis_current3[,33])

analysis_current6<-append(analysis_current1[,6],analysis_current2[,6])
analysis_current6<-append(analysis_current6,analysis_current3[,6])
#Create data
names <- c(rep("A. No Intervention", 120) , rep("B. Total Diagnosed", 120) , 
           rep("B. Navigated", 120), rep("B. Neighbor Nav", 120), rep("B. No Nav", 120), rep("C. Total Diagnosed", 360),
           rep("C. Navigated", 360), rep("C. Neighbor Nav", 360), rep("C. No Nav", 360))
value <- c(analysis_null[,22]/analysis_null[,6]*100,
           analysis_high[,22]/analysis_high[,6]*100,
           analysis_high[,26]/analysis_high[,6]*100,
           analysis_high[,29]/analysis_high[,6]*100,
           analysis_high[,33]/analysis_high[,6]*100,
           analysis_current22/analysis_current6*100,
           analysis_current26/analysis_current6*100, 
           analysis_current29/analysis_current6*100, 
           analysis_current33/analysis_current6*100)
data <- data.frame(names,value)


myColors <-c("green", "red","red","red","red", "blue","blue","blue","blue")

# Build the plot
boxplot(data$value ~ data$names, 
        col=myColors , 
        ylab="percentage at Symptom Severity 0 (%)" , xlab="types of interventions")

# Add a legend
legend("topleft", legend = c("No Intervention","5x Institutional Navigation Parameter", "Current Intervention Parameters") , 
       col = c("green", "red", "blue") ,
       bty = "n", pch=20 , pt.cex = 3, cex = 1)



text(x=4, y=54, "** small sample", pos=3, cex=1.2)

t.test(analysis_null[,14],analysis_current14)
t.test(analysis_null[,14],analysis_current15)
t.test(analysis_null[,14],analysis_current16)
t.test(analysis_null[,14],analysis_current17)











###



###

a1<-median(analysis_null[,14]) #24
a2<-median(analysis_null[,16]) #NA but will say 24
a3<-median(analysis_null[,16]) #24

b1<-median(analysis_current1[,14]) #18.5
b2<-median(analysis_current1[,15]) #19
b3<-median(analysis_current1[,16]) #17.5

c1<-median(analysis_current2[,14]) #19
c2<-median(analysis_current2[,15]) #19
c3<-median(analysis_current2[,16]) #17

d1<-median(analysis_current3[,14]) #20
d2<-median(analysis_current3[,15][-1]) #20
d3<-median(analysis_current3[,16]) #20

e1<-median(analysis_high[,14]) #13.25
e2<-median(analysis_high[,15]) #12.5
e3<-median(analysis_high[,16]) #25.25

f1<-median(analysis_socialoff1[,14]) #24
f2<-median(analysis_socialoff1[,15][-1]) #23
f3<-median(analysis_socialoff1[,16]) #20

g1<-median(analysis_socialoff2[,14]) #21
g2<-median(analysis_socialoff2[,15]) #22
g3<-median(analysis_socialoff2[,16]) #19.5

h1<-median(analysis_socialoff3[,14]) #22
h2<-median(analysis_socialoff3[,15]) #22.5
h3<-median(analysis_socialoff3[,16]) #21.5

i1<-median(analysis_highsocial[,14]) #16.75
i2<-median(analysis_highsocial[,15]) #17
i3<-median(analysis_highsocial[,16]) #17


library(ggplot2)
MDT_trials2 <- read.table(
  header=TRUE, text='Category Trial MDT
1  All_diagnosed  AllOff      24
2  Navigated_Dx  AllOff      24
3  Unnavigated_Dx AllOff      24
4  All_diagnosed  Test1      24
5  Navigated_Dx Test1      23
6  Unnavigated_Dx Test1      20
7  All_diagnosed  Test2      21
8  Navigated_Dx   Test2      22
9  Unnavigated_Dx Test2      19.5
10 All_diagnosed Test3     22
11 Navigated_Dx Test3     22.5
12 Unnavigated_Dx Test3     21.5
13 All_diagnosed Indivx5     13.25
14 Navigated_Dx  Indivx5     12.5
15 Unnavigated_Dx Indivx5     25.25
16 All_diagnosed  Socialx5 16.75
17 Navigated_Dx   Socialx5 17 
18 Unnavigated_Dx Socialx5 17')
ggplot(MDT_trials2 , aes(factor(Trial), MDT, fill = Category)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Comparison with current parameters without social components to no intervention and 5x individual navigation")

MDT_trials <- read.table(
  header=TRUE, text='Category Trial MDT
1  All_diagnosed  AllOff      24
2  Navigated_Dx  AllOff      24
3  Unnavigated_Dx AllOff      24
4  All_diagnosed  Test1      18.5
5  Navigated_Dx Test1      19
6  Unnavigated_Dx Test1      17.5
7  All_diagnosed  Test2      19
8  Navigated_Dx   Test2      19
9  Unnavigated_Dx Test2      17
10 All_diagnosed Test3     20
11 Navigated_Dx Test3     20
12 Unnavigated_Dx Test3     20
13 All_diagnosed Indivx5     13.25
14 Navigated_Dx  Indivx5     12.5
15 Unnavigated_Dx Indivx5     25.25
16 All_diagnosed  Socialx5 16.75
17 Navigated_Dx   Socialx5 17 
18 Unnavigated_Dx Socialx5 17')

ggplot(MDT_trials , aes(factor(Trial), MDT, fill = Category)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Comparison with current parameters to no intervention and 5x individual navigation")



#comparing diagnosis time w.no intervention, social, institutional, both.

x<-analysis_off[,1]
y1<-analysis_off[,14]
y2<-analysis_social[,14]
y3<-analysis_institutional[,14]
z1<-analysis_off[,13]
z2<-analysis_social[,13]
z3<-analysis_institutional[,13]
y4<-analysis_both[,14]

naindices<-which(is.na(analysis_off[,14]))
y1[naindices]<-0

naindices<-which(is.na(analysis_social[,14]))
y2[naindices]<-0

naindices<-which(is.na(analysis_institutional[,14]))
y3[naindices]<-0

naindices<-which(is.na(analysis_both[,14]))
y4[naindices]<-0

# first plot
plot(x, y1, ylim=range(c(0,80)), col="dark blue",lwd=2, pch=1)

# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(x, y2, ylim=range(c(0,80)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of months",
     main = "mean diagnosis time (MDT)",col="blue",lwd=2, pch=1)
par(new = TRUE)
plot(x, y3, ylim=range(c(0,80)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of months",
     main = "mean diagnosis time (MDT)",col="dark red",lwd=2, pch=1)
par(new = TRUE)
plot(x, z1, ylim=range(c(0,80)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of months",
     main = "mean diagnosis time (MDT)",col="green",lwd=2, pch=1)
par(new = TRUE)
plot(x, z2, ylim=range(c(0,80)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of months",
     main = "mean diagnosis time (MDT)",col="dark green",lwd=2, pch=1)
par(new = TRUE)
plot(x, z3, ylim=range(c(0,80)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of months",
     main = "mean  diagnosis time (MDT)",col="light green",lwd=2, pch=1)
par(new = TRUE)
plot(x, y4, ylim=range(c(y1,y3)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of months",
     main = "disease and diagnosis time",col="red",lwd=2, pch=7)
legend("bottomright",legend=c("MDT w/o nav", "MDT w/social nav", "MDT w/inst. nav ", "MDT w/ both"),
       col=c("dark blue", "blue","dark red", "red"), lty=1, cex=0.52, lwd=5)

abline(h=12, col="purple")
abline(h=24, col="green")
abline(h=36, col="orange")
#




#hpos hneg  plots

x<-analysis[,1]
y1<-analysis[,4]
y2<-analysis[,5]

# first plot
plot(x, y1, ylim=range(c(y1,y2)), col="red")

# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(x, y2, ylim=range(c(y1,y2)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of cases",
     main = "hpos and hneg bc cases",col="blue")
legend("topright",legend=c("hpos", "hneg"),
       col=c("red", "blue"), lty=1)



#positives and diagnosed plots

x<-analysis[,1]
y1<-analysis[,3]
y2<-analysis[,6]

# first plot
plot(x, y1, ylim=range(c(y1,y2)), col="red")

# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(x, y2, ylim=range(c(y1,y2)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of cases",
     main = "positive and diagnosed bc cases",col="blue")
legend("topright",legend=c("positives", "diagnosed"),
       col=c("red", "blue"), lty=1)

#screens and dxt referrals.

x<-analysis_off[,1]
y1<-analysis_off[,17]
y2<-analysis_off[,18]

# first plot
plot(x, y1, ylim=range(c(y1,y2)), col="red")

# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(x, y2, ylim=range(c(y1,y2)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of referrals",
     main = "dx test referrals and screen completions",col="blue")
legend("right",legend=c("dx test ref", "screen completion"),
       col=c("red", "blue"), lty=1)

# disease time and diagnosis time.

analysis<-read.table("2bc.dem.burnin.fixed5000.for360.social_off.unipartite.data")

analysis<-analysis[721:1080,]

x<-analysis[,1]
y1<-analysis[,13]
y2<-analysis[,14]
naindices<-which(is.na(analysis[,14]))
y2[naindices]<-0
analysis<-read.table("2bc.dem.burnin.fixed5000.for360.social_off.unipartite.data")
analysis<-analysis[1:360,]

y3<-analysis[,13]
y4<-analysis[,14]

naindices<-which(is.na(analysis[,13]))
y3[naindices]<-0
naindices<-which(is.na(analysis[,14]))
y4[naindices]<-0



# first plot
plot(x, y1, ylim=range(c(y1,y4)), col="dark blue",lwd=2, pch=7)

# second plot  EDIT: needs to have same ylim
par(new = TRUE)
plot(x, y2, ylim=range(c(y1,y4)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of months",
     main = "disease and diagnosis time",col="blue",lwd=2, pch=7)
par(new = TRUE)
plot(x, y3, ylim=range(c(y1,y4)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of months",
     main = "disease and diagnosis time",col="dark red",lwd=2, pch=7)
par(new = TRUE)
plot(x, y4, ylim=range(c(y1,y4)), 
     axes = FALSE, xlab = "timesteps", ylab = "number of months",
     main = "disease and diagnosis time",col="red",lwd=2, pch=7)
legend("bottomright",legend=c("disease time w/nav", "diagnosis time w/nav", "disease time w/o nav ", "diagnosis time w/o nav"),
       col=c("dark blue", "blue","dark red","red"), lty=1, cex=0.52, lwd=5)

abline(h=12, col="purple")
abline(h=24, col="green")
abline(h=36, col="orange")

#




















#old analysis

alive_agents<-analysis[,3]

plot(analysis[,1],analysis[,5]/analysis[,3]) #bc rate graph
plot(analysis[,1],analysis[,7]/analysis[,3]) #navigation rate graph

decay<-c()
for (i in 2:length(alive_agents)){
  decay<-append(decay,(alive_agents[i]-alive_agents[i-1])/alive_agents[i])
}

decay<-append(0,decay)

#1 goal: achieve a 0 slope with prevalence of breast cancer growth as population matures. 
#What number does that fix at?

#Can also look at navigation diagnoses vs non navigation diagnoses
#adherence/navigation play.

bc_agents<-analysis[,5]

alive_cancer_vec<-c()
for(i in 1:360){
  bc_vec<-get.vertex.attribute.active(x=net.f, prefix="bc_status", at=1)
  alive_cancer_vec<-append(alive_cancer_vec,
                           length(
                               which(
                                 is.active(net.f,
                                           v = which(bc_vec==1), 
                                           at=i)==TRUE)))
}



neighbors<-c()
for (i in alive_agents){
  neighbors<-append(neighbors, length(get.neighborhood(net0_bip,i)))
}

neighbors[3701:length(neighbors)]