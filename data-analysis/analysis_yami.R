analysis_high<-read.table("11highburnin3.120.06192020")
analysis_null<-read.table("11offburnin3.120.06192020")
analysis_current1<-read.table("11burnin3.120.06192020")
analysis_current2<-read.table("22burnin3.120.06192020")
analysis_current3<-read.table("33burnin3.120.06192020")



analysis_current20<-c()
analysis_current24<-c()
analysis_current28<-c()
analysis_current32<-c()

analysis_current21<-c()
analysis_current25<-c()
analysis_current29<-c()
analysis_current33<-c()

analysis_current22<-c()
analysis_current26<-c()
analysis_current30<-c()
analysis_current34<-c()

analysis_current23<-c()
analysis_current27<-c()
analysis_current31<-c()
analysis_current35<-c()

analysis_current6<-c()


for(i in 1:30){
        new_data<-read.table(paste0("06_22_2020_Intervention30/data/", i, ".data"))
        analysis_current20<-append(analysis_current20, new_data[,20][!is.na(new_data[,20])])
        analysis_current24<-append(analysis_current24, new_data[,24][!is.na(new_data[,24])])
        analysis_current28<-append(analysis_current28, new_data[,28][!is.na(new_data[,28])])
        analysis_current32<-append(analysis_current32, new_data[,32][!is.na(new_data[,32])])
        analysis_current6 <-append(analysis_current6, new_data[,6][!is.na(new_data[,6])])
        
        analysis_current21<-append(analysis_current21, new_data[,21][!is.na(new_data[,21])])
        analysis_current25<-append(analysis_current25, new_data[,25][!is.na(new_data[,25])])
        analysis_current29<-append(analysis_current29, new_data[,29][!is.na(new_data[,29])])
        analysis_current33<-append(analysis_current33, new_data[,33][!is.na(new_data[,33])])
       
        
        analysis_current22<-append(analysis_current22, new_data[,22][!is.na(new_data[,21])])
        analysis_current26<-append(analysis_current26, new_data[,26][!is.na(new_data[,26])])
        analysis_current30<-append(analysis_current30, new_data[,30][!is.na(new_data[,30])])
        analysis_current34<-append(analysis_current34, new_data[,34][!is.na(new_data[,34])])
        
        analysis_current23<-append(analysis_current23, new_data[,23][!is.na(new_data[,23])])
        analysis_current27<-append(analysis_current27, new_data[,27][!is.na(new_data[,27])])
        analysis_current31<-append(analysis_current31, new_data[,31][!is.na(new_data[,31])])
        analysis_current35<-append(analysis_current35, new_data[,35][!is.na(new_data[,35])])
}

analysis_current20<-append(analysis_current1[,20],analysis_current2[,20])
analysis_current20<-append(analysis_current20,analysis_current3[,20])

analysis_current24<-append(analysis_current1[,24],analysis_current2[,24])
analysis_current24<-append(analysis_current24,analysis_current3[,24])

analysis_current28<-append(analysis_current1[,28],analysis_current2[,28])
analysis_current28<-append(analysis_current28,analysis_current3[,28])

analysis_current32<-append(analysis_current1[,32],analysis_current2[,32])
analysis_current32<-append(analysis_current32,analysis_current3[,32])

analysis_current6<-append(analysis_current1[,6],analysis_current2[,6])
analysis_current6<-append(analysis_current6,analysis_current3[,6])
#Create data
#SS0
names <- c(rep("A. No Intervention", 1) , rep("B. Total Diagnosed", 1) , 
           rep("B. Navigated", 1), rep("B. Neighbor Nav", 1), rep("B. No Nav", 1), rep("C. Total Diagnosed", 1),
           rep("C. Navigated", 1), rep("C. Neighbor Nav", 1), rep("C. No Nav", 1))
value <- c(sum(analysis_null[,20])/sum(analysis_null[,6])*100,
           sum(analysis_high[,20])/sum(analysis_high[,6])*100,
           sum(analysis_high[,24])/sum(analysis_high[,6])*100,
           sum(analysis_high[,28])/sum(analysis_high[,6])*100,
           sum(analysis_high[,32])/sum(analysis_high[,6])*100,
           sum(analysis_current20)/sum(analysis_current6)*100,
           sum(analysis_current24)/sum(analysis_current6)*100, 
           sum(analysis_current28)/sum(analysis_current6)*100, 
           sum(analysis_current32)/sum(analysis_current6)*100)
data <- data.frame(names,value)


myColors <-c("green", "red","red","red","red", "blue","blue","blue","blue")

# Build the plot
barplot(data$value ~ data$names, 
        col=myColors , 
        ylab="Percentage of diagnoses at Symptom Severity 0 (%)" , xlab="types of interventions")

# Add a legend
legend(x=6, y=10, legend = c("No Intervention","5x Institutional Navigation Parameter", "Current Intervention Parameters") , 
       col = c("green", "red", "blue") ,
       bty = "n", pch=20 , pt.cex = 3, cex = 1)



#SS1:

analysis_current21<-append(analysis_current1[,21],analysis_current2[,21])
analysis_current21<-append(analysis_current21,analysis_current3[,21])

analysis_current25<-append(analysis_current1[,25],analysis_current2[,25])
analysis_current25<-append(analysis_current25,analysis_current3[,25])

analysis_current29<-append(analysis_current1[,29],analysis_current2[,29])
analysis_current29<-append(analysis_current29,analysis_current3[,29])

analysis_current33<-append(analysis_current1[,33],analysis_current2[,33])
analysis_current33<-append(analysis_current33,analysis_current3[,33])

analysis_current6<-append(analysis_current1[,6],analysis_current2[,6])
analysis_current6<-append(analysis_current6,analysis_current3[,6])
#Create data
names <- c(rep("A. No Intervention", 1) , rep("B. Total Diagnosed", 1) , 
           rep("B. Navigated", 1), rep("B. Neighbor Nav", 1), rep("B. No Nav", 1), rep("C. Total Diagnosed", 1),
           rep("C. Navigated", 1), rep("C. Neighbor Nav", 1), rep("C. No Nav", 1))
value <- c(sum(analysis_null[,21])/sum(analysis_null[,6])*100,
           sum(analysis_high[,21])/sum(analysis_high[,6])*100,
           sum(analysis_high[,25])/sum(analysis_high[,6])*100,
           sum(analysis_high[,29])/sum(analysis_high[,6])*100,
           sum(analysis_high[,33])/sum(analysis_high[,6])*100,
           sum(analysis_current21)/sum(analysis_current6)*100,
           sum(analysis_current25)/sum(analysis_current6)*100, 
           sum(analysis_current29)/sum(analysis_current6)*100, 
           sum(analysis_current33)/sum(analysis_current6)*100)
data <- data.frame(names,value)


myColors <-c("green", "red","red","red","red", "blue","blue","blue","blue")

# Build the plot
barplot(data$value ~ data$names, 
        col=myColors , 
        ylab="Percentage of diagnoses at Symptom Severity 1 (%)" , xlab="types of interventions")

# Add a legend
legend(x=6, y=22, legend = c("No Intervention","5x Institutional Navigation Parameter", "Current Intervention Parameters") , 
       col = c("green", "red", "blue") ,
       bty = "n", pch=20 , pt.cex = 3, cex = 1)


#SS2:

analysis_current22<-append(analysis_current1[,22],analysis_current2[,22])
analysis_current22<-append(analysis_current22,analysis_current3[,22])

analysis_current26<-append(analysis_current1[,26],analysis_current2[,26])
analysis_current26<-append(analysis_current26,analysis_current3[,26])

analysis_current30<-append(analysis_current1[,30],analysis_current2[,30])
analysis_current30<-append(analysis_current30,analysis_current3[,30])

analysis_current34<-append(analysis_current1[,34],analysis_current2[,34])
analysis_current34<-append(analysis_current33,analysis_current3[,34])

analysis_current6<-append(analysis_current1[,6],analysis_current2[,6])
analysis_current6<-append(analysis_current6,analysis_current3[,6])
#Create data
names <- c(rep("A. No Intervention", 1) , rep("B. Total Diagnosed", 1) , 
           rep("B. Navigated", 1), rep("B. Neighbor Nav", 1), rep("B. No Nav", 1), rep("C. Total Diagnosed", 1),
           rep("C. Navigated", 1), rep("C. Neighbor Nav", 1), rep("C. No Nav", 1))
value <- c(sum(analysis_null[,22])/sum(analysis_null[,6])*100,
           sum(analysis_high[,22])/sum(analysis_high[,6])*100,
           sum(analysis_high[,26])/sum(analysis_high[,6])*100,
           sum(analysis_high[,30])/sum(analysis_high[,6])*100,
           sum(analysis_high[,34])/sum(analysis_high[,6])*100,
           sum(analysis_current22)/sum(analysis_current6)*100,
           sum(analysis_current26)/sum(analysis_current6)*100, 
           sum(analysis_current30)/sum(analysis_current6)*100, 
           sum(analysis_current34)/sum(analysis_current6)*100)
data <- data.frame(names,value)


myColors <-c("green", "red","red","red","red", "blue","blue","blue","blue")

# Build the plot
barplot(data$value ~ data$names, 
        col=myColors , 
        ylab="Percentage of diagnoses at Symptom Severity 2 (%)" , xlab="types of interventions")

# Add a legend
legend(x=7, y=22, legend = c("No Intervention","5x Institutional Navigation Parameter", "Current Intervention Parameters") , 
       col = c("green", "red", "blue") ,
       bty = "n", pch=20 , pt.cex = 3, cex = 1)


#SS3:

analysis_current23<-append(analysis_current1[,23],analysis_current2[,23])
analysis_current23<-append(analysis_current23,analysis_current3[,23])

analysis_current27<-append(analysis_current1[,27],analysis_current2[,27])
analysis_current27<-append(analysis_current26,analysis_current3[,27])

analysis_current31<-append(analysis_current1[,31],analysis_current2[,31])
analysis_current31<-append(analysis_current31,analysis_current3[,31])

analysis_current35<-append(analysis_current1[,35],analysis_current2[,35])
analysis_current35<-append(analysis_current33,analysis_current3[,35])

analysis_current6<-append(analysis_current1[,6],analysis_current2[,6])
analysis_current6<-append(analysis_current6,analysis_current3[,6])
#Create data
names <- c(rep("A. No Intervention", 1) , rep("B. Total Diagnosed", 1) , 
           rep("B. Navigated", 1), rep("B. Neighbor Nav", 1), rep("B. No Nav", 1), rep("C. Total Diagnosed", 1),
           rep("C. Navigated", 1), rep("C. Neighbor Nav", 1), rep("C. No Nav", 1))
value <- c(sum(analysis_null[,23])/sum(analysis_null[,6])*100,
           sum(analysis_high[,23])/sum(analysis_high[,6])*100,
           sum(analysis_high[,27])/sum(analysis_high[,6])*100,
           sum(analysis_high[,31])/sum(analysis_high[,6])*100,
           sum(analysis_high[,35])/sum(analysis_high[,6])*100,
           sum(analysis_current23)/sum(analysis_current6)*100,
           sum(analysis_current27)/sum(analysis_current6)*100, 
           sum(analysis_current31)/sum(analysis_current6)*100, 
           sum(analysis_current35)/sum(analysis_current6)*100)
data <- data.frame(names,value)


myColors <-c("green", "red","red","red","red", "blue","blue","blue","blue")

# Build the plot
barplot(data$value ~ data$names, 
        col=myColors , 
        ylab="Percentage of diagnoses at Symptom Severity 3 (%)" , xlab="types of interventions")

# Add a legend
legend(x=2, y=60, legend = c("No Intervention","5x Institutional Navigation Parameter", "Current Intervention Parameters") , 
       col = c("green", "red", "blue") ,
       bty = "n", pch=20 , pt.cex = 3, cex = 1)





