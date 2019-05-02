#########################################################
############    PSY 5210 Exam 2              ############    
############    Problem2                     ############    
############    Submitted by: Prateek Kumar  ############    
#########################################################

A2 <- read.csv("A2_1_MOPP.csv", header=T) #Reading the data from CSV
B1 <- read.csv("B1_2_BDU.csv", header=T)
C2 <- read.csv("C2_2_MOPP.csv", header=T)
D2 <- read.csv("D2_2_MOPP.csv", header=T)

dfA2 <- A2 %>% select(Time, ECG.HR, EDR.BR, Belt.BR, CoreTemp, Temp) #filtering the columns which are meaningful
dfB1 <- B1 %>% select(Time, ECG.HR, EDR.BR, Belt.BR, CoreTemp, Temp)
dfC2 <- C2 %>% select(Time, ECG.HR, EDR.BR, Belt.BR, CoreTemp, Temp)
dfD2 <- D2 %>% select(Time, ECG.HR, EDR.BR, Belt.BR, CoreTemp, Temp)

dfA2$CoreTemp[which(dfA2$CoreTemp==0)] = NA #Setting data value to NA which are out of the resonable range
dfB1$CoreTemp[which(dfB1$CoreTemp==0)] = NA
dfC2$CoreTemp[which(dfC2$CoreTemp==0)] = NA
dfD2$CoreTemp[which(dfD2$CoreTemp==0)] = NA

library(ggplot2)
dfA2$ECG.HR[which(dfA2$ECG.HR==0)] = NA #Checking if the data is out of resonable range and changing them to NA
dfB1$ECG.HR[which(dfA2$ECG.HR==0)] = NA
dfC2$ECG.HR[which(dfA2$ECG.HR==0)] = NA
dfD2$ECG.HR[which(dfA2$ECG.HR==0)] = NA

# A2_1_MOPP
p <- ggplot(data=dfA2, aes(x=as.numeric(Time),y=ECG.HR))
p+geom_line(color="orange",size=1)+geom_smooth(method = "loess",stat="smooth",size=1,span=0.02)+ theme_bw()+
  xlab("Time")+labs(title = "ECG.HR vs Time", subtitle = "Loess Regression Plot | R^2 = 0.405",
                    caption = "(based on data from A2_1_MOPP)")
lmodel <- loess(as.numeric(dfA2$Time)~dfA2$ECG.HR,span=0.02)
cor(as.numeric(dfA2$Time),predict(lmodel))^2 #R-squared value

# B1_2_BDU
p <- ggplot(data=dfB1, aes(x=as.numeric(Time),y=ECG.HR))
p+geom_line(color="orange",size=1)+geom_smooth(method = "loess",stat="smooth",size=2,span=0.008)+ theme_bw()+
  xlab("Time")+labs(title = "ECG.HR vs Time", subtitle = "Loess Regression Plot | R^2 = 0.345",
                    caption = "(based on data from B1_2_BDU)")
lmodel <- loess(as.numeric(dfB1$Time)~dfB1$ECG.HR,span=0.008)
cor(as.numeric(dfB1$Time),predict(lmodel))^2 #R-squared value

# C2_2_MOPP
p <- ggplot(data=dfC2, aes(x=as.numeric(Time),y=ECG.HR))
p+geom_line(color="orange",size=1)+geom_smooth(method = "loess",stat="smooth",size=2,span=0.008)+ theme_bw()+
  xlab("Time")+labs(title = "ECG.HR vs Time", subtitle = "Loess Regression Plot | R^2 = 0.338",
                    caption = "(based on data from C2_2_MOPP)")
lmodel <- loess(as.numeric(dfC2$Time)~dfC2$ECG.HR,span=0.008)
cor(as.numeric(dfC2$Time),predict(lmodel))^2 #R-squared value

# D2_2_MOPP
p <- ggplot(data=dfD2, aes(x=as.numeric(Time),y=ECG.HR))
p+geom_line(color="orange",size=1)+geom_smooth(method = "loess",stat="smooth",size=2,span=0.011)+ theme_bw()+
  xlab("Time")+labs(title = "ECG.HR vs Time", subtitle = "Loess Regression Plot | R^2 = 0.356",
                    caption = "(based on data from D2_2_MOPP)")
lmodel <- loess(as.numeric(dfD2$Time)~dfD2$ECG.HR,span=0.011)
cor(as.numeric(dfD2$Time),predict(lmodel))^2 #R-squared value