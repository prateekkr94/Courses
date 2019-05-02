#########################################################
############    PSY 5210 Exam 2              ############    
############    Problem1                     ############    
############    Submitted by: Prateek Kumar  ############    
#########################################################


A2 <- read.csv("A2_1_MOPP.csv", header=T) #Reading the 4 CSV files
B1 <- read.csv("B1_2_BDU.csv", header=T)
C2 <- read.csv("C2_2_MOPP.csv", header=T)
D2 <- read.csv("D2_2_MOPP.csv", header=T)

library(dplyr)

dfA2 <- A2 %>% select(Time, ECG.HR, EDR.BR, Belt.BR, CoreTemp, Temp) #filtering the columns which are meaningful
dfB1 <- B1 %>% select(Time, ECG.HR, EDR.BR, Belt.BR, CoreTemp, Temp)
dfC2 <- C2 %>% select(Time, ECG.HR, EDR.BR, Belt.BR, CoreTemp, Temp)
dfD2 <- D2 %>% select(Time, ECG.HR, EDR.BR, Belt.BR, CoreTemp, Temp)

sapply(dfA2, class) #checking the datatype and summary of each dataset
summary(dfA2)
sapply(dfB1, class)
summary(dfB1)
sapply(dfC2, class)
summary(dfC2)
sapply(dfD2, class)
summary(dfD2)


colSums(is.na(dfA2)) #Checking the missing values if any
colSums(is.na(dfB1))
colSums(is.na(dfC2))
colSums(is.na(dfD2))

# ECG.HR average 60-100 per minute
# CoreTemp average 36.3-37.3 C
# Temp average 36.5-37.5 C

dfA2$CoreTemp[which(dfA2$CoreTemp==0)] = NA #Setting data value to NA which are out of the resonable range
dfB1$CoreTemp[which(dfB1$CoreTemp==0)] = NA
dfC2$CoreTemp[which(dfC2$CoreTemp==0)] = NA
dfD2$CoreTemp[which(dfD2$CoreTemp==0)] = NA


require(lattice)

# A2_1_MOPP
xplot <- rep((0:(length(dfA2$Time)-1))/1000,5) #x and y plot
yplot <- c(dfA2$ECG.HR, dfA2$EDR.BR, dfA2$Belt.BR, dfA2$CoreTemp, dfA2$Temp)

mean(dfA2$ECG.HR) #calculating the mean, min, max and median values for each attribute
min(dfA2$ECG.HR)
max(dfA2$ECG.HR)
median(dfA2$ECG.HR)

mean(dfA2$EDR.BR)
min(dfA2$EDR.BR)
max(dfA2$EDR.BR)
median(dfA2$EDR.BR)

mean(dfA2$Belt.BR)
min(dfA2$Belt.BR)
max(dfA2$Belt.BR)
median(dfA2$Belt.BR)

mean(dfA2$CoreTemp, na.rm = T)
min(dfA2$CoreTemp, na.rm = T)
max(dfA2$CoreTemp, na.rm = T)
median(dfA2$CoreTemp, na.rm = T)

mean(dfA2$Temp)
min(dfA2$Temp)
max(dfA2$Temp)
median(dfA2$Temp)

gplot <- c(rep("ECG.HR                            Mean: 93.11 | Median: 89.3 | Min: 14.6 | Max: 144.7",length(dfA2$ECG.HR)),
           rep("EDR.BR                            Mean: 28.76 | Median: 26.9 | Min: 21.1 | Max: 41.60",length(dfA2$EDR.BR)),
           rep("Belt.BR                           Mean: 27.50 | Median: 26.1 | Min: 17.0 | Max: 41.50",length(dfA2$Belt.BR)),
           rep("CoreTemp                          Mean: 37.51 | Median: 37.5 | Min: 37.4 | Max: 37.68",length(dfA2$CoreTemp)),
           rep("Temp                              Mean: 33.34 | Median: 33.2 | Min: 31.3 | Max: 35.70",length(dfA2$Temp)))

#plotting the attributes w.r.t. time
tp <- xyplot(yplot~xplot|gplot, type="l", layout=c(1,5), 
             xlab="Time", ylab="Attributes", grid=T, lwd=2, main = "A2_1_MOPP", 
             fill = "blue",
             scales=list(x=list(draw=FALSE), 
                         y=list(relation='free',limits=list(c(17,41.6), c(37.42,37.69), 
                                                            c(14.5,144.8), c(21,41.7), c(31.2,35.8)))))
tp

# B1_2_BDU
xplot <- rep((0:(length(dfB1$Time)-1))/1000,5) #x and y plot
yplot <- c(dfB1$ECG.HR, dfB1$EDR.BR, dfB1$Belt.BR, dfB1$CoreTemp, dfB1$Temp)

mean(dfB1$ECG.HR) #calculating the mean, min, max and median values for each attribute
min(dfB1$ECG.HR)
max(dfB1$ECG.HR)
median(dfB1$ECG.HR)

mean(dfB1$EDR.BR)
min(dfB1$EDR.BR)
max(dfB1$EDR.BR)
median(dfB1$EDR.BR)

mean(dfB1$Belt.BR)
min(dfB1$Belt.BR)
max(dfB1$Belt.BR)
median(dfB1$Belt.BR)

mean(dfB1$CoreTemp, na.rm = T)
min(dfB1$CoreTemp, na.rm = T)
max(dfB1$CoreTemp, na.rm = T)
median(dfB1$CoreTemp, na.rm = T)

mean(dfB1$Temp)
min(dfB1$Temp)
max(dfB1$Temp)
median(dfB1$Temp)

gplot <- c(rep("ECG.HR                            Mean: 148.26 | Median: 148.35 | Min: 76.3 | Max: 221.0",length(dfB1$ECG.HR)),
           rep("EDR.BR                            Mean: 50.72 | Median: 50.75 | Min: 6 | Max: 69.1",length(dfB1$EDR.BR)),
           rep("Belt.BR                           Mean: 21.85 | Median: 21.80 | Min: 6 | Max: 30.7",length(dfB1$Belt.BR)),
           rep("CoreTemp                          Mean: 37.06 | Median: 37.05 | Min: 36.91 | Max: 37.21",length(dfB1$CoreTemp)),
           rep("Temp                              Mean: 30.62 | Median: 30.50 | Min: 25.9 | Max: 33.10",length(dfB1$Temp)))

#plotting the attributes w.r.t. time
tp <- xyplot(yplot~xplot|gplot, type="l", layout=c(1,5), 
             xlab="Time", ylab="Attributes", grid=T, lwd=2, main = "B1_2_BDU", fill = "blue",
             scales=list( x=list(draw=FALSE), 
                          y=list(relation='free',limits=list(c(6,30.7), c(36.9,37.22), 
                                                             c(76.2,221.1), c(6,69.1), c(25.9,33.1)))))


tp

# C2_2_MOPP
xplot <- rep((0:(length(dfC2$Time)-1))/1000,5) #x and y plot
yplot <- c(dfC2$ECG.HR, dfC2$EDR.BR, dfC2$Belt.BR, dfC2$CoreTemp, dfC2$Temp)

mean(dfC2$ECG.HR) #calculating the mean, min, max and median values for each attribute
min(dfC2$ECG.HR)
max(dfC2$ECG.HR)
median(dfC2$ECG.HR)

mean(dfC2$EDR.BR)
min(dfC2$EDR.BR)
max(dfC2$EDR.BR)
median(dfC2$EDR.BR)

mean(dfC2$Belt.BR)
min(dfC2$Belt.BR)
max(dfC2$Belt.BR)
median(dfC2$Belt.BR)

mean(dfC2$CoreTemp, na.rm = T)
min(dfC2$CoreTemp, na.rm = T)
max(dfC2$CoreTemp, na.rm = T)
median(dfC2$CoreTemp, na.rm = T)

mean(dfC2$Temp)
min(dfC2$Temp)
max(dfC2$Temp)
median(dfC2$Temp)

gplot <- c(rep("ECG.HR                            Mean: 97.65 | Median: 97.45 | Min: 59.1 | Max: 139.3",length(dfC2$ECG.HR)),
           rep("EDR.BR                            Mean: 22.75 | Median: 22.1 | Min: 11.4 | Max: 37.7",length(dfC2$EDR.BR)),
           rep("Belt.BR                           Mean: 16.55 | Median: 15 | Min: 6.8 | Max: 32.6",length(dfC2$Belt.BR)),
           rep("CoreTemp                          Mean: 37.18 | Median: 37.19 | Min: 36.85 | Max: 37.42",length(dfC2$CoreTemp)),
           rep("Temp                              Mean: 35.83 | Median: 36.7 | Min: 32.9 | Max: 37.0",length(dfC2$Temp)))

#plotting the attributes w.r.t. time
tp <- xyplot(yplot~xplot|gplot, type="l", layout=c(1,5), 
             xlab="Time", ylab="Attributes", grid=T, lwd=2, main = "C2_2_MOPP", fill = "blue",
             scales=list( x=list(draw=FALSE), 
                          y=list(relation='free',limits=list(c(6.7,32.7), c(36.84,37.43), 
                                                             c(59,139.4), c(11.2,37.8), c(32.8,37.1)))))

tp

# D2_2_MOPP
xplot <- rep((0:(length(dfD2$Time)-1))/1000,5) #x and y plot
yplot <- c(dfD2$ECG.HR, dfD2$EDR.BR, dfD2$Belt.BR, dfD2$CoreTemp, dfD2$Temp)

mean(dfD2$ECG.HR) #calculating the mean, min, max and median values for each attribute
min(dfD2$ECG.HR)
max(dfD2$ECG.HR)
median(dfD2$ECG.HR)

mean(dfD2$EDR.BR)
min(dfD2$EDR.BR)
max(dfD2$EDR.BR)
median(dfD2$EDR.BR)

mean(dfD2$Belt.BR)
min(dfD2$Belt.BR)
max(dfD2$Belt.BR)
median(dfD2$Belt.BR)

mean(dfD2$CoreTemp, na.rm = T)
min(dfD2$CoreTemp, na.rm = T)
max(dfD2$CoreTemp, na.rm = T)
median(dfD2$CoreTemp, na.rm = T)

mean(dfD2$Temp)
min(dfD2$Temp)
max(dfD2$Temp)
median(dfD2$Temp)

gplot <- c(rep("ECG.HR                            Mean: 114.18 | Median: 112.40 | Min: 77.4 | Max: 159.8",length(dfD2$ECG.HR)),
           rep("EDR.BR                            Mean: 22.35 | Median: 22.0 | Min: 3.0 | Max: 38.2",length(dfD2$EDR.BR)),
           rep("Belt.BR                           Mean: 21.10 | Median: 21.2 | Min: 5 | Max: 32.8",length(dfD2$Belt.BR)),
           rep("CoreTemp                          Mean: 37.57 | Median: 37.58 | Min: 37.53 | Max: 37.65",length(dfD2$CoreTemp)),
           rep("Temp                              Mean: 36.61 | Median: 37.30 | Min: 31.1 | Max: 38.1",length(dfD2$Temp)))

#plotting the attributes w.r.t. time
tp <- xyplot(yplot~xplot|gplot, type=c("l"), layout=c(1,5), 
             xlab="Time", ylab="Attributes", grid=T, lwd=2, main = "D2_2_MOPP", fill = "blue",
             scales=list( x=list(draw=FALSE), 
                          y=list(relation='free',limits=list(c(5,32.9), c(37.52,37.66), 
                                                             c(77.3,159.9), c(11.2,37.8), c(31,38)))))
tp
