######################################  Q1   ###################################################

#Reading the pooled.csv file
dat <- read.table("pooled.csv",header=T,sep=",",
                  colClasses=c("factor","factor","numeric","numeric","numeric","numeric",
                               "factor","factor","numeric","numeric","numeric","factor","numeric"))


##################################################################
##### What was the effect of time of day on RT and accuracy? #####
##################################################################
unique(dat$tod) #We have 4 time of day; 1=morning, 2=late morning 3= afternoon 4=evening

#using aggregate to calculate the mean, sd and length
md_1_rt <- aggregate(dat$rt, by=list(dat$tod), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
md_1_rt <- do.call(data.frame, md_1_rt)

#calculating the standard error
md_1_rt$se <- md_1_rt$x.sd / sqrt(md_1_rt$x.n)

#renaming the columns
colnames(md_1_rt) <- c("tod", "mean", "sd", "n", "se")
md_1_rt$TOD <- c('Morning', 'Late Morning', 'Afternoon', 'Evening')

#Doing the same for correlation
md_1_acc <- aggregate(dat$corr, by=list(dat$tod), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
md_1_acc <- do.call(data.frame, md_1_acc)
md_1_acc$se <- md_1_acc$x.sd / sqrt(md_1_acc$x.n)
colnames(md_1_acc) <- c("tod", "mean", "sd", "n", "se")
md_1_acc$TOD <- c('Morning', 'Late Morning', 'Afternoon', 'Evening')

##############################################################
##### What was the effect of session on RT and accuracy? #####
##############################################################
unique(dat$session) #We have 12 days

#using aggregate to calculate the mean, sd and length
md_2_rt <- aggregate(dat$rt, by=list(dat$session), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
md_2_rt <- do.call(data.frame, md_2_rt)

#calculating the standard error
md_2_rt$se <- md_2_rt$x.sd / sqrt(md_2_rt$x.n)

#renaming the columns
colnames(md_2_rt) <- c("session", "mean", "sd", "n", "se")
md_2_rt$SESS <- c('Day 1','Day 2','Day 3','Day 4','Day 5','Day 6','Day 7','Day 8','Day 9','Day 10','Day 11','Day 12')

#Doing the same for correlation
md_2_acc <- aggregate(dat$corr, by=list(dat$session), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
md_2_acc <- do.call(data.frame, md_2_acc)
md_2_acc$se <- md_2_acc$x.sd / sqrt(md_2_acc$x.n)
colnames(md_2_acc) <- c("session", "mean", "sd", "n", "se")
md_2_acc$SESS <- c('Day 1','Day 2','Day 3','Day 4','Day 5','Day 6','Day 7','Day 8','Day 9','Day 10','Day 11','Day 12')

tapply(dat$rt,list(tod=dat$tod,session=dat$session),mean)#for both

##############################################################################################
##### What was the effect of the correct response side (i.e., order) on RT and accuracy? #####
##############################################################################################

unique(dat$order) #We have 2 orders left-shift or right-shift

#using aggregate to calculate the mean, sd and length
md_3_rt <- aggregate(dat$rt, by=list(dat$order), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
md_3_rt <- do.call(data.frame, md_3_rt)

#calculating the standard error
md_3_rt$se <- md_3_rt$x.sd / sqrt(md_3_rt$x.n)

#renaming the columns
colnames(md_3_rt) <- c("order", "mean", "sd", "n", "se")
md_3_rt$resp <- c('rshift','lshift')

#Doing the same for correlation
md_3_acc <- aggregate(dat$corr, by=list(dat$order), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
md_3_acc <- do.call(data.frame, md_3_acc)
md_3_acc$se <- md_3_acc$x.sd / sqrt(md_3_acc$x.n)
colnames(md_3_acc) <- c("order", "mean", "sd", "n", "se")
md_3_acc$resp <- c('rshift','lshift')


##############################################################
##### Was there improvement in RT/accuracy with session? #####
##############################################################

unique(dat$session) #We have 12 sessions which refers to the day the sample was taken

#using aggregate to calculate the mean, sd and length
md_4_rt <- aggregate(dat$rt, by=list(dat$session), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
md_4_rt <- do.call(data.frame, md_4_rt)

#calculating the standard error
md_4_rt$se <- md_4_rt$x.sd / sqrt(md_4_rt$x.n)

#renaming the columns
colnames(md_4_rt) <- c("session", "mean", "sd", "n", "se")
md_4_rt$SESS <- c('Day 1','Day 2','Day 3','Day 4','Day 5','Day 6','Day 7','Day 8','Day 9','Day 10','Day 11','Day 12')

#Doing the same for correlation
md_4_acc <- aggregate(dat$corr, by=list(dat$session), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
md_4_acc <- do.call(data.frame, md_4_acc)
md_4_acc$se <- md_4_acc$x.sd / sqrt(md_4_acc$x.n)
colnames(md_4_acc) <- c("session", "mean", "sd", "n", "se")
md_4_acc$SESS <- c('Day 1','Day 2','Day 3','Day 4','Day 5','Day 6','Day 7','Day 8','Day 9','Day 10','Day 11','Day 12')


#####################################################################
##### What was the effect of time of day                        #####
##### and session block (i.e., a 4x3 table) on RT and accuracy? #####
#####################################################################

unique(dat$session) #We have 12 sessions which refers to the day the sample was taken

#Creating the 3 session groups
d1<-dat[which(dat$session %in% c(1,2,3,4)),] 
d2<-dat[which(dat$session %in% c(5,6,7,8)),]
d3<-dat[which(dat$session %in% c(9,10,11,12)),]

#using aggregate to calculate the mean, sd and length for each session group
mydata_5_1 <- aggregate(d1$rt, by=list(d1$tod), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
mydata_5_1 <- do.call(data.frame, mydata_5_1)
mydata_5_2 <- aggregate(d2$rt, by=list(d2$tod), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
mydata_5_2 <- do.call(data.frame, mydata_5_2)
mydata_5_3 <- aggregate(d3$rt, by=list(d3$tod), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
mydata_5_3 <- do.call(data.frame, mydata_5_3)

#Calculating standard error for each group
mydata_5_1$se <- mydata_5_1$x.sd / sqrt(mydata_5_1$x.n)
mydata_5_2$se <- mydata_5_2$x.sd / sqrt(mydata_5_2$x.n)
mydata_5_3$se <- mydata_5_3$x.sd / sqrt(mydata_5_3$x.n)

#renaming the columns
colnames(mydata_5_1) <- c("tod", "mean", "sd", "n", "se")
colnames(mydata_5_2) <- c("tod", "mean", "sd", "n", "se")
colnames(mydata_5_3) <- c("tod", "mean", "sd", "n", "se")

mydata_5_1$TOD <- c('Morning', 'Late Morning', 'Afternoon', 'Evening')
mydata_5_2$TOD <- c('Morning', 'Late Morning', 'Afternoon', 'Evening')
mydata_5_3$TOD <- c('Morning', 'Late Morning', 'Afternoon', 'Evening')

mydata_5_1$type <- 'Session Block 1'
mydata_5_2$type <- 'Session Block 2'
mydata_5_3$type <- 'Session Block 3'

#using tapply here to calculate the mean, sd for correlation
tapply(d1$corr,d1$tod,mean)
tapply(d1$corr,d1$tod,sd)

tapply(d2$corr,d2$tod,mean)
tapply(d2$corr,d2$tod,sd)

tapply(d3$corr,d3$tod,mean)
tapply(d3$corr,d3$tod,sd)

######################################  Q2   ###################################################

##################################################################
##### What was the effect of time of day on RT and accuracy? #####
##################################################################

library("ggplot2", lib.loc="~/R/win-library/3.5") #We here are using ggplot library

#Doing same as question 1a 
mydata_1 <- aggregate(dat$rt, by=list(dat$tod), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
mydata_1 <- do.call(data.frame, mydata_1)

#Standard error
mydata_1$se <- mydata_1$x.sd / sqrt(mydata_1$x.n)
colnames(mydata_1) <- c("tod", "mean", "sd", "n", "se")
mydata_1$TOD <- c('Morning', 'Late Morning', 'Afternoon', 'Evening')

#dodge for plot positions and gap
dodge <- position_dodge(width = 0.9)

#limits for error plot
limits <- aes(ymax = mydata_1$mean + mydata_1$se,
              ymin = mydata_1$mean - mydata_1$se)

#Plotting barchart with errorbars specifying the standard error
p <- ggplot(data = mydata_1, aes(x = TOD, y = mean, fill = TOD))

p + geom_bar(stat = "identity", position = dodge, show.legend = T) + 
  geom_errorbar(limits, position = dodge, width = 0.3) + 
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic", hjust = .5),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")
  ) + ggtitle("Plot of Time of Day by Mean of Response Time") +
  xlab("Time of Day") + ylab("Mean of Response Time")

##############################################################
##### What was the effect of session on RT and accuracy? #####
##############################################################

library("ggplot2", lib.loc="~/R/win-library/3.5") #We here are using ggplot library

#Doing same as question 1b
mydata_2 <- aggregate(dat$rt, by=list(dat$session), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
mydata_2 <- do.call(data.frame, mydata_2)

#Standard error
mydata_2$se <- mydata_2$x.sd / sqrt(mydata_2$x.n)
colnames(mydata_2) <- c("session", "mean", "sd", "n", "se")
mydata_2$SESS <- c('Day 1','Day 2','Day 3','Day 4','Day 5','Day 6','Day 7','Day 8','Day 9','Day 10','Day 11','Day 12')

limits <- aes(ymax = mydata_2$mean + mydata_2$se,
              ymin = mydata_2$mean - mydata_2$se)
#matplot
matplot(mydata_2$session,mydata_2$mean,type = 'o',log = 'y',pch = 1, lty = 1,lwd = 2,
        ylim = c(800,1600),xlim = c(1,12),col = 1:12,
        xlab = 'Day Sample was Taken', ylab = "Mean of Response Time", main = 'Plot of Session by Mean of Response Time')
#matplot using GGPLOT
ggplot(mydata_2, aes(x = mydata_2$session, y = mydata_2$mean))+geom_line()+
  geom_point(size=3,shape=21,aes(fill=factor(mydata_2$session)),show.legend = F)+
  ggtitle("Plot of Session by Mean of Response Time") +
  xlab("Session") + ylab("Mean of Response Time")+ 
  geom_errorbar(limits, width=0.2, col='navy')+
  theme(plot.title = element_text(color="navy", size=12, face="bold.italic", hjust = .5),
        axis.title.x = element_text(color="navy", size=12, face="bold"),
        axis.title.y = element_text(color="navy", size=12, face="bold"))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c('1'='Day1','2'='Day2','3'='Day3','4'='Day4','5'='Day5','6'='Day6',
                              '7'='Day7','8'='Day8','9'='Day9','10'='Day10','11'='Day11','12'='Day12'))

##############################################################################################
##### What was the effect of the correct response side (i.e., order) on RT and accuracy? #####
##############################################################################################

#Doing same as question 1c
mydata_3 <- aggregate(dat$rt, by=list(dat$order), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
mydata_3 <- do.call(data.frame, mydata_3)

#Standard error
mydata_3$se <- mydata_3$x.sd / sqrt(mydata_3$x.n)
colnames(mydata_3) <- c("order", "mean", "sd", "n", "se")
mydata_3$resp <- c('rshift','lshift')

limits <- aes(ymax = mydata_3$mean + mydata_3$se,
              ymin = mydata_3$mean - mydata_3$se)

#Plotting barchart with errorbars specifying the standard error
p <- ggplot(data = mydata_3, aes(x = order, y = mean, fill = resp))

p + geom_bar(stat = "identity", position = dodge, show.legend = T) + 
  geom_errorbar(limits, position = dodge, width = 0.3) + 
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic", hjust = .5),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")
  ) + ggtitle("Plot of Correct Response Side by Mean of Response Time") +
  xlab("Order") + ylab("Mean of Response Time")

#####################################################################
##### Was there improvement in RT/accuracy within a session? #####
#####################################################################

#Doing same as question 1d
mydata_4 <- aggregate(dat$rt, by=list(dat$session), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
mydata_4 <- do.call(data.frame, mydata_4)

#Standard error
mydata_4$se <- mydata_4$x.sd / sqrt(mydata_4$x.n)
colnames(mydata_4) <- c("session", "mean", "sd", "n", "se")

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = mydata_4$mean + mydata_4$se,
              ymin = mydata_4$mean - mydata_4$se)

#plotting boxplot
ggplot(mydata_4, aes(x=as.factor(mydata_4$session), y=mydata_4$mean)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)+ 
  geom_errorbar(limits, position = dodge, width = 0.3, col=2)+ 
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic", hjust = .5),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")
  ) + ggtitle("Plot of Session by Mean of Response Time") +
  xlab("Session") + ylab("Mean of Response Time")+
  scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                   labels=c('1'='Day1','2'='Day2','3'='Day3','4'='Day4','5'='Day5','6'='Day6',
                            '7'='Day7','8'='Day8','9'='Day9','10'='Day10','11'='Day11','12'='Day12'))

################################################################
##### What was the effect of time of day                   #####
#and session block (i.e., a 4x3 table) on RT and accuracy? #####
################################################################

library("ggplot2", lib.loc="~/R/win-library/3.5")
library("cowplot", lib.loc="~/R/win-library/3.5")

d1<-dat[which(dat$session %in% c(1,2,3,4)),]
d2<-dat[which(dat$session %in% c(5,6,7,8)),]
d3<-dat[which(dat$session %in% c(9,10,11,12)),]

mydata_5_1 <- aggregate(d1$rt, by=list(d1$tod), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
mydata_5_1 <- do.call(data.frame, mydata_5_1)
mydata_5_2 <- aggregate(d2$rt, by=list(d2$tod), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
mydata_5_2 <- do.call(data.frame, mydata_5_2)
mydata_5_3 <- aggregate(d3$rt, by=list(d3$tod), FUN = function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
mydata_5_3 <- do.call(data.frame, mydata_5_3)

#Standard error
mydata_5_1$se <- mydata_5_1$x.sd / sqrt(mydata_5_1$x.n)
mydata_5_2$se <- mydata_5_2$x.sd / sqrt(mydata_5_2$x.n)
mydata_5_3$se <- mydata_5_3$x.sd / sqrt(mydata_5_3$x.n)


colnames(mydata_5_1) <- c("tod", "mean", "sd", "n", "se")
colnames(mydata_5_2) <- c("tod", "mean", "sd", "n", "se")
colnames(mydata_5_3) <- c("tod", "mean", "sd", "n", "se")

mydata_5_1$TOD <- c('Morning', 'Late Morning', 'Afternoon', 'Evening')
mydata_5_2$TOD <- c('Morning', 'Late Morning', 'Afternoon', 'Evening')
mydata_5_3$TOD <- c('Morning', 'Late Morning', 'Afternoon', 'Evening')

mydata_5_1$type <- 'Session Block 1'
mydata_5_2$type <- 'Session Block 2'
mydata_5_3$type <- 'Session Block 3'

A <- rbind(mydata_5_1,mydata_5_2,mydata_5_3)

#plotting bandplot
p1<-ggplot(data = mydata_5_1, aes(x = mydata_5_1$tod, y = mydata_5_1$mean, 
                                  ymin = mydata_5_1$mean - mydata_5_1$se,
                                  ymax = mydata_5_1$mean + mydata_5_1$se))+ 
  geom_line() + geom_point(size=3,shape=21,aes(fill=factor(mydata_5_1$tod)),show.legend = F)+
  geom_ribbon(alpha=0.2, fill='red',show.legend = F)+
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic", hjust = .5),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")
  ) + ggtitle("Plot of Time of Day and Session Group 1 by Mean of Response Time") +
  xlab("Time of Day") + ylab("Mean of Response Time")+
  scale_x_continuous(breaks=c(1,2,3,4),
                     labels=c('1'='Morning','2'='Late Morning','3'='Afternoon','4'='Evening'))

p2<-ggplot(data = mydata_5_2, aes(x = mydata_5_2$tod, y = mydata_5_2$mean, 
                                  ymin = mydata_5_2$mean - mydata_5_2$se,
                                  ymax = mydata_5_2$mean + mydata_5_2$se))+ 
  geom_line() + geom_point(size=3,shape=21,aes(fill=factor(mydata_5_2$tod)),show.legend = F)+
  geom_ribbon(alpha=0.2, fill='blue',show.legend = F)+
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic", hjust = .5),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")
  ) + ggtitle("Plot of Time of Day and Session Group 2 by Mean of Response Time") +
  xlab("Time of Day") + ylab("Mean of Response Time")+
  scale_x_continuous(breaks=c(1,2,3,4),
                     labels=c('1'='Morning','2'='Late Morning','3'='Afternoon','4'='Evening'))

p3<-ggplot(data = mydata_5_3, aes(x = mydata_5_3$tod, y = mydata_5_3$mean, 
                                  ymin = mydata_5_3$mean - mydata_5_3$se,
                                  ymax = mydata_5_3$mean + mydata_5_3$se))+ 
  geom_line() + geom_point(size=3,shape=21,aes(fill=factor(mydata_5_1$tod)),show.legend = F)+
  geom_ribbon(alpha=0.2, fill='green',show.legend = F)+
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic", hjust = .5),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")
  ) + ggtitle("Plot of Time of Day and Session Group 3 by Mean of Response Time") +
  xlab("Time of Day") + ylab("Mean of Response Time")+
  scale_x_continuous(breaks=c(1,2,3,4),
                     labels=c('1'='Morning','2'='Late Morning','3'='Afternoon','4'='Evening'))

plot_grid(p1, p2, p3, labels = "AUTO")

######################################  Q3   ###################################################

#we are passing the entire dataset
PlotCombo <- function(subnum,session,data) 
{
  #filtering the data
  dat3 <- data[which(data$subnum==subnum & data$session==session),]
  #Removing the duplicates from Time of day
  tod_3 <- unique(dat3["tod"])
  
  #checking if we have values or not
  if(is.null(tod_3) | dim(dat3)[1] == 0)
  {
    tod_3 = "No Data Available"
  } 
  else 
  {
    tod_3 <- unique(dat3["tod"])
  }
  
  #for Null plot
  if(is.null(dat3) | dim(dat3)[1] == 0) 
  {
    plot(0, 0, xlab = "Mean Response Time", ylab = "Trial number", col="gold",
         main = paste("Mean Response Time vs Trial number \n Session = ",session,
                      "| Subject Code = ", subnum, "| (", tod_3, ")"))
    warning('No data available')
    text(0, 0, col = "red", "No data available" , cex = 1)
    grid(col = "grey", lty = "dotted")
  } 
  # For plotting data
  else 
  {
    dat_mean <- aggregate(dat3$rt, list(dat3$trial), mean)
    
    plot(dat_mean$x, dat_mean$Group.1, xlab = "Mean Response Time" ,
         ylab = "Trial number", pch = 16, cex = 3, col="gold",
         main = paste("Mean of response time vs Trial number \n Session = ",session, 
                      "; Subject Code = ", subnum, "; TOD =", tod_3))
    text(dat_mean$x, dat_mean$Group.1, col = "black", dat_mean$x , cex = .7)
    grid(col = "grey", lty = "dotted")
  }
}

#Generating pdf for the output
pdf("sub_Z13.pdf") 
par(mfrow=c(1,1))
PlotCombo(subnum='Z13',session=1,dat)
PlotCombo(subnum='Z13',session=2,dat)
PlotCombo(subnum='Z13',session=3,dat)
PlotCombo(subnum='Z13',session=4,dat)
PlotCombo(subnum='Z13',session=5,dat)
PlotCombo(subnum='Z13',session=6,dat)
PlotCombo(subnum='Z13',session=7,dat)
PlotCombo(subnum='Z13',session=8,dat)
dev.off()