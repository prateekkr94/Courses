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

# dodge <- position_dodge(width = 0.9)
# limit_1 <- aes(ymax = mydata_5_1$mean + mydata_5_1$se,
#               ymin = mydata_5_1$mean - mydata_5_1$se)
# 
# limit_2 <- aes(ymax = mydata_5_2$mean + mydata_5_2$se,
#                ymin = mydata_5_2$mean - mydata_5_2$se)
# 
# limit_3 <- aes(ymax = mydata_5_3$mean + mydata_5_3$se,
#                ymin = mydata_5_3$mean - mydata_5_3$se)


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
  xlab("Time of Day") + ylab("Mean of Response Time")

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
  xlab("Time of Day") + ylab("Mean of Response Time")

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
  xlab("Time of Day") + ylab("Mean of Response Time")
                    
plot_grid(p1, p2, p3, labels = "AUTO")