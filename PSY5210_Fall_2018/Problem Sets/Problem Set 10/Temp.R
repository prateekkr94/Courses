library(ggplot2)
data <- read.csv("ps10data.csv")

ggplot(data,aes(x=day,y=stockprice)) + geom_point(aes(color=sector)) +theme_minimal()

#############################################################################
#############               QUESTION 1              #########################
#############################################################################

day.0 <- c("Mo","Tu","We","Th","Fr","Sa","Su")
data$day<- factor(data$day,levels=day.0) #add the level to months variable
aggregate(data$stockprice,list(data$day),mean)
model1 <- lm(stockprice~day, data=data)
model1
summary(model1)

#############################################################################
#############               QUESTION 2              #########################
#############################################################################

contrasts(data$day)<-contr.sdif(levels(data$day))
model2 <- lm(stockprice~day, data=data)
model2
summary(model2)

#############################################################################
#############               QUESTION 3              #########################
#############################################################################

pairwise.t.test(data$stockprice, data$day)

#############################################################################
#############               QUESTION 4              #########################
#############################################################################

TukeyHSD(aov(stockprice~day,data=data))

#############################################################################
#############               QUESTION 5              #########################
#############################################################################

kruskal.test(stockprice~day,data=data)
#summary(kruskal.test(stockprice~day,data=data))

#############################################################################
#############               QUESTION 6              #########################
#############################################################################
library(BayesFactor)
bfmodel <- anovaBF(stockprice~day,data=data)
bfmodel

#############################################################################
#############               QUESTION 2              #########################
#############################################################################

# 1. the effect of sector on its own (a one-way test), and
lm1 <- lm(stockprice~sector, data=data)
summary(lm1)
anova(lm1)
#summary(aov(lm(stockprice~sector, data=data)))

# 2. whether sector has an effect after day-of-week is considered: lm(stockprice~day+sector)
anova(lm(stockprice~day+sector, data=data))

# 3. whether the results differ if sector is included in the model first
anova(lm(stockprice~sector+day, data=data))