################################################
##### Problem 1: Categorical relationships #####
################################################

set.seed(1000)
indoor<- sample(c("A","B","C","D"),prob=c(5,8,25,6),size=200,replace=T)
outdoor <-sample(c("A","B","C","D"),prob=c(5,20,3,6),size=200,replace=T)
tab1 <- table(indoor,outdoor)
tab2 <- table(c(indoor,outdoor),rep(c("I","O"),each=200))

row.names(tab1) <- c('clowns','horses','chocolate fountain','magician')
colnames(tab1) <- c('clowns','horses','chocolate fountain','magician')
row.names(tab2) <- c('clowns','horses','chocolate fountain','magician')
addmargins(tab1)
addmargins(tab2)

#determine whether responses to indoor preference influenced individuals response to outdoor preference
#chisq.test(tab2[,1],p = tab2[,2],rescale.p = T)
ct_t1 <- chisq.test(tab1)
contingencyTableBF(tab1, sampleType = 'indepMulti', fixedMargin = 'cols')
#install.packages("corrplot")
library(corrplot)
corrplot(ct_t1$residuals, is.cor = F)

#determine whether indoor and outdoor preferences were the same
ct_t2 <- chisq.test(tab2)
contingencyTableBF(tab2, sampleType = 'indepMulti', fixedMargin = 'cols')
corrplot(ct_t2$residuals, is.cor = F)
#Indoor high association for chocolate fountain(strong positive association) & high association for outdoor and horses
#There is repulsion between outdoor and chocolate fountain


####################################################
##### Problem 2: Which test to do: You decide. #####
####################################################

cardat <- read.table(text="age gender type origin origin.last carval carval.last
                     34 F SUV US US 16400 15800
                     31 M Truck US Europe 16900 16000
                     47 M Sedan US US 18800 17100
                     21 F Sedan Japan Japan 16000 15500
                     42 M SUV US Japan 16800 16100
                     43 F SUV US US 17200 16300
                     60 F Truck Europe Europe 19900 17800
                     37 M Truck Europe Europe 17100 16200
                     46 F SUV Japan Japan 16900 16300
                     27 M Sedan US US 16200 15700
                     50 M SUV US US 18800 17100
                     64 F SUV Japan US 50700 31700
                     33 M SUV Japan Japan 16500 15900
                     39 M Truck US Europe 17000 16200
                     58 F Sedan Japan US 19400 17500
                     53 F SUV US Europe 19200 17400
                     29 F Sedan US Japan 16300 15700
                     37 F Sedan US US 17300 16300
                     37 M SUV US Japan 18200 16700
                     54 F Sedan Japan Japan 24500 19800
                     46 F SUV Japan Europe 18000 16700
                     55 F SUV US Japan 28900 21700
                     46 F Truck US Europe 16600 16100
                     57 M SUV Europe Europe 24300 19700
                     40 M SUV US US 16800 16100
                     27 M Sedan Japan US 16900 16000
                     58 M SUV Europe Europe 20300 17900
                     64 M Truck US US 40600 27100
                     47 M Truck US Europe 18400 16900
                     32 M Truck US US 15900 15600
                     43 F Sedan Japan US 17200 16300
                     66 M Truck Europe Europe 19100 17500
                     36 F SUV US Japan 16900 16100
                     68 M Truck US US 69300 40100
                     54 F Sedan Japan US 17000 16400
                     64 M Truck Japan Europe 34900 24600
                     27 M SUV Japan Europe 15800 15500
                     51 F Sedan Japan Japan 29000 21700
                     69 M Sedan US Japan 54400 33400
                     25 F Sedan Japan Japan 15800 15500",header=T)

######################## Is there an impact of gender on the type of car purchased? ########################
t1 <- table(cardat$gender, cardat$type)
chisq.test(t1)

######################## Is there a difference in amount paid for a car for men versus women? ########################
t2 <- table(cardat$carval,cardat$gender) 

women = subset(cardat, cardat$gender == "F")
men = subset(cardat, cardat$gender == "M")
#overall differences
t.test(women$carval, men$carval)
wilcox.test(inc.women$carval, inc.men$carval, exact = FALSE, alternative = "two.sided")

library(BayesFactor)
ttestBF(inc.women$carval, inc.men$carval)

######################## Do people tend to buy vehicles from of the same origin as their last vehicle (US, europe, japan)? ########################
new <- cardat$origin
old <- cardat$origin.last
t3 <- table(old,new)
addmargins(t3)


######################## Is there a relationship between driver age and the value of his car? ########################
t4 <- cardat[,c('age','carval')]

model1 <- lm(t4$age~t4$carval)
model1

par(mfrow=c(1,1))
plot(t4$carval, t4$age, pch=16,cex=1.5,col="gold",
     main=paste("Best-fitting line\n",
                "y = ",round(model1$coef[1] ,2) ," + ", round(model1$coef[2],3) , " * x",sep=""))

points(t4$carval, t4$age, pch=1,cex=1.5,col="grey20")
abline(model1$coef,lwd=2)


#t4 <-t4[order(t4$age),]

summary(model1)

predictedy <- model1$coef[1] + model1$coef[2]* t4$carval

plot(t4$age,predictedy,cex=.5,col="grey20",pch=16)
abline(0,1)
cor(t4$age,predictedy)^2

######################## What is your best estimate for the value of a car driven by a 32, 52, and 62-year-old? ########################
sum(cardat$carval[cardat$age==32])
sum(cardat$carval[cardat$age==52])
sum(cardat$carval[cardat$age==62])

######################## Is there a relationship between how much someone paid for their previous car and how much they paid for their current car? ########################
t6 <- cardat[,c("carval","carval.last")]
cor.test(t6$carval, t6$carval.last)
# Conclusion: the correlation is almost perfect
# the closer it get to 1 the better it will be.

######################## Did people tend to pay more for their current car than their previous car? ########################
tmp1 <- cbind(mean(cardat[which(cardat$type=='Sedan'),"carval"]),mean(cardat[which(cardat$type=='Sedan'),"carval.last"]))
tmp2 <- cbind(mean(cardat[which(cardat$type=='SUV'),"carval"]),mean(cardat[which(cardat$type=='SUV'),"carval.last"]))
tmp3 <- cbind(mean(cardat[which(cardat$type=='Truck'),"carval"]),mean(cardat[which(cardat$type=='Truck'),"carval.last"]))
t7 <- rbind(tmp1,tmp2,tmp3)
row.names(t7) <- c('Sedan','SUV','Truck')
colnames(t7) <- c('carval','carval.last')

######################## Did trucks cost more than SUVs? ########################
q1<-subset( cardat, cardat$type == 'Truck') 
t8 <- cbind(sum(q1$carval),sum(q1$carval.last))

q2 <- subset( cardat, cardat$type == 'SUV') 
temp <- cbind(sum(q2$carval),sum(q2$carval.last))
t8 <- rbind(t8,temp)
row.names(t8) <- c('Truck','SUV')
colnames(t8) <- c('carval','carval.last')