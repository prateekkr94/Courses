#########################################################
############    PSY 5210 Exam 2              ############    
############    Problem3                     ############    
############    Submitted by: Prateek Kumar  ############    
#########################################################

A2 <- read.csv("A2_1_MOPP.csv", header=T) #reading the data from the CSV
B1 <- read.csv("B1_2_BDU.csv", header=T)

A2_BR <- c(A2$EDR.BR, A2$Belt.BR) #one-dimensional vector
t <- rep(1:nrow(A2),2) #time predictor
c <- rep(c("ecg","belt"), times = 1, length.out = NA, each = 592) #categorical predictor

df_A2 = cbind(time=t,type=c,rate=A2_BR) #combining the 3 vectors to a matrix
df_A2 = data.frame(df_A2) #converting to the dataframe
df_A2$time = as.numeric(df_A2$time) #converting time and rate into numeric
df_A2$rate = as.numeric(as.matrix(df_A2$rate))

B1_BR <- c(B1$EDR.BR, B1$Belt.BR) #repeating same for B1 data
t <- rep(1:nrow(B1),2)
c <- rep(c("ecg","belt"), times = 1, length.out = NA, each = 1294)

df_B1 = cbind(time=t,type=c,rate=B1_BR)
df_B1 = data.frame(df_B1)
df_B1$time = as.numeric(df_B1$time)
df_B1$rate = as.numeric(as.matrix(df_B1$rate))

#polynomial regression on A2 data
model_A2_1 <- lm(rate ~ poly(time, 1, raw = TRUE) + type, data = df_A2)
summary(model_A2_1)$adj.r.squared
model_A2_2 <- lm(rate ~ poly(time, 2, raw = TRUE) + type, data = df_A2)
summary(model_A2_2)$adj.r.squared
model_A2_3 <- lm(rate ~ poly(time, 3, raw = TRUE) + type, data = df_A2)
summary(model_A2_3)$adj.r.squared
model_A2_4 <- lm(rate ~ poly(time, 4, raw = TRUE) + type, data = df_A2)
summary(model_A2_4)$adj.r.squared
model_A2_5 <- lm(rate ~ poly(time, 5, raw = TRUE) + type, data = df_A2)
summary(model_A2_5)$adj.r.squared
model_A2_6 <- lm(rate ~ poly(time, 6, raw = TRUE) + type, data = df_A2)
summary(model_A2_6)$adj.r.squared
model_A2_7 <- lm(rate ~ poly(time, 7, raw = TRUE) + type, data = df_A2)
summary(model_A2_7)$adj.r.squared
model_A2_8 <- lm(rate ~ poly(time, 8, raw = TRUE) + type, data = df_A2)
summary(model_A2_8)$adj.r.squared
model_A2_9 <- lm(rate ~ poly(time, 9, raw = TRUE) + type, data = df_A2)
summary(model_A2_9)$adj.r.squared
model_A2_10 <- lm(rate ~ poly(time, 10, raw = TRUE) + type, data = df_A2)
summary(model_A2_10)$adj.r.squared
model_A2_20 <- lm(rate ~ poly(time, 20, raw = TRUE) + type, data = df_A2)
summary(model_A2_20)$adj.r.squared
model_A2_30 <- lm(rate ~ poly(time, 30, raw = TRUE) + type, data = df_A2)
summary(model_A2_30)$adj.r.squared
model_A2_50 <- lm(rate ~ poly(time, 50, raw = TRUE) + type, data = df_A2)
summary(model_A2_50)$adj.r.squared
model_A2_70 <- lm(rate ~ poly(time, 70, raw = TRUE) + type, data = df_A2)
summary(model_A2_70)$adj.r.squared
model_A2_110 <- lm(rate ~ poly(time, 110, raw = TRUE) + type, data = df_A2)
summary(model_A2_110)$adj.r.squared

# summary(model_A2_1)
# model_A2_1$coefficients

#Extracting the AIC value
data.frame(model = paste ("lm" ,1:15 , sep =""), 
           rbind ( extractAIC ( model_A2_1 ), 
                   extractAIC ( model_A2_2 ), 
                   extractAIC ( model_A2_3 ), 
                   extractAIC ( model_A2_4 ), 
                   extractAIC ( model_A2_5 ),
                   extractAIC ( model_A2_6 ),
                   extractAIC ( model_A2_7 ),
                   extractAIC ( model_A2_8 ),
                   extractAIC ( model_A2_9 ),
                   extractAIC ( model_A2_10 ),
                   extractAIC ( model_A2_20 ),
                   extractAIC ( model_A2_30 ),
                   extractAIC ( model_A2_50 ),
                   extractAIC ( model_A2_70 ),
                   extractAIC ( model_A2_110 )))

#Extracting the BIC value
extractBIC <- function (model) 
  { 
  extractAIC (model ,k= log ( length ( model $ residuals ))) 
  }
data.frame( model = paste ("lm" ,1:15 , sep =""), 
            rbind ( extractBIC ( model_A2_1 ), 
                    extractBIC ( model_A2_2 ), 
                    extractBIC ( model_A2_3 ), 
                    extractBIC ( model_A2_4 ), 
                    extractBIC ( model_A2_5 ),
                    extractBIC ( model_A2_6 ),
                    extractBIC ( model_A2_7 ),
                    extractBIC ( model_A2_8 ),
                    extractBIC ( model_A2_9 ),
                    extractBIC ( model_A2_10 ),
                    extractBIC ( model_A2_20 ),
                    extractBIC ( model_A2_30 ),
                    extractBIC ( model_A2_50 ),
                    extractBIC ( model_A2_70 ),
                    extractBIC ( model_A2_110 )))

#ANOVA test
anova(model_A2_1, model_A2_2, model_A2_3, model_A2_4, model_A2_5, model_A2_6, model_A2_7, model_A2_8, 
      model_A2_9, model_A2_10, model_A2_20, model_A2_30, model_A2_50, model_A2_70, model_A2_110)

####################################################################

#polynomial regression on B1 data
model_B1_1 <- lm(rate ~ poly(time, 1, raw = TRUE) + type, data = df_B1)
summary(model_B1_1)$adj.r.squared
model_B1_2 <- lm(rate ~ poly(time, 2, raw = TRUE) + type, data = df_B1)
summary(model_B1_2)$adj.r.squared
model_B1_3 <- lm(rate ~ poly(time, 3, raw = TRUE) + type, data = df_B1)
summary(model_B1_3)$adj.r.squared
model_B1_4 <- lm(rate ~ poly(time, 4, raw = TRUE) + type, data = df_B1)
summary(model_B1_4)$adj.r.squared
model_B1_5 <- lm(rate ~ poly(time, 5, raw = TRUE) + type, data = df_B1)
summary(model_B1_5)$adj.r.squared
model_B1_6 <- lm(rate ~ poly(time, 6, raw = TRUE) + type, data = df_B1)
summary(model_B1_6)$adj.r.squared
model_B1_7 <- lm(rate ~ poly(time, 7, raw = TRUE) + type, data = df_B1)
summary(model_B1_7)$adj.r.squared
model_B1_8 <- lm(rate ~ poly(time, 8, raw = TRUE) + type, data = df_B1)
summary(model_B1_8)$adj.r.squared
model_B1_9 <- lm(rate ~ poly(time, 9, raw = TRUE) + type, data = df_B1)
summary(model_B1_9)$adj.r.squared
model_B1_10 <- lm(rate ~ poly(time, 10, raw = TRUE) + type, data = df_B1)
summary(model_B1_10)$adj.r.squared
model_B1_20 <- lm(rate ~ poly(time, 20, raw = TRUE) + type, data = df_B1)
summary(model_B1_20)$adj.r.squared
model_B1_30 <- lm(rate ~ poly(time, 30, raw = TRUE) + type, data = df_B1)
summary(model_B1_30)$adj.r.squared
model_B1_50 <- lm(rate ~ poly(time, 50, raw = TRUE) + type, data = df_B1)
summary(model_B1_50)$adj.r.squared
model_B1_70 <- lm(rate ~ poly(time, 70, raw = TRUE) + type, data = df_B1)
summary(model_B1_70)$adj.r.squared
model_B1_90 <- lm(rate ~ poly(time, 90, raw = TRUE) + type, data = df_B1)
summary(model_B1_90)$adj.r.squared

#Extracting AIC
data.frame(model = paste ("lm" ,1:15 , sep =""), 
           rbind ( extractAIC ( model_B1_1 ), 
                   extractAIC ( model_B1_2 ), 
                   extractAIC ( model_B1_3 ), 
                   extractAIC ( model_B1_4 ), 
                   extractAIC ( model_B1_5 ),
                   extractAIC ( model_B1_6 ),
                   extractAIC ( model_B1_7 ),
                   extractAIC ( model_B1_8 ),
                   extractAIC ( model_B1_9 ),
                   extractAIC ( model_B1_10 ),
                   extractAIC ( model_B1_20 ),
                   extractAIC ( model_B1_30 ),
                   extractAIC ( model_B1_50 ),
                   extractAIC ( model_B1_70 ),
                   extractAIC ( model_B1_90 )))

#Extracting BIC
extractBIC <- function (model) 
{ 
  extractAIC (model ,k= log ( length ( model $ residuals ))) 
}
data.frame( model = paste ("lm" ,1:15 , sep =""), 
            rbind ( extractBIC ( model_B1_1 ), 
                    extractBIC ( model_B1_2 ), 
                    extractBIC ( model_B1_3 ), 
                    extractBIC ( model_B1_4 ), 
                    extractBIC ( model_B1_5 ),
                    extractBIC ( model_B1_6 ),
                    extractBIC ( model_B1_7 ),
                    extractBIC ( model_B1_8 ),
                    extractBIC ( model_B1_9 ),
                    extractBIC ( model_B1_10 ),
                    extractBIC ( model_B1_20 ),
                    extractBIC ( model_B1_30 ),
                    extractBIC ( model_B1_50 ),
                    extractBIC ( model_B1_70 ),
                    extractBIC ( model_B1_90 )))

#ANOVA Test
anova(model_B1_1, model_B1_2, model_B1_3, model_B1_4, model_B1_5, model_B1_6, model_B1_7, model_B1_8, 
      model_B1_9, model_B1_10, model_B1_20, model_B1_30, model_B1_50, model_B1_70, model_B1_90)
