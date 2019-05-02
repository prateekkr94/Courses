#########################################################
############    PSY 5210 Exam 2              ############    
############    Problem4                     ############    
############    Submitted by: Prateek Kumar  ############    
#########################################################


D2 <- read.csv("D2_2_MOPP.csv", header=T) #reading the data

#Doing F-Test on selected columns
lm1 <- lm(Belt.BR~EDR.BR+Temp+CoreTemp+ECG.HR+Motion+Body.Pos+poly(as.numeric(Time)),data=D2)
summary(lm1)
summary(lm1)$adj.r.squared
drop1(lm1, test = "F")

lm2 <- lm(Belt.BR~EDR.BR+Temp+CoreTemp+ECG.HR+Body.Pos+poly(as.numeric(Time)),data=D2)
summary(lm2)
summary(lm2)$adj.r.squared
drop1(lm2, test = "F")

lm3 <- lm(Belt.BR~EDR.BR+Temp+ECG.HR+Body.Pos+poly(as.numeric(Time)),data=D2)
summary(lm3)
summary(lm3)$adj.r.squared
drop1(lm3, test = "F")

lm4 <- lm(Belt.BR~EDR.BR+Temp+ECG.HR+poly(as.numeric(Time),27),data=D2)
summary(lm4)
summary(lm4)$adj.r.squared
drop1(lm4, test = "F")

##############################################################################################

#Considering all the columns at once
lm_all <- lm(Belt.BR~poly(as.numeric(Time))+Temp+ECG.Qual+Belt.Qual+EDR.Qual+Imp.Qual+PWI.Conf+ECG.HR+
               EDR.BR+Imp.BR+HR.consist+BR.consist+Core.Serial+CoreTemp+Vbat+Body.Pos+Motion+
               SpO2+Nonin.HR+HFS.1+HFS.2+HFS.Therm, data=D2)
summary(lm_all)$adj.r.squared

#Applying BIC to get the simple model
step_all <- step(lm_all, direction = "both", k=log(nrow(D2)))
summary(step_all)

#Simpler model obtained
lm_final <- lm(Belt.BR~Temp+Vbat+BR.consist+ECG.HR+EDR.BR+PWI.Conf, data=D2)
summary(lm_final)$adj.r.squared

#Trying to improve the model by polynomial regression
lm_final_poly <- lm(Belt.BR~poly(Temp,10)+Vbat+BR.consist+poly(ECG.HR,2)+poly(EDR.BR,2)+PWI.Conf, data=D2)
summary(lm_final_poly)$adj.r.squared
