
library(plotrix)
library(faraway)

kanga_final <- kanga[!is.na(rowSums(kanga[,3:20])),]

sex_data <- as.numeric(kanga_final$sex)

#kanga_male<-subset(kanga_final, kanga_final$sex == "Male")
#kanga_female<-subset(kanga_final, kanga_final$sex == "Female")

#colnames(kanga_final)
var_comb <- kanga_final$basilar.length+kanga_final$occipitonasal.length+kanga_final$palate.length+kanga_final$palate.width+kanga_final$nasal.length+kanga_final$nasal.width+kanga_final$squamosal.depth+kanga_final$lacrymal.width+kanga_final$zygomatic.width+kanga_final$orbital.width+kanga_final$.rostral.width+kanga_final$occipital.depth+kanga_final$crest.width+kanga_final$foramina.length+kanga_final$mandible.length+kanga_final$mandible.width+kanga_final$mandible.depth+kanga_final$ramus.height

model_kanga_kanga <- lm(sex_data ~ var_comb, data=kanga_final)

summary_kanga<-summary(model_kanga_kanga)
summary_kanga

#plot(var_comb,sex_data,xlab="", ylab="Sum of Linear Variables", pch=19, col="blue")
#abline(lm(sex_data ~ var_comb, data=kanga_final), col="red")

#predict_val<-round(predict(model_kanga_kanga))
#predict_val

#sex_data

plot(model_kanga$fit~kanga_final$sex, xlab="Gender",ylab =" Gender coefficient ")
points(sex_data,model_kanga$fit )
abline(1.5 ,0 , lwd =3)
predictedgender <- model_kanga $ fit > 1.5
sex_tab <- table (sex_data ,c("Female","Male")[( predictedgender +1) ])
val <- (sex_tab[1,1]+sex_tab[2,2]) / (sex_tab[1,1]+sex_tab[1,2]+sex_tab[2,1]+sex_tab[2,2])*100

print(paste("Accuracy =", round(val,2),"%"))