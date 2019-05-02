library(faraway)

kanga<-faraway::kanga

#colnames(kanga)
#SumofAll<-kanga$basilar.length+kanga$occipitonasal.length+kanga$palate.length+kanga$palate.width+kanga$nasal.length+kanga$nasal.width+kanga$squamosal.depth+kanga$lacrymal.width+kanga$zygomatic.width+kanga$orbital.width+kanga$occipital.depth+kanga$crest.width+kanga$foramina.length+kanga$mandible.length+kanga$mandible.width+kanga$mandible.depth+kanga$ramus.height+kanga$.rostral.width
var_comb <- kanga$basilar.length+kanga$occipitonasal.length+kanga$palate.length+kanga$palate.width+kanga$nasal.length+kanga$nasal.width+kanga$squamosal.depth+kanga$lacrymal.width+kanga$zygomatic.width+kanga$orbital.width+kanga$.rostral.width+kanga$occipital.depth+kanga$crest.width+kanga$foramina.length+kanga$mandible.length+kanga$mandible.width+kanga$mandible.depth+kanga$ramus.height

model_petalwidth <- lm(kanga$palate.width ~SumofAll )

missing <- kanga[is.na(kanga$palate.width),]

newpred <- round(predict(model_petalwidth,missing))

kanga$palate.width[is.na(kanga$palate.width)] <- newpred


Type_kanga<-as.numeric(kanga$sex)

model <- lm(Type_kanga~SumofAll )
model
plot (model$fit~kanga$sex, xlab="Gender",ylab =" Gender coefficient ")
points (Type_kanga,model$fit )
abline (1.5 ,0 , lwd =3)
predictedgender <- model $ fit > 1.5
table (Type_kanga ,c("Female","Male")[( predictedgender +1) ])

