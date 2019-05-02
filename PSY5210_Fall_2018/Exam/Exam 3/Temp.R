library(nlme)
am <- lme(responsetime_log ~ baseImage + type + baseImage*type, random = (~1|subnum), data = data)

am2 <- lme(responsetime_log ~ baseImage + type, random = (~1|subnum), data = data)
anova(am, am2)


anova(lmer2, lmer)
