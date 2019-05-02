## 1. Data reading, management

library(gplots)
filter <- 1:30 ##ignor the last year, update this if you get more data:
alldat <- read.csv("weather_30yrs.csv")[filter,]



##snowfall and other data:
years <- alldat[,1]
snowfall <- alldat[,2:13]
elnino <- alldat[,16:27]
sunspots <- alldat[,29:40]
avgtemp <- alldat[,42:53]
ice <- alldat[,55:66]

str(alldat) # check the data types

n <- nrow(alldat) # Taking 30yrs data
c(as.character(alldat$Snowfall[1]), as.character(alldat$Snowfall[n])) # Timeline span

cols_withNa <- apply(alldat, 2, function(x) sum(is.na(x))) # Checking if any Null values