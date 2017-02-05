#install.packages("sas7bdat")
#install.packages("longitudinalData")
#install.packages("xtable")
library(sas7bdat)
library(longitudinalData)
library(xtable)

##### question 1


### read the SASdataset in R
mmse.data <- read.sas7bdat("C:/Users/frauke/Dropbox/1e/multilevel, longitudinal, mixed models/final assignment/mmse.sas7bdat")
attach(mmse.data)

### mean evolution
meanMmse <- tapply(mmse,list(time,NEURO),mean)
matplot(log(unique(time)), meanMmse,type="l")

### variance evolution
sdMmse <- tapply(mmse,list(time,NEURO),sd)
matplot(log(unique(time)),sdMmse,type="l")

#### Correlation
mmse.dataw <- reshapeWide( data.frame(cbind(id,time,mmse))  )
cor(mmse.dataw[,-1],use="pairwise.complete.obs")

### we can add a summary measures 

#### Question 3
# read the random effects model Question 2
solutionr <- read.sas7bdat("C:/Users/frauke/Dropbox/1e/multilevel, longitudinal, mixed models/final assignment/Multilevel-assignment/out.sas7bdat")

Intercept <- solutionr$Estimate[solutionr$Effect=="Intercept"]
slope <- solutionr$Estimate[solutionr$Effect=="ltime"]
plot(Intercept,slope)

###################################################
###################################################

#### same thing but with the model of question 4
solutionr2 <- read.sas7bdat("C:/Users/frauke/Dropbox/1e/multilevel, longitudinal, mixed models/final assignment/Multilevel-assignment/out2.sas7bdat")

Intercept2 <- solutionr2$Estimate[solutionr$Effect=="Intercept"]
slope2 <- solutionr2$Estimate[solutionr$Effect=="ltime"]

plot(Intercept2,slope2, ylab="Slope", xlab="Intercept", main="Linear Model including housing and age")

