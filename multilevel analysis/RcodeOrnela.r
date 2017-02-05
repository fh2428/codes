#install.packages("sas7bdat")
#install.packages("longitudinalData")
#install.packages("xtable")
library(sas7bdat)
library(longitudinalData)
library(xtable)

##### question 1
### read the SASdataset in R
mmse.data <- read.sas7bdat("C:/Users/frauke/Dropbox/2e/Robust statistics/Project")
attach(mmse.data)

#individual profiles per neuro group
png("C:/Users/user/Dropbox/MultilevelAssigment/indProf.png")
qplot(log(time), mmse, data=mmse.data, colour=as.factor(id),facets=.~NEURO, geom="path",main="Individual MMSE Profiles")
dev.off()

### mean evolution
meanMmse <- tapply(mmse,list(time,NEURO),mean)
matplot(log(unique(time)), meanMmse,type="l", xlab="Log of Time", ylab="Mean of MMSE")
legend(locator(1),c("Neuro=1","Neuro=0"),col=c(2,1),lty=1)

### variance evolution
sdMmse <- tapply(mmse,list(time,NEURO),var)
matplot(log(unique(time)),sdMmse,type="l", xlab="Log of Time", ylab="Std. Deviation of MMSE")
legend(locator(1),c("Neuro=1","Neuro=0"),col=c(2,1),lty=1)

#### Correlation
mmse.dataw <- reshapeWide( data.frame(cbind(id,time,mmse))  )
cor(mmse.dataw[,-1],use="pairwise.complete.obs")



### we can add a summary measures 
EE
#### Question 3
# read the random effects model Question 2
solutionr <- read.sas7bdat("C:/Users/frauke/Dropbox/2e/Robust statistics/Project/out.sas7bdat")

Intercept <- solutionr$Estimate[solutionr$Effect=="Intercept"]
slope <- solutionr$Estimate[solutionr$Effect=="ltime"]
plot(Intercept,slope)

#### same thing but with the model of question 4
solutionr2 <- read.sas7bdat("C:/Users/user/Dropbox/MultilevelAssigment/Multilevel-assignment/out2.sas7bdat")

Intercept2 <- solutionr2$Estimate[solutionr$Effect=="Intercept"]
slope2 <- solutionr2$Estimate[solutionr$Effect=="ltime"]

plot(Intercept2,slope2)



plot(c(),ylim=c(12,25),xlim=c(0,2.5),ylab="",xlab="")
abline(a=21.7624, b=0.953)
abline(a=21.7624-8.3807, b=0.953-0.3268,lty=2)

x=log(unique(time))
y = 0.203*x^2 + 2*(-2.37)*x+47.382+5.957
y2=0.195*x^2 + 2*(-1.665)*x +29.054+6.256
plot(x,y2,type='l', col = "red",lty=2)
lines(x,y2, lty=4)

#plot variance functions
plot(x,y)
#lines(predict(lo), col='red', lwd=2)
#for extended model


d11 <- 47.382
d22 <- 0.2034
d21 <- -2.3703
var <- 5.9572

timef <- factor(time)
md1 <- lm(mmse~timef + NEURO + timef*NEURO)
summary(log(unique(time)))

x=log(unique(time))

Var1 <- tapply(residuals(md1),time,var)
plot(log(unique(time)),Var1,ylab="Variance of MMSE",xlab="Log of time",type="b")
#lines(y=d22*x^2 + 2*d21*x+d11+var, x, type="b")
curve(d22*x^2 + 2*d21*x+d11+var,xlim=c(0,2.485),add=TRUE,col="red",lwd=2)

mmse.data

#### second model
d.11 <- 29.0542
d.22 <- 0.1952
d.21 <- -1.6651
var. <- 6.2558


housingf <- factor(housing, levels=c(1,2,3))
md2 <- lm(mmse~age + housingf + timef + NEURO + timef*NEURO)
nah <- is.na(housingf)

Var2 <- tapply(residuals(md2),time[nah==FALSE],var)
plot(log(unique(time)),Var2,ylab="Variance of MMSE",xlab="Log of time",type="b",ylim=c(15,40))
curve(d.22*x^2 + 2*d.21*x+d.11+var.,xlim=c(0,2.5),add=TRUE,col="blue",lwd=2)


#### Average evolution
meanEvol <- function(age, housing){
  if(housing==1){
    h=c(1,0)
    house="Alone"
  }   else{
    if(housing==2){
      h=c(0,1)
      house="with family or partner"
    } else{h=c(0,0)
           house="nursing home"}
  }
  b<- rnorm(1000,0,sqrt(5.7223))#5.72=variance of patient random effect, sample 1000 values for the RE
  ltime <- seq(0,log(12),0.01)
  k <- 1:length(b)
  mmse0 <- matrix(0,length(b), length(ltime))#matrix of 0s 1000*249(length ltime)
  mmse1 <- matrix(0,length(b), length(ltime))
  for(i in k){
    mmse0[i,] <- exp(-9.9227+b[i] + 0.1596*age -3.6591*h[1]
                     -3.3452*h[2]-0.09561*ltime)/(1+
                                                    exp(-9.9227+b[i] + 0.1596*age -3.6591*h[1]
                                                        -3.3452*h[2]-0.09561*ltime)) #E(Y_ij|b_i) for neuro=0
    
    mmse1[i,] <- exp(-9.9227+1.884+b[i] + 0.1596*age -3.6591*h[1]
                     -3.3452*h[2]+(-0.09561+0.6688)*ltime )/(1+
                                                               exp(-9.9227+1.884+b[i] + 0.1596*age -3.6591*h[1]
                                                                   -3.3452*h[2]+(-0.09561+.6688)*ltime )) #neuro =1
  }
  plot(ltime,colMeans(mmse0),type="l",ylim=c(0,1),ylab="P(MMSE<24)",xlab="Log(Time)",lwd=2,
       main=house)
  lines(ltime,colMeans(mmse1),col="red",lwd=2)
  legend("bottomleft",c("Neuro=1","Neuro=0"),col=c(2,1),lty=1)
}
r( mfrow = c( 1, 3 ) ) 
meanEvol(78,1)
meanEvol(78,2)
meanEvol(78,3)

