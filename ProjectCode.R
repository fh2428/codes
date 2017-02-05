library(R2WinBUGS)

## the dataset
tsh = c(0.67,1.06,1.27,1.11,0.53,0.90,1.12,1.03,0.72,0.76,4.16,0.67,2.14,0.63,1.38,0.41,1.80,1.86,
        0.81,2.67,1.47,1.63,0.91,0.71,0.37,0.57,0.73,0.99,0.63,2.78,0.91,1.09,1.16,0.88,2.30,1.69,
        0.66,2.77,1.08,0.66,1.03,0.35,1.06,0.54,0.90,1.31,1.07,0.33,0.79,1.23,1.04,0.66,1.56,1.05,
        1.92,0.69,0.76,0.98,0.92,1.12,1.86,0.40,3.12,0.68,1.53,0.58,1.74,0.83,0.52,0.85,0.79,1.34,
        1.81,1.04,1.52,0.80,1.43,2.41,6.84)
birthweight= c(3930, 3500, 3260, 3360, 3150, 3050, 2960, 2930, 2900, 3530, 3900, 3650, 3100, 3020, 
               3730, 2790, 3300, 3010, 3710, 3490, 3110, 2900, 3850, 3610, 3960, 2880, 3070, 3760, 
               3130, 3700, 4280, 2880, 3190, 4380, 3500, 3220, 2550, 4250, 3510, 3400, 3360, 3350, 
               4410, 3220, 2950, 3540, 3820, 3250, 3380, 3250, 3110, 3770, 3500, 3460, 3300, 3040, 
               3630, 3500, 3740, 3060, 3170, 3920, 4000, 3180, 3350, 3110, 2880, 3120, 2980, 3880, 
               3640, 2530, 3450, 3630, 3140, 3390, 3850, 2650, 2200)
gender=  c(0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 
           0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 
           1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0)
n <- 79

## Model specification

modelfile<-'C:/Users/Alvaro/Dropbox/Master in Statistics/Semester II/Concepts of Bayesian Data Analysis/project/model1.txt'

data<-list('n','tsh','birthweight','gender')
vars2keep<-list('alpha1','alpha2','alpha3','alpha4','s')

inits<-list(list(alpha1=0, alpha2=0, alpha3=0, alpha4=0,tau=1),
            list(alpha1=10, alpha2=10, alpha3=10, alpha4=10,tau=0.01))

#### 1000 interactions 2 chains
output <- bugs(
  model.file=file.path(modelfile),
  data=data,
  inits = inits,
  parameters.to.save = vars2keep,
  n.chains=2,
  n.iter=1000,
  n.burnin=0,
  n.thin=1,
  bugs.seed=1,
  bugs.directory = 'C:/Users/Alvaro/Desktop/WinBUGS14',
  debug=TRUE,
  DIC=TRUE
)

#### History plot
alpha1 <- output$sims.array[,,1]
plot(alpha1[,2],type="l", ylab=expression(alpha[1]),xlab="Iteration")
lines(alpha1[,1],type="l",col="red")

alpha2 <- output$sims.array[,,2]
plot(alpha2[,2],type="l", ylab=expression(alpha[2]),xlab="Iteration")
lines(alpha2[,1],type="l",col="red")

alpha3 <- output$sims.array[,,3]
plot(alpha3[,2],type="l", ylab=expression(alpha[3]),xlab="Iteration")
lines(alpha3[,1],type="l",col="red")

alpha4 <- output$sims.array[,,4]
plot(alpha4[,2],type="l", ylab=expression(alpha[4]),xlab="Iteration")
lines(alpha4[,1],type="l",col="red")

s <- output$sims.array[,,5]
plot(s[,2],type="l", ylab=expression(sigma),xlab="Iteration")
lines(s[,1],type="l",col="red")

#### autocorrelation plot
acf(alpha1[,1],main="")
acf(alpha1[,2],main="")

acf(alpha2[,1],main="")
acf(alpha2[,2],main="")

acf(alpha3[,1],main="")
acf(alpha3[,2],main="")

acf(alpha4[,1],main="")
acf(alpha4[,2],main="")

acf(s[,1],main="")
acf(s[,2],main="")

##### thinning
#### 10000 interactions with thin=10 2 chains
outputThin <- bugs(
  model.file=file.path(modelfile),
  data=data,
  inits = inits,
  parameters.to.save = vars2keep,
  n.chains=2,
  n.iter=10000,
  n.burnin=0,
  n.thin=10,
  bugs.seed=1,
  bugs.directory = 'C:/Users/Alvaro/Desktop/WinBUGS14',
  debug=TRUE,
  DIC=TRUE
)

#### History plot
alpha1Thin <- outputThin$sims.array[,,1]
plot(alpha1Thin[,2],type="l", ylab=expression(alpha[1]),xlab="Iteration")
lines(alpha1Thin[,1],type="l",col="red")

alpha2Thin <- outputThin$sims.array[,,2]
plot(alpha2Thin[,2],type="l", ylab=expression(alpha[2]),xlab="Iteration")
lines(alpha2Thin[,1],type="l",col="red")

alpha3Thin <- outputThin$sims.array[,,3]
plot(alpha3Thin[,2],type="l", ylab=expression(alpha[3]),xlab="Iteration")
lines(alpha3Thin[,1],type="l",col="red")

alpha4Thin <- outputThin$sims.array[,,4]
plot(alpha4Thin[,2],type="l", ylab=expression(alpha[4]),xlab="Iteration")
lines(alpha4Thin[,1],type="l",col="red")

sThin <- outputThin$sims.array[,,5]
plot(sThin[,2],type="l", ylab=expression(sigma),xlab="Iteration")
lines(sThin[,1],type="l",col="red")

#### autocorrelation plot
acf(alpha1Thin[,1],main="")
acf(alpha1Thin[,2],main="")

acf(alpha2Thin[,1],main="")
acf(alpha2Thin[,2],main="")

acf(alpha3Thin[,1],main="")
acf(alpha3Thin[,2],main="")

acf(alpha4Thin[,1],main="")
acf(alpha4Thin[,2],main="")

acf(sThin[,1],main="")
acf(sThin[,2],main="")

##### model centering covariates
modelfileC<-'C:/Users/Alvaro/Dropbox/Master in Statistics/Semester II/Concepts of Bayesian Data Analysis/project/model2.txt'

#### 1000 interactions 2 chains (centering covariates)
outputC <- bugs(
  model.file=file.path(modelfileC),
  data=data,
  inits = inits,
  parameters.to.save = vars2keep,
  n.chains=2,
  n.iter=1000,
  n.burnin=0,
  n.thin=1,
  bugs.seed=1,
  bugs.directory = 'C:/Users/Alvaro/Desktop/WinBUGS14',
  debug=TRUE,
  DIC=TRUE
)

#### History plot
alpha1C <- outputC$sims.array[,,1]
plot(alpha1C[,2],type="l", ylab=expression(alpha[1]),xlab="Iteration")
lines(alpha1C[,1],type="l",col="red")

alpha2C <- outputC$sims.array[,,2]
plot(alpha2C[,2],type="l", ylab=expression(alpha[2]),xlab="Iteration")
lines(alpha2C[,1],type="l",col="red")

alpha3C <- outputC$sims.array[,,3]
plot(alpha3C[,2],type="l", ylab=expression(alpha[3]),xlab="Iteration")
lines(alpha3C[,1],type="l",col="red")

alpha4C <- outputC$sims.array[,,4]
plot(alpha4C[,2],type="l", ylab=expression(alpha[4]),xlab="Iteration")
lines(alpha4C[,1],type="l",col="red")

sC <- outputC$sims.array[,,5]
plot(sC[,2],type="l", ylab=expression(sigma),xlab="Iteration")
lines(sC[,1],type="l",col="red")

#### autocorrelation plot
acf(alpha1C[,1],main="")
acf(alpha1C[,2],main="")

acf(alpha2C[,1],main="")
acf(alpha2C[,2],main="")

acf(alpha3C[,1],main="")
acf(alpha3[,2],main="")

acf(alpha4[,1],main="")
acf(alpha4[,2],main="")

acf(s[,1],main="")
acf(s[,2],main="")


#### POSTERIOR INFERENCES
alpha1T <- outputThin$sims.matrix[,1]
alpha2T <- outputThin$sims.matrix[,2]
alpha3T <- outputThin$sims.matrix[,3]
alpha4T <- outputThin$sims.matrix[,4]
sT <- outputThin$sims.matrix[,5]

print(outputThin,digits.summary=5)

plot(density(alpha1T),main="",xlab=expression(alpha[1]))
plot(density(alpha2T),main="",xlab=expression(alpha[2]))
plot(density(alpha3T),main="",xlab=expression(alpha[3]))
plot(density(alpha4Thin),main="",xlab=expression(alpha[4]))
plot(density(sT^{2}),main="",xlab=expression(sigma^2))

#### posterior probability that the difference in slopes of the
#### effect of birthweight for males as compared to females is positive
mean(alpha4T > 0)

#### the expected TSH value for males and females (separately)
#### with birthweight equal to 4000g

MeanF4000 <- alpha1T + alpha2T*4000
MeanM4000 <- alpha1T + alpha2T*4000 + alpha3T + alpha4T*4000 

exp(mean(MeanF4000));exp(mean(MeanM4000))

#### predictive distribution for the TSH values of males
#### and females (separately) with birthweight equal to 4000g

X <- cbind(1,birthweight,gender,birthweight*gender)
XXinv <- solve(t(X)%*%X)
nsim <- outputThin$n.sims

# Calculation fo the variance for female
yf <- c()
xf0 <- c(1,4000,0,0)
vf <- t(xf0)%*%XXinv%*%xf0
varF <- sT^{2}*(1+vf)


for(i in 1:nsim){ yf[i] <- rnorm(1,MeanF4000[i],sqrt(varF[i]))    }

## predictive distribution for females
plot(density(exp(yf)),main="",xlab="TSH for females with birthweight=4000")
summary(exp(yf))
sd(exp(yf))

# Calculation fo the variance for males
ym <- c()
xm0 <- c(1,4000,1,4000)
vm <- t(xf0)%*%XXinv%*%xf0
varM <- sT^{2}*(1+vm)


for(i in 1:nsim){ ym[i] <- rnorm(1,MeanM4000[i],sqrt(varM[i]))    }

## predictive distribution for males
plot(density(exp(ym)),main="",xlab="TSH for males with birthweight=4000")
summary(exp(ym))
sd(exp(ym))

##### probability that a future newb orn with birthweight equal
##### to 4000g has a TSH value large than 2
# for females
mean(exp(yf)>2)
mean(exp(ym)>2)


#### the Bayesian analog of the adjusted co efficient of determination

R2b <- 1-mean(sT^2)/var(log(tsh))

#### standardised residuals
meanAlpha<- colMeans(outputThin$sims.matrix[,1:4])
r <- (log(tsh) - X%*%meanAlpha)/sd(X%*%meanAlpha)
plot(r,ylab="standardised residuals")
abline(h=c(-2,2),lty=2)

#### Delete observations with values with standardised residual outside the reference
#### interval [???2, 2]

tsh <- tsh[abs(residuals) < 2]
birthweight <- birthweight[abs(residuals) < 2]
gender <- gender[abs(residuals) < 2]
n <- length(tsh)

#### 10000 interactions with thin=10 2 chains
data<-list('n','tsh','birthweight','gender')

outputNO <- bugs(
  model.file=file.path(modelfile),
  data=data,
  inits = inits,
  parameters.to.save = vars2keep,
  n.chains=2,
  n.iter=10000,
  n.burnin=0,
  n.thin=10,
  bugs.seed=1,
  bugs.directory = 'C:/Users/Alvaro/Desktop/WinBUGS14',
  debug=TRUE,
  DIC=TRUE
)

#### Do the outliers influence the posterior summary measures?
print(outputNO,digits.summary=5)
print(outputThin,digits.summary=5)

alpha1NO <- outputNO$sims.matrix[,1]
plot(density(alpha1NO),main="",xlab=expression(alpha[1]))
lines(density(alpha1T),col="red")

alpha2NO <- outputNO$sims.matrix[,2]
plot(density(alpha2NO),main="",xlab=expression(alpha[2]))
lines(density(alpha2T),col="red")

#### Replace the Gaussian distribution into a t-distribution with 4 degrees
#### of freedom.
# we have to read the (complete) data again
modelfilet4 <-'C:/Users/Alvaro/Dropbox/Master in Statistics/Semester II/Concepts of Bayesian Data Analysis/project/model3.txt'

outputt4 <- bugs(
  model.file=file.path(modelfilet4),
  data=data,
  inits = inits,
  parameters.to.save = vars2keep,
  n.chains=2,
  n.iter=10000,
  n.burnin=0,
  n.thin=10,
  bugs.seed=1,
  bugs.directory = 'C:/Users/Alvaro/Desktop/WinBUGS14',
  debug=TRUE,
  DIC=TRUE
)

print(outputt4,digits.summary=5)
print(outputThin,digits.summary=5)


alpha1t4 <- outputt4$sims.matrix[,1]
plot(density(alpha1T),main="",xlab=expression(alpha[1]))
lines(density(alpha1t4),col="red")

alpha2t4 <- outputt4$sims.matrix[,2]
plot(density(alpha2T),main="",xlab=expression(alpha[2]))
lines(density(alpha2t4),col="red")

alpha3t4 <- outputt4$sims.matrix[,3]
plot(density(alpha3T),main="",xlab=expression(alpha[3]))
lines(density(alpha3t4),col="red")

alpha4t4 <- outputt4$sims.matrix[,4]
plot(density(alpha4T),main="",xlab=expression(alpha[4]))
lines(density(alpha4t4),col="red")

st4 <- outputt4$sims.matrix[,5]
plot(density(sT^2),main="",xlab=expression(sigma^2))
lines(density(st4^2),col="red")

## Model specification

modelfile<-'C:/Users/Alvaro/Dropbox/Master in Statistics/Semester II/Concepts of Bayesian Data Analysis/project/model1.txt'

alphameans <- matrix(0,10,4)
for(k in 1:10){
  
  modelfile <-paste('C:/Users/Alvaro/Dropbox/bayesian data analysis I/R code/modelEx',k,'.txt', sep = "")
  
  output <- bugs(
    model.file=file.path(modelfile),
    data=data,
    inits = inits,
    parameters.to.save = vars2keep,
    n.chains=2,
    n.iter=10000,
    n.burnin=0,
    n.thin=10,
    bugs.seed=1,
    bugs.directory = 'C:/Users/Alvaro/Desktop/WinBUGS14',
    debug=TRUE,
  )
  alphameans[k,] <- colMeans(output$sims.matrix[,1:4])
}
