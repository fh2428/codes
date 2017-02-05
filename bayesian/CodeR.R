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

modelfile<-'C:/Users/Alvaro/Desktop/model1.txt'

data<-list('n','tsh','birthweight','gender')
vars2keep<-list('alpha1','alpha2','alpha3','alpha4','s','Postprob','meanF','meanM')
inits<-list(list(alpha1=0, alpha2=1, alpha3=1, alpha4=1,tau=1),list(alpha1=100, alpha2=100, alpha3=100, alpha4=100,tau=10))

output <- bugs(
  model.file=file.path(modelfile),
  data=data,
  inits = inits,
  parameters.to.save = vars2keep,
  n.chains=2,
  n.iter=5000,
  n.burnin=0,
  n.thin=1,
  bugs.directory = 'C:/Users/Alvaro/Desktop/WinBUGS14',
  debug=TRUE,
  DIC=TRUE
  )

print(output,digits.summary=5)

alpha1 <- output$sims.array[,,1]
plot(density(alpha1))
plot(alpha1[,1],type="l")
lines(alpha1[,2],type="l",col="red")

acf(ts(alpha1[,1]))
acf(ts(alpha1[,2]))


#### predictive distribution
X <- cbind(1,birthweight,gender,birthweight*gender)
XXinv <- solve(t(X)%*%X)
nsim <- output$n.sims

# female
yf <- c()
xf0 <- c(1,4000,0,0) 
vf <- t(xf0)%*%XXinv%*%xf0

meanF <- output$sims.matrix[,7]
varF <- output$sims.matrix[,5]^{2}*(1+vf)

for(i in 1:nsim){ yf[i] <- rnorm(1,meanF[i],sqrt(varF[i]))    }
plot(density(exp(yf)))
summary(exp(yf))
sd(exp(yf))

# male
ym <- c()
xm0 <- c(1,4000,1,4000) 
vm <- t(xf0)%*%XXinv%*%xf0

meanM <- output$sims.matrix[,8]
varm <- output$sims.matrix[,5]^{2}*(1+vm)

for(i in 1:nsim){ ym[i] <- rnorm(1,meanM[i],sqrt(varm[i]))    }
plot(density(exp(ym)))
summary(exp(ym))





###### linear regression
mod <- lm(log(tsh)~ birthweight*gender)
newdata <- data.frame(birthweight = c(4000,4000), gender=c(0,1))
predict(mod,newdata,interval="prediction",se.fit=TRUE)

mean(yf)
quantile(yf,c(0.025,0.975))