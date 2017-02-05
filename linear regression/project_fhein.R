#LM Project#######################################
##################################################

#EXERCISE 1#

rm(list=ls())

#(a)
#Explore the senic dataset and model a linear relationship. Give the regression 
#estimates and the standard errors. Interpret the output. Calculate 99% confidence
#intervals of the regression parameters.
senic<- read.table(file=file.choose(),header=TRUE)
names(senic)
attach(senic)
View(senic)

#explore data
summary(senic)
cor(senic)
par(mfrow=(c(1,4)))
boxplot(length,xlab='Length')
boxplot(age,xlab='Age')
boxplot(risk,xlab='Risk')
boxplot(facser,xlab='Facilities')

fit_senic<-lm(length~age + risk + facser)
summary(fit_senic)

confint(fit_senic,level=0.99)

#(b)
#Test with 10% significance level whether there is a linear relation between the response
#and the predictor variables. At the 99% level, can X1 and X3 be dropped from the
#model together?


# => 10% level ok
# => everything below can be dropped at 99% level
# run the summary again and compare:
fit_senic1<-lm(length~age +risk)
summary(fit_senic1)
#no improvement, confirmed

#(c)
#Examine the validity of the fitted regression model and corresponding inference.
#Graphically explore whether interactions would be useful to add to this model.

#[validity,inference, graph corrs]
fit_senic.stdres <- stdres(fit_senic)
par(mfrow=c(2,2))
qqnorm(fit_senic.stdres, main="")
qqline(fit_senic.stdres)
plot(fit_senic$residuals, xlab="Index", ylab="Residual")
plot(fit_senic$fitted.values, fit_senic$residuals, xlab="Fitted value", ylab="Residual")
lines(lowess(fit_senic$residuals~fit_senic$fitted.values), col="blue")
plot(fit_senic.stdres, xlab="Index", ylab="Standardized residual", ylim=c(-3,3))
abline(h=-2.5,lty=2)
abline(h=2.5,lty=2)

plot(fit_senic)

pairs(senic)
pairs(senic,panel=function(x,y){points(x,y);lines(lowess(x,y),col=8)})


# what is actaully relevant: shows the same result of two outliers
par(mfrow=c(1,1))

plot(stdres(fit_senic),xlab='Fitted Values',ylab='Standardized Residuals')
abline(h=c(-2.5,2.5),lty=2)

#(d)
#Build a regression model that can predict well the length of stay of future patients
#based on these three predictor variables.
fittest<-lm(length~.*.,data=senic,x=TRUE)
sjp.int(fittest,plevel=1,printPlot=TRUE)

pairs(length~age + risk + facser,data=senic)
abline(length~age + risk + facser)
#second OLS including interaction term
fit_senic2<-lm(length~age+risk+facser+risk*facser)
summary(fit_senic2)

# age could be dropped...is not sig

#(e)
#Investigate whether there are any isolated outliers that have a large 
#influence on the regression analysis.
fit_senic2.stdres <- stdres(fit_senic2)
par(mfrow=c(2,2))
qqnorm(fit_senic2.stdres, main="")
qqline(fit_senic2.stdres)
plot(fit_senic2$residuals, xlab="Index", ylab="Residual")
plot(fit_senic2$fitted.values, fit_senic3$residuals, xlab="Fitted value", ylab="Residual")
lines(lowess(fit_senic2$residuals~fit_senic2$fitted.values), col="blue")
plot(fit_senic2.stdres, xlab="Index", ylab="Standardized residual", ylim=c(-3,3))
abline(h=-2.5,lty=2)
abline(h=2.5,lty=2)
# no outliers
# again: better solution:
par(mfrow=c(1,1))
plot(stdres(fit_senic),xlab='Fitted Values',ylab='Standardized Residuals')
abline(h=c(-2.5,2.5),lty=2)
# 2 outliers

#(f) 
#Explain (in words) the meaning of the breakdown value. Compute the reweighted
#least trimmed squares estimator with 25% breakdown value. Identify the four different
#types of observations based on a diagnostic plot
# [h=0.75*133=99.75]

#(reweighted) Least trimmed squares, alpha=0.5
RLTS <- ltsReg(length ~ age + risk + facser + I(risk*facser), quan=0.75*133)
summary(RLTS)

par(mfrow=c(1,1))
plot(RLTS)
# heavier tail...2-5 outliers?

## double check
# Detection of outliers
plot(RLTS, which="rindex")
plot(RLTS, which="rdiag")

# Standardized residuals
RLTS.stdres <- RLTS$resid # already standardized by RLTS$scale
plot(RLTS.stdres, ylim=c(-5,20), ylab="Standardized residuals")
abline(h=c(-2.5,2.5), col="red")

#EXERCISE 2#

#(a)
#Model the relationship between salary and all other variables. If necessary use trans-
#formations, but keep all variables in the model. Interpret the results.

baseball <- read.delim("C:/Users/Frauke/Dropbox/KUL 14-15/1/lm/Linear Regression/final project/baseball.txt", header=FALSE)
names(baseball)
summary(baseball)

fit_bb<-lm(V1~.-V18-V19, data=baseball)
summary(fit_bb)

#make training set depending on level of V19
testset<- subset(baseball, V19==1)
fit_testset<-lm(V1~.-V18-V19, data=testset) #regression of trainset
summary(fit_testset)

#make validation set
valset<-subset(baseball, V19==0)
fit_valset<-lm(V1~.-V18-V19,data=valset)

# to find out lambda => 0.1, so 0 is part of the interval
boxcox(fit_testset, main="Box-Cox Tranformation") 
names(testset)
#check normality of response variable
hist(testset$V1, prob = TRUE, xlab = "Salary (in thousands of dollars)", main = "Histogram of Salary") 
#log transform the reponse in both sets
testset$VA <- log(testset$V1) 
valset$VA2<-log(valset$V1)
cbind(testset$VA, testset) 
cbind(valset$VA2, valset)

#regression with transformed salary
fit_testset <- lm(VA~.-V1-V18-V19, data=testset) 
summary(fit_testset)
#better fit

#(b)
#Compute the standardized and studentized residuals and detect possible outlying
#observations. Remove the outliers from the dataset.
par(mfrow=c(1,2))

# standard. resids:
base_res<-stdres(fit_testset)
base_res
plot(base_res,ylab='Standardized Residuals',xlab='Fitted Values',main="Standardized Residuals")
abline(h=c(-2.5,2.5),lty=2)

#studentized residuals
base_studres<-studres(fit_testset)
base_studres
plot(base_studres,ylab='Studentized Residuals',xlab='Fitted Values', main="Studentized Residuals")
abline(h=c(-2.5,2.5),lty=2)
# 1 possible outlier detetected, no differeces between std. res/stud.redids
#basically thats still acceptable in the range +/- 2.5, 3
View(base_res)
View(testset)
#if delete

fit_testset<-fit_testset[-131,]
fit_testset<-fit_testset[-108,]

View(testset)
#(c)
#Allow for interactions between number of runs and the four dummy variables and
#build stable models that reflect well the relation between salary and the most relevant
#predictors in the dataset. Which unique models do you obtain? Validate these models.

attach(testset)
names(testset)
head(V1)

#delete column V19 and V18 for later 2(d)
testset.V19.del <- testset
testset.V19.del$V19 <- NULL
testset.V19.del$V18 <- NULL
names(testset.V19.del)

#fir regression with interactions
fit_testset.int <- lm(VA ~ . + V4:V14 + V4:V15 + V4:V16 + V4:V17 -V1 , data = testset.V19.del) 
summary(fit_testset.int) # 1sig interaction

#develop a stable model
fit_stable<-stepAIC(fit_testset.int,direction="both") #va~3v v4 v4 v7v v9 v13 v14 v15 v16 v4:v15
#fit_stable$anova

#implement the stale model and validate
fit_testset.int2<- lm(VA2~V3+ V4+ V5+ V7+ V9 +V13 +V14 +V15 +V16+ V4:V15, data=valset)
summary(fit_testset.int2) #some more sig interactions

#prediction error
MSEP1 <- mean((predict(fit_testset.int, data=testset)-(testset$VA))^2) 
MSEP2 <- mean((predict(fit_testset.int2,data=valset)-(valset$VA2))^2) 
MSPE <- c(MSPE1,MSPE2) 
names(MSPE) <- c("full model","stepAIC model") 
MSPE
#0.1871273 0.3614834 

#(d) Employ the model with the best prediction abilities to predict the salary of a new
#baseball player (use the entire outlier-free dataset). The performance measures of the
#new baseball player are available on Toledo. Compute 90% prediction intervals too.

baseball_new <- read.table("C:/Users/Frauke/Dropbox/KUL 14-15/1/lm/Linear Regression/final project/baseball_newplayer.txt", quote="\"")

class(baseball_new)
#deleter cases
baseball_new$V18 <- NULL
baseball_new$V19 <- NULL
#define as numeric
baseball_new$V1 <- as.numeric(baseball_new$V1)

fit_testset.int$V18 <- NULL
fit_testset.int$V19 <- NULL

predict(fit_testset.int, newdata = baseball_new, interval="predict", level=.90)
#6.304. [5.295293 7.314492]
mean(baseball$V1)
# 1248.528

predict(fit_testset.int2, newdata = baseball_new, interval="predict", level=.90)
#6.278738 [5.151272,7.406203]
mean(baseball$V1)
#1248.528

#Exercise 3#
#(a)
#Fit a linear model. Consider transformations of predictors if they improve the model
#fit. Check the model assumptions and check for multicollinearity problems.

air<- read.table(file=file.choose(),header=TRUE)
attach(air)
View(air)
summary(air) 

fit_air<-lm(mort~.,data=air)
summary(fit_air) #only prec,(jant),nonw are significant

#re-scaling: because two variables are meas. in different scales
#mean std.ize -> pcr later
airmc<-scale(air, center = TRUE, scale = FALSE)
airmc<-data.frame(airmc)
attach(airmc)
names(airmc)

#fit a linear regression
fit_mort<-lm(mort~.,data=airmc)
summary(fit_mort) #still nonw, (jant), prec significant

#qq plot for studentized resid and leverage plots
qqPlot(fit_mort, main="QQ Plot") 
leveragePlots(fit_mort)

#detect partial resids:information in Y which is left after 
#the effects of all predictors but Xj have been eliminated.
crPlots(fit_mort, main="Component and Residuals")

#refit the linear regression, including some tranformations accoding to the cr plot
#analysyze residuals
refit_mort<-lm(airmc$mort~airmc$prec+airmc$jant+jult+ovr95+popn+educ+hous+dens+nonw+wwdrk+I(wwdrk^2)+poor+I(poor^2)+log(hc)+log(nox)+so+I(so^2)+I(so^3)+humid+I(humid^2)+I(humid^3),data=air)   
summary(refit_mort)
crPlots(refit_mort)

par(mfrow=c(1,1))
plot(stdres(refit_mort),xlab='Fitted Values',ylab='Standardized Residuals', main="Standardized Residuals")
abline(h=c(-2.5,2.5),lty=2) # no outliers

#(homework III)
#check correlations
mort_cor <- cor(airmc)
mort_cor
mort_corx <- mort_cor[-1,-1]
mort_VIF <- diag(solve(mort_corx))
mort_VIF
1/(1-summary(refit_mort, data=airmc)$r.squared) #~7, arguably high
#eigenvalues
mort_eig <- eigen(corx)$values
mort_eig #eigenvalues very small

#create validation set for the last 10obs.
valset_mort<-airmc[51:60,]
valset_mort

#root mean square error of predicton
RMSEP<-sqrt(sum((valset_mort$mort-predict(newdata=valset_mort,object=lm(mort~.,data=airmc)))^2)/10)
RMSEP #36.49

#(b) Perform principal component regression. Select an optimal model. Consider using a
#validation set (take the last 10 observations) and leave-one-out cross-validation. Do
#different selection techniques lead to the same model? Hint: for cross-validation you
#can use the option validation="LOO" in the R command pcr from the package pls.

#do oc regression
pcr_mort<-pcr(airmc$mort~airmc$prec+airmc$jant+airmc$jult+airmc$ovr95+airmc$popn+airmc$educ+airmc$hous+airmc$dens+airmc$nonw+airmc$wwdrk+airmc$poor+airmc$hc+airmc$nox+airmc$so+airmc$humid,data=airmc, ncomp=6, validation="LOO", jackknife=TRUE)
#plot(pcr_mort)
#substract pcr scores
pcr_mort$scores

coef(pcr_mort,ncomp=pcr_mort$ncomp)
fitted(pcr_mort)
explvar(pcr_mort)
summary(pcr_mort)
residuals(pcr_mort)
explvar(pcr_mort)

#crosvalidate with partial least squares
??pls
pls_mort<-plsr(airmc$mort~airmc$prec+airmc$jant+airmc$jult+airmc$ovr95+airmc$popn+airmc$educ+airmc$hous+airmc$dens+airmc$nonw+airmc$wwdrk+airmc$poor+airmc$hc+airmc$nox+airmc$so+airmc$humid, ncomp = 6, data=airmc,validation="LOO",jackknife=TRUE)
summary(pls_mort)  # 
plot(RMSEP(pls_mort))

plot(pls_mort, ncomp=5,asp=1,line=TRUE)
coef(pls_mort,ncomp=pls_mort$ncomp)
fitted(pls_mort)
explvar(pls_mort)
summary(pls_mort)
residuals(pls_mort)
explvar(pls_mort)
#yields similar results

#run regression
jack.test(pcr_mort,ncomp=5) 

#residuals
pcr_mort.resid<-pcr_mort$residuals
pcr_mort.resid
RMSEP<-sqrt(sum((valset_mort$mort-predict(newdata=valset_mort,object=lm(mort~.,data=airmc)))^2)/10)

RMSEP_pcr<-sqrt(sum(pcr_mort$residuals[50:60]^2)/10)
RMSEP_pcr #57.07328
RMSEP(pls_mort)

#(c)
#Perform ridge regression. Based on the validation set of the previous question
#calculate the RMSEP

#ridge regression use lm.ridge
fit_ridge<-lm.ridge(airmc$mort~prec+jant+jult+ovr95+popn+educ+hous+dens+nonw+wwdrk+poor+hc+nox+so+humid -1, ,data=airmc,lambda=seq(0,0.5,0.001))
plot(lm.ridge(mort~prec+jant+jult+ovr95+popn+educ+hous+dens+nonw+wwdrk+poor+hc+nox+so+humid -1,data=airmc,lambda=seq(0,0.5,0.001)),xlab="lambda",ylab="coefficients", mail="ridge regression")
#summary(fit_ridge)

#features
fit_ridge$coef
fit_ridge$residuals #null
fit_ridge$scales

#validate,ridge
#seperately
ridge_coef<-fit_ridge$coef/fit_ridge$scale
fit_ridge.res<<-valset_mort[,16]-(as.matrix(valset_mort[,-16])%*%as.matrix(ridge_coef))
res2<-(fit_ridge.res^2)
sse<-sum(res2)
rmsep_ridge<-sqrt(sse/10)
rmsep_ridge
#all at once
RMSEP_ridge=sqrt(sum((valset_mort[,16]-as.matrix(valset_mort[,-16])%*%as.matrix(ridge_coef))^2)/10)
RMSEP_ridge #815.7757

#EXERICISE 4#

#(a)Investigate this suspicion by 
#fitting a non-parametric curve to the data.

army<-read.table(file=file.choose(),header=TRUE)
attach(army)
names(army)

par(mfrow=c(1,2))

plot(Mo.Number, Draft_No., main="local lin. regression",xlab="Month",ylab="Drawing")
t <- c(2/3, 1/3, 1/10)
colors <- c("blue", "red", "purple")
for (i in 1:length(t)) lines(Mo.Number, predict(loess(Draft_No. ~ Mo.Number, span=t[i], degree=1), data=army), col=colors[i])
legend(40, -60, c("span = 2/3", "span = 1/3", "span = 1/10"), lty=1, col=c("blue", "red", "purple"))

# Local quadratic regression
plot(Mo.Number, Draft_No., main="local quad. regression",xlab="Month",ylab="Drawing")
for (i in 1:length(t)) lines(Mo.Number, predict(loess(Draft_No. ~ Mo.Number, span=t[i], degree=2), data=army), col=colors[i],xlab="Month",ylab="Drawing")
legend(40, -60, c("span = 2/3", "span = 1/3", "span = 1/10"), lty=1, col=c("blue", "red", "purple"))

# model validation and assumptions
lowess<-loess(Draft_No. ~ Mo.Number, span=2/3, degree=2)
par(mfrow=c(1,2))
#check resids
qqnorm(residuals(lowess)) 
qqline(residuals(lowess)) #funny tails and s-shaped
scatter.smooth(residuals(lowess), span = 1, degree = 1,xlab="Fitted Values",ylab="Residuals",main="Fitted vs. Residuals")


#make confidence interval
par(mfrow=c(1,1))
plot(Mo.Number, Draft_No., main="local quad. regression",xlab="Month",ylab="Drawing")
pred<-predict(loess(Draft_No. ~ Mo.Number, span=2/3, degree=2), se=TRUE)
lines(Mo.Number, predict(loess(Draft_No. ~ Mo.Number, span=2/3, degree=2), data=army), col=colors[i],xlab="Month",ylab="Drawing")
#add upper and lower bounds
lines(Mo.Number,pred$fit+2*pred$s, lty=2)
lines(Mo.Number,pred$fit-2*pred$s, lty=2)
#men born in the last months seem ben drawn less often than men born in the earlier montc
#of the year. the assumption is rejected.
