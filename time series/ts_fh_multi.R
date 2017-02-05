rm(list=ls())

setwd("C:/Users/frauke/Dropbox/1e/adv. time series/project")

library("Hmisc")
library("readxl")
library("CADFtest")
library("forecast")
library("Metrics")
library("lmtest")
library("vars")
library("urca")

# MULTIVARIATE MODEL

iron_earn <- read_excel("iron_earn.xlsx")
names(iron_earn)

par(mfrow=c(1,1))

# Declare TS
iron_ts <- ts(iron_earn$iron, frequency = 4, start = c(1960,2))
earn_ts <- ts(iron_earn$earn_av, frequency = 4, start = c(1960,2))


# Plot 
par(mfrow = c(1,1))
plot.ts(iron_ts, ylab = "Iron", main = "Quarterly Iron Production, AUS, 1956 - 1991")
plot.ts(earn_ts, ylab = "Earnings", main ="Quarterly Earnings, AUS, 1956 - 1991")


par(mfrow = c(1,1))
ts.plot(iron_ts, earn_ts, col = c("blue", "black"), ylab = "Iron and Earnings", main = "Plot of Time Series")


# Dickey-Fuller unit root test to test for stationarity IRON
sqrt(length(iron_ts)) # 11.87434 max. nr. of lags 
CADFtest(iron_ts, type ="trend", criterion="BIC", max.lag.y=12) # H0 non stationary

# Dickey-Fuller unit root test to test for stationarity EARN
sqrt(length(earn_ts)) # 11.87434, max. nr. of lags 
CADFtest(earn_ts, type ="trend", criterion="BIC", max.lag.y=12) # H0. non stationary
CADFtest(earn_ts, type ="drift", criterion="BIC", max.lag.y=12) # H0. non stationary


# Make them stationary
# Stationarity - unit root
par(mfrow=c(2,1))

monthplot(iron_ts, ylab = "Iron", main = "Quarterly Iron Production, AUS, 1956 - 1991")
diron_ts <- diff(log(iron_ts), lag = 1)
monthplot(diron_ts, ylab = "Iron", main = "Seasonal Diff Iron Prod., AUS, 1956 - 1991")

monthplot(earn_ts, ylab = "Earnings", main ="Quarterly Av. Earnings, AUS, 1956 - 1991")
dearn_ts <- diff(log(earn_ts), lag = 1)
monthplot(earn_ts, ylab = "Earnings", main ="Seasonal Diff in Earnings, AUS, 1956 - 1991")

dearn2_ts <- diff(dearn_ts, lag = 1)

par(mfrow=c(2,1))
plot.ts(diron_ts)
plot.ts(dearn_ts)

# Dickey-Fuller
CADFtest(diron_ts, type = "drift", criterion = "BIC", max.lag.y = 12) #stationary
CADFtest(dearn_ts, type = "none", criterion = "BIC", max.lag.y = 12) #stationary

# Regression

fit_lm <- lm(dearn_ts ~ diron_ts) #because these are both stationary time series
summary(fit_lm)


# ADLM Models

lag <- 5
n <- length(diron_ts)
diron_ts.0 <- diron_ts[(lag+1):(n)]
diron_ts.0 <- dearn_ts[(lag+1):(n)]
diron_ts.1 <- dearn_ts[(lag):(n-1)]
diron_ts.2 <- dearn_ts[(lag-1):(n-2)]
diron_ts.3 <- dearn_ts[(lag-2):(n-3)]
diron_ts.4 <- dearn_ts[(lag-3):(n-4)]
diron_ts.5 <- dearn_ts[(lag-4):(n-5)]

lag <- 4
diron_ts.0 <- diron_ts[(lag+1):(n)]
dearn_ts.0 <- dearn_ts[(lag+1):(n)]
diron_ts.1 <- diron_ts[(lag):(n-1)]
dearn_ts.1 <- dearn_ts[(lag):(n-1)]
diron_ts.2 <- diron_ts[(lag-1):(n-2)]
dearn_ts.2 <- dearn_ts[(lag-1):(n-2)]
diron_ts.3 <- diron_ts[(lag-2):(n-3)]
dearn_ts.3 <- dearn_ts[(lag-2):(n-3)]
diron_ts.4 <- diron_ts[(lag-3):(n-4)]
dearn_ts.4 <- dearn_ts[(lag-3):(n-4)]

#adlm diron
adlm1.1 <- lm(diron_ts.0 ~ diron_ts.1 + dearn_ts.1)
adlm1.2 <- lm(diron_ts.0 ~ diron_ts.1 + diron_ts.2 + dearn_ts.1 + dearn_ts.2)

Box.test(adlm1.2$residuals, lag = 18, type="Ljung-Box") # p-value = 0.5052, white noise, valid regression

adlm1.3 <- lm(diron_ts.0 ~ diron_ts.1 + diron_ts.2 + diron_ts.3 + dearn_ts.1 + dearn_ts.2 + dearn_ts.3)
adlm1.4 <- lm(diron_ts.0 ~ diron_ts.1 + diron_ts.2 + diron_ts.3 + diron_ts.4 + dearn_ts.1 + dearn_ts.2 + dearn_ts.3 + dearn_ts.4)

Box.test(adlm1.4$residuals, lag = 18, type="Ljung-Box") # p-value = 0.9758, white noise, valid regression


#adlm dearn
adlm2.1 <- lm(dearn_ts.0 ~ diron_ts.1 + dearn_ts.1)
adlm2.2 <- lm(dearn_ts.0 ~ diron_ts.1 + diron_ts.2 + diron_ts.1 + diron_ts.2)

Box.test(adlm2.2$residuals, lag = 18, type="Ljung-Box") # p-value < 2.2e-16, not white noise 

adlm2.3 <- lm(dearn_ts.0 ~ diron_ts.1 + diron_ts.2 + diron_ts.3 + dearn_ts.1 + dearn_ts.2 + dearn_ts.3)
adlm2.4 <- lm(dearn_ts.0 ~ diron_ts.1 + diron_ts.2 + diron_ts.3 + diron_ts.4 + dearn_ts.1 + dearn_ts.2 + dearn_ts.3 + dearn_ts.4)

Box.test(adlm2.4$residuals, lag = 18, type="Ljung-Box") # p-value < 0.1944 noise 


#BIC
AIC(adlm1.1, k = log(140)) # -261.2871
AIC(adlm1.2, k = log(140)) # -262.1256 #
AIC(adlm1.3, k = log(140)) # -260.5113
AIC(adlm1.4, k = log(140)) # -251.5674

AIC(adlm2.1, k = log(140)) # -494.8483
AIC(adlm2.2, k = log(140)) # -431.5044
AIC(adlm2.3, k = log(140)) # -575.7394
AIC(adlm2.4, k = log(140)) # -627.7123 #



# Granger test
grangertest(diron_ts ~ dearn_ts, order = 2) # significant (borderline)
grangertest(dearn_ts ~ diron_ts, order = 4) # not significant

# => weak evidence for granger causalitiy: iron weakly granger causes male earnings


# Co-integration

fit_ts <- lm(iron_ts[-1] ~ dearn_ts)
fit_ts_res <- fit_ts$residuals
plot(fit_ts$residuals)

CADFtest(fit_ts_res , type = "none", criterion = "BIC", max.lag.y = 12) 
#p-value = 0.04418, cointegration

fit_ts2 <- lm(dearn_ts ~ iron_ts[-1])
fit_ts_res2 <- fit_ts2$residuals
plot(fit_ts2$residuals)

CADFtest(fit_ts_res2, type = "none", criterion = "BIC", max.lag.y = 12) 
#p-value = 0.04418, cointegration



# VAR MODEL

VARselect(cbind(diron_ts, dearn_ts), lag.max = 12, type = "const")

# var model fit
var <- cbind(iron_ts[-1], dearn_ts)
fit_var1 <- VAR(var, p = 2) 
summary(fit_var1)
# iron on lags if iron and earning, none of the lags of earnings are sig. but 2 lags of iron are sig
# => earnings dont grager cause iron
# earnings on earnings, sig. ,Adjusted R-squared: 0.9096 , ok, Adjusted R-squared: 0.4125 , ok, F test for joint sig, really high, Matrix covariance
plot(fit_var)

fit_var2 <- VAR(var, p = 3)
summary(fit_var2)

fit_var3 <- VAR(var, p = 4)
summary(fit_var)

# PACF does not look like white noise

var_fit_resid <- resid(fit_var1)
var_fit_resid3 <- resid(fit_var3)

par(mfrow=c(1,3))
acf(var_fit_resid[,1])
acf(var_fit_resid[,2])
pacf(var_fit_resid[,1])
pacf(var_fit_resid[,2])

ccf(var_fit_resid[,1], var_fit_resid[,2], )
# cannot validate my model, since no white noise residuals as shown by the graph

par(mfrow=c(1,3))
acf(var_fit_resid3[,1])
acf(var_fit_resid3[,2])
ccf(var_fit_resid3[,1], var_fit_resid3[,2])

# impulse response function
irf_var <- irf(fit_var, ortho = F, boot = T, n.ahead = 12)
plot(irf_var)

# eam dynamics: if one changes by one unit what happens to the other one
# => no change 
# not really same trend, but then again not really cointegrated 

# Portmanteau Test (adjusted): test for white noise, check!
var_test <- serial.test(fit_var, lags.pt = 12, type = "PT.adjusted") # p-value < 2.2e-16, not white noise

# Breusch-Godfrey LM test
var_test2 <- serial.test(fit_var,lags.pt = 12, type = "BG") # p-value < 2.2e-16

#predict
var_predict <- predict(fit_var, n.ahead = 24)
plot(var_predict)

# VECM

# Johansen Test for cointegration
trace <- ca.jo(var, type = "trace", K = 2, ecdet = "const", spec = "transitory")
summary(trace)
# Values of teststatistic and critical values of test: what is the critical value?

max_eigen <- ca.jo(var, type = "eigen", K = 2, ecdet = "const", spec = "transitory")
summary(max_eigen)

vecm_trace <- cajorls(trace, r = 1)
vecm_trace
vecm_eigen <- cajorls(max_eigen, r = 1)
vecm_eigen
summary(vecm_eigen)

fit_vecm <- vec2var(max_eigen, r = 1)
summary(fit_vecm)
res <- resid(fit_vecm)
acf(res)
vecm_predict <- predict(fit_vecm, n.ahead = 24, ci = 0.95)

fct <- vecm_predict$fcst
fct_dearn <- fct$dearn_ts

ts.plot(fct_dearn)
plot(vecm_predict)
