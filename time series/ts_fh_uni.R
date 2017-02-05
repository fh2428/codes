rm(list=ls())

setwd("C:/Users/frauke/Dropbox/1e/adv. time series/project")

library("Hmisc")
library("readxl")
library("CADFtest")
library("forecast")
library("Metrics")
library("fGarch")

# UNIVARIATE MODEL

consumption <- read_excel("gdpconsusa.xlsx")
attach(consumption)
consumption$CONS
describe(consumption$CONS)

par(mfrow=c(1,1))
cons_ts <- ts(CONS, frequency=4, start=c(1948,1))

plot.ts(cons_ts, ylab="Quarterly Consumption", main="Quarterly Consumption, USA, 1948 - 2001")

# white noise with drift: without drift it´s mean zero, has intercept

plot.ts(log(cons_ts), ylab="Quarterly Consumption", main="Quarterly Consumption, USA, 1948 - 2001")

plot(decompose(cons_ts))

# Dickey-Fuller unit root test to test for stationarity to trend
sqrt(length(cons_ts)) # 14.69694, max. nr. of lags 
CADFtest(cons_ts, type = "trend", criterion = "BIC", max.lag.y = 15) # RH0. stationary p-value < 2.2e-16

CADFtest(cons_ts, type = "drift", criterion = "BIC", max.lag.y = 15) # RH0. stationary p-value < 2.2e-16

# Seasonality 
par(mfrow=c(3,1))

monthplot(cons_ts, ylab = "Quarterly consumption", main = "Quarterly change in consumption, USA") 
dcons_ts <- diff(log(cons_ts), lag = 4)

monthplot(dcons_ts, ylab = "Quarterly consumption", main = "Quarterly change in consumption, USA")
plot.ts(dcons_ts, ylab = "Quarterly differenced consumption",main = "Seasonally differenced consumption, USA")

# Dickey-Fuller
CADFtest(dcons_ts, type = "trend", criterion = "BIC", max.lag.y = 15) # still stationary

# (P)ACF
par(mfrow = c(1,1))
acf(dcons_ts, main = "ACF of consumption, USA")  
pacf(dcons_ts, main = "PACF of consumption, USA") 

# independence, autocorrelation
Box.test(dcons_ts, lag = 15, type ="Ljung-Box") # superautocorrelation, p-value = 1.113e-10



# ARMA
# Find a model

auto.arima(dcons_ts, ic = c("aicc"), seasonal = T) # Best model: ARIMA(2,0,2)(0,0,2)[4] with non-zero mean 

# best model
fit_sarima <- arima(dcons_ts, order = c(2,0,2), seasonal = list(order = c(0,0,2), period = 4))
summary(fit_sarima)

acf(fit_sarima$residuals, main = "ACF for quarterly consumption")
pacf(fit_sarima$residuals, main = "PACF for quarterly consumption")
Box.test(fit_sarima$residuals, lag = 15, type = "Ljung-Box") #p-value = 0.2521: no autocorr

abs(fit_sarima$coef/sqrt(diag(fit_sarima$var.coef)))



# alternative 1
fit_sarima2 <- arima(dcons_ts, order = c(1,0,1), seasonal = list(order = c(2,1,1), period = 4))
summary(fit_sarima2)

par(mfrow = c(1,1))
acf(fit_sarima2$residuals, main = "ACF for quarterly consumption")
pacf(fit_sarima2$residuals, main = "ACF for quarterly consumption")
Box.test(fit_sarima2$residuals, lag = 15, type = "Ljung-Box") #p-value = 0.0006:still autocorr

# alternative 5: ARIMA(1,0,3)(1,1,2)
fit_sarima3 <- arima(dcons_ts, order = c(2,0,2), seasonal = list(order = c(0,0,1), period = 4))
summary(fit_sarima3)
acf(fit_sarima3$residuals)
pacf(fit_sarima3$residuals)
Box.test(fit_sarima3$residuals, lag = 15, type = "Ljung-Box") #p-value = 0.2389: no autocorr

# alternative 2: ARIMA(3,0,2)(0,0,2)
# alternative 3: ARIMA(2,0,2)(1,0,2)
# alternative 4: ARIMA(2,0,2)(0,0,1)

# GARCH
# fit a model without seasonality

auto.arima(dcons_ts, ic = c("aicc"), trace = T, seasonal = F)

# ARIMA(2,0,2) BIC=819.98

fit_arima <- arima(dcons_ts, order = c(2,0,2))
summary(fit_arima)
acf(fit_arima$residuals)
pacf(fit_arima$residuals)
Box.test(fit_arima$residuals, lag = 15, type = "Ljung-Box") 

# => since autocorrelation, need garch model
acf(fit_arima$residuals^2)

fit_garch <- garchFit(dcons_ts ~ arma(2,2) + garch(1,1), cond.dist = "QMLE")
summary(fit_garch)
plot(fit_garch)

# not normal residuals, additionally check summary of garch
# garch is not validated

# 10 and 11
# 12
# 13

# one or two more garch, but what gifs



# FORECAST


# NAIVE, for the next 4 quarters
# sarima1
fit_forecast <- arima(log(cons_ts), order = c(2,0,2), seasonal = list(order = c(0,1,2), period = 4))
fit_forecast

naive <- forecast.Arima(fit_forecast, level = c(95), h = 8)
naive

plot.forecast(naive, shaded = T, xlim = c(1993,2003), ylab = "Consumption", xlab = "Time", main = "Naive forecast for 4 quarters" )
naive_ts <- naive$mean
naive_ts

# interval gets wider because SE of prediction becoms bigger
# because errors get accumulated and variance of estimated prediction errors become larger but SE are valid because no serial corr in error terms
# the interval widens very quickly 

# sarima3
fit_forecast3 <- arima(log(cons_ts), order = c(2,0,2), seasonal = list(order = c(0,1,2)))
fit_forecast3

naive_sarima3 <- forecast.Arima(fit_forecast3, level = c(95), h = 8)
naive_sarima3

plot.forecast(naive_sarima3, shaded = T, xlim = c(1993, 2003), ylab = "Consumption", xlab = "Time", main = "Naive forecast for 4 quarters" )
naive_ts3 <- naive_sarima3$mean


# final plot
par(mfrow = c(1,1))
plot.forecast(naive_sarima3, shaded = T, col = "darkgreen", xlim = c(1993,2003), ylab = "consumption", xlab = "time", main = "Naive forecast SARIMA(2,0,2)(0,1,2)" )


# ROLLING WINDOW
# Sarima1
rolling <- c()
cons_ts_rolling <- log(cons_ts)
for(i in 1:8)
{
  fit_sarima_rolling <- arima(cons_ts_rolling, order = c(2,0,2), seasonal = c(0,1,2))
  prediction <- forecast.Arima(fit_sarima_rolling, level = c(95), h = 1)
  rolling[i] <- prediction$mean
  cons_ts_rolling <- c(cons_ts_rolling, rolling[i])
}


# declare forecasted values as time series
cons_sarima_rolling_ts <- ts(rolling, frequency = 4, start = c(2002,1))
plot.ts(cons_sarima_rolling_ts, main = "Rolling Window SARIMA(2,0,2)(0,1,2)")


# Sarima2
rolling3 <- c()
cons_ts_rolling3 <- log(cons_ts)
for(i in 1:8)
{
  fit_sarima_rolling3 <- arima(cons_ts_rolling3, order = c(1,0,3), seasonal = c(1,1,2))
  prediction3 <- forecast.Arima(fit_sarima_rolling3, level = c(95), h = 1)
  rolling3[i] <- prediction3$mean
  cons_ts_rolling3 <- c(cons_ts_rolling3, rolling3[i])
}

# declare forecasted values as time series
cons_sarima_rolling3_ts <- ts(rolling3, frequency = 4, start = c(2002,1))

ts.plot(cons_sarima_rolling3_ts) 

# plot the forecasts
plot.ts(cons_sarima_rolling_ts, main = "Rolling Window Forecast", ylab = "Quarterly consumption", xlab = "Time")


# COMPARE PERFORMANCE


par(mfrow=c(1,1))
cbind(naive_ts, cons_ts_rolling, log(cons_ts))
ts.plot(log(cons_ts), naive_ts, cons_sarima_rolling_ts, col=c("black","blue","red"), xlim=c(1993,2003), main = "Naive/Rolling Window SARIMA(2,0,2)(0,1,2)", ylab = "Quarterly consumption", xlab = "Time")

par(mfrow=c(1,1))


#PSEUDO OOS ROLLING WINDOW

#best model
S=106
rolling_pseudo <- c()
for(i in S:length(log(cons_ts))-1)
{
  
  fit_sarima_pseudo <- arima(log(cons_ts[1:i]), order = c(2,0,2), seasonal = c(0,1,2))
  prediction <- forecast.Arima(fit_sarima_pseudo, level = c(95), h = 1)
  rolling_pseudo[i+1] <- prediction$mean
}


#second best model
S=106
rolling_pseudo2 <- c()
for(i in S:length(log(cons_ts))-1)
{
  
  fit_sarima_pseudo2 <- arima(log(cons_ts[1:i]), order = c(2,0,2), seasonal = c(1,1,2))
  prediction2 <- forecast.Arima(fit_sarima_pseudo2, level = c(95), h = 1)
  rolling_pseudo2[i+1] <- prediction2$mean
}

# third best model
S=106
rolling_pseudo3 <- c()
for(i in S:length(log(cons_ts))-1)
{
  
  fit_sarima_pseudo3 <- arima(log(cons_ts[1:i]), order = c(3,0,2), seasonal = c(0,1,2))
  prediction3 <- forecast.Arima(fit_sarima_pseudo3, level = c(95), h = 1)
  rolling_pseudo3[i+1] <- prediction3$mean
}

# declare these values as time series
rolling_pseudo_ts <- ts(rolling_pseudo, frequency = 4, start = c(1949, 1))
rolling_pseudo_ts2 <- ts(rolling_pseudo2, frequency = 4, start = c(1949, 1))
rolling_pseudo_ts3 <- ts(rolling_pseudo3, frequency = 4, start = c(1949, 1))

# Remarks: forecasted values for the second half of consumption time series. comparing forecasted values with observed ones, 
#evaluate the forecasting and performance of the model.

#plot
ts.plot(log(cons_ts), rolling_pseudo_ts, col = c("black","blue"), ylab = "Consumption", main = "Pseudo OOS Rolling Window SARIMA(2,0,2)(0,0,2)")


# FORECASTING ERRORS

RMSE <- sqrt(mean((dcons_ts - rolling_pseudo_ts)^2, na.rm = TRUE))
MAE <- mean(abs(dcons_ts - rolling_pseudo_ts), na.rm = TRUE)
MAPE <- mean(abs((dcons_ts - rolling_pseudo_ts) / dcons_ts), na.rm= TRUE)*100

accuracy(rolling_pseudo_ts, log(cons_ts))
accuracy(rolling_pseudo_ts2, log(cons_ts))
accuracy(rolling_pseudo_ts3, log(cons_ts))


# plots
par(mfrow = c(3,1))

plot.forecast(naive_sarima3, shaded = T, col = "darkgreen", xlim = c(1993,2003), ylab = "consumption", xlab = "time", main = "Naive forecast SARIMA(2,0,2)(0,1,2)" )
ts.plot(log(cons_ts), naive_ts, cons_sarima_rolling_ts, col=c("black","blue","red"), xlim=c(1993,2003), main = "Naive/Rolling Window SARIMA(2,0,2)(0,1,2)", ylab = "Quarterly consumption", xlab = "Time")
plot.ts(cons_sarima_rolling_ts, main = "Rolling Window Forecast", ylab = "Quarterly consumption", xlab = "Time")
ts.plot(log(cons_ts), rolling_pseudo_ts, col = c("black","blue"), ylab = "Consumption", main = "Pseudo OOS Rolling Window SARIMA(2,0,2)(0,0,2)")

