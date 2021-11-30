library(forecast)
library(tseries)

data <- subset(data_stud, select = -X)
data = as.data.frame(data)

#==============================Focus sur la ville de La Brevine =====================================
#==============================================================================================

data_brl_u10 = subset(data, station == "BRL", select = c(station, datetime, U_10M_mean, V_10M_mean, U_10M, V_10M, u10, v10))
head(data_brl)
attach(data_brl)

#==========Analyse the data BRL==========

pairs(data_brl[,-c(1,2)])

#==========Modèle ARIMA==========

#==========First we are looking for stationarity==========
plot(u10)
#not really stationary
acf(u10) 
#this suggest MA(0)
pacf(u10)
#this suggest AR(4) or AR(5) or AR(7)

#try around ARIMA(7,0,0), first intuition but not the best model : AIC = 8931.24
(fit1 <-arima(u10, order = c(7,0,0), xreg = cbind(U_10M_mean, V_10M_mean, U_10M, V_10M)))

checkresiduals(fit1)
acf(fit1$residuals)
pacf(fit1$residuals)

qqnorm(fit1$residuals)
cpgram(fit1$residuals)

autoplot(forecast(fit1))


#more stationary :
plot(diff(u10),ylab='Differenced u10')
acf(diff(u10))
#this suggest MA(1) or MA(2) or MA(3)
pacf(diff(u10))
#this suggest AR(4)

#or :
#plot(log10(u10), ylab='log(u10)')
#plot(diff(log10(u10)),ylab='Differenced Log (u10)')


#We try this model which has smaller aic around ARIMA(4,1,2), AIC = 8931.22 :
(fit2 <-arima(u10, order = c(4,0,3), xreg = cbind(U_10M_mean,  V_10M_mean, U_10M, V_10M)))

#now if we use only umean and vmean, AIC = 8929.61
(fit2 <-arima(u10, order = c(4,0,3), xreg = cbind(U_10M_mean,  V_10M_mean)))

checkresiduals(fit2)
acf(fit2$residuals)
pacf(fit2$residuals)
qqnorm(fit2$residuals)
cpgram(fit2$residuals)

autoplot(forecast(fit2))


#We use the function auto.arima returns best ARIMA
auto.arima(u10)
#this suggest ARIMA(1,1,1), AIC = 8998.14
(fit3 <-arima(u10, order = c(1,1,1), xreg = cbind(U_10M_mean, V_10M_mean, U_10M, V_10M)))

checkresiduals(fit3)
acf(fit3$residuals)
pacf(fit3$residuals)
qqnorm(fit3$residuals)
cpgram(fit3$residuals)

autoplot(forecast(fit3))
