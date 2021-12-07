library(forecast)
library(tseries)

data <- subset(data_stud, select = -X)
data = as.data.frame(data)

#==============================Focus sur la ville de La Brevine =====================================
#==============================================================================================

data_brl= subset(data, station == "BRL", select = c(station, datetime, U_10M_mean, V_10M_mean, U_10M, V_10M, u10, v10))
head(data_brl)
attach(data_brl)

#====================Analyse the data BRL====================

pairs(data_brl[,-c(1,2)])

#====================Modèle ARIMA====================

#====================First we are looking for stationarity====================

#====================ARIMA(7,0,0)====================

plot(u10, type="l", ylab="Value of the wind u")
#not really stationary
acf(u10) 
#this suggest MA(0)
pacf(u10)
#this suggest AR(4) or AR(5) or AR(7)


#try around ARIMA(7,0,0), first intuition but not the best model : AIC = 8929.86
(fit1 <-arima(u10, order = c(7,0,0), xreg = cbind(U_10M_mean, V_10M_mean)))

checkresiduals(fit1)
#pacf(fit1$residuals)
qqnorm(fit1$residuals)
cpgram(fit1$residuals)

pred1 <- predict(fit1, newxreg = cbind(U_10M_mean,  V_10M_mean))

u_10 = c(rep(0,length(u10)), u10)
plot(u_10, type = "l",  main="Prediction with ARIMA(7,0,0)",xlab="Time", ylab="Value of the wind u", xlim=c(5112,10224), ylim=c(-2,12), col="darkolivegreen1")
lines(pred1$pred,col="red")

#Compute the mean square error and mean absolute error
err1 <- c(0,0)
for (i in 1:5112) {
  err1[1] <- err1[1] + (u10[i]-pred1$pred[i])^2
  err1[2] <- err1[2] + abs(u10[i]-pred1$pred[i])
}
err1[1] <- sqrt(err1[1]/5112)
err1[2] <- err1[2]/5112
err1[1]
err1[2]


#====================ARIMA(4,0,3)====================

#more stationary :
plot(diff(u10),ylab='Differenced u10')
acf(diff(u10))
#this suggest MA(1) or MA(2) or MA(3)
pacf(diff(u10))
#this suggest AR(4)

#We try this model which has smaller aic around ARIMA(4,1,3), AIC = 8931.22 :
(fit2 <-arima(u10, order = c(4,0,3), xreg = cbind(U_10M_mean,  V_10M_mean, U_10M, V_10M)))

#now if we use only umean and vmean, AIC = 8929.61
(fit2 <-arima(u10, order = c(4,0,3), xreg = cbind(U_10M_mean,  V_10M_mean)))

checkresiduals(fit2)
#pacf(fit2$residuals)
qqnorm(fit2$residuals)
cpgram(fit2$residuals)

pred2 <- predict(fit2, newxreg = cbind(U_10M_mean,  V_10M_mean))

plot(u_10, type = "l",  main="Prediction with ARIMA(4,0,3)",xlab="Time", ylab="Value of the wind u", xlim=c(5112,10224), ylim=c(-2,12), col="darkolivegreen1")
lines(pred2$pred,col="red")


#Compute the mean square error and mean absolute error
err2 <- c(0,0)
for (i in 1:5112) {
  err2[1] <- err2[1] + (u10[i]-pred2$pred[i])^2
  err2[2] <- err2[2] + abs(u10[i]-pred2$pred[i])
}
err2[1] <- sqrt(err2[1]/5112)
err2[2] <- err2[2]/5112
err2[1]
err2[2]


#Compare prediction 1 and 2
plot(pred1$pred, col="red")
lines(pred2$pred, col="green")
plot(pred1$pred, pred2$pred)


#====================ARIMA(1,1,1)====================

#We use the function auto.arima returns best ARIMA
auto.arima(u10)
#this suggest ARIMA(1,1,1), AIC = 8998.14
(fit3 <-arima(u10, order = c(1,1,1), xreg = cbind (U_10M_mean, V_10M_mean)))

checkresiduals(fit3)
#pacf(fit3$residuals)
qqnorm(fit3$residuals)
cpgram(fit3$residuals)

pred3 <- predict(fit3, newxreg = cbind(U_10M_mean,  V_10M_mean))

plot(u_10, type = "l",  main="Prediction with ARIMA(1,1,1)",xlab="Time", ylab="Value of the wind u", xlim=c(5112,10224), ylim=c(-2,12), col="darkolivegreen1")
lines(pred3$pred,col="red")


#Compute the mean square error and mean absolute error
err3 <- c(0,0)
for (i in 1:5112) {
  err3[1] <- err3[1] + (u10[i]-pred3$pred[i])^2
  err3[2] <- err3[2] + abs(u10[i]-pred3$pred[i])
}
err3[1] <- sqrt(err3[1]/5112)
err3[2] <- err3[2]/5112
err3[1]
err3[2]


#Compare prediction 1, 2 and 3
plot(pred1$pred, col="red")
lines(pred2$pred, col="green")
lines(pred3$pred, col="blue")
plot(pred1$pred, pred3$pred)
