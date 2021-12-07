library(forecast)
library(tseries)

data <- subset(data_stud, select = -X)
data = as.data.frame(data)

#==============================Focus sur la ville de La Brevine =====================================
#==============================================================================================

data_brl= subset(data, station == "BRL", select = c(station, datetime, U_10M_mean, V_10M_mean, U_10M, V_10M, u10, v10))
attach(data_brl)

#====================Modèle ARIMA====================

#====================Best model ARIMA(1,1,2)====================

#We use the function auto.arima returns best ARIMA
auto.arima(v10)
#this suggest ARIMA(1,1,2), AIC = 12208.17 and with xreg, AIC = 11924.1
(fit4 <-arima(v10, order = c(1,1,2), xreg = cbind (U_10M_mean, V_10M_mean, U_10M, V_10M)))

checkresiduals(fit4)
#pacf(fit4$residuals)
qqnorm(fit4$residuals)
cpgram(fit4$residuals)

pred4 <- predict(fit4, newxreg = cbind(U_10M_mean, V_10M_mean, U_10M, V_10M))

v_10 = c(rep(0,length(v10)), v10)
plot(v_10, type = "l",  main="Prediction with ARIMA(1,1,2)",xlab="Time", ylab="Value of the wind v", xlim=c(5112,10224), ylim=c(-2,12), col="darkolivegreen1")
lines(pred4$pred,col="red")


#Compute the mean square error and mean absolute error
err4 <- c(0,0)
for (i in 1:5112) {
  err4[1] <- err4[1] + (v10[i]-pred4$pred[i])^2
  err4[2] <- err4[2] + abs(v10[i]-pred4$pred[i])
}
err4[1] <- sqrt(err4[1]/5112)
err4[2] <- err4[2]/5112
err4[1]
err4[2]

#====================Same model as u ARIMA(1,1,1)====================

#this suggest ARIMA(1,1,1), AIC = 11929.05
(fit5 <-arima(v10, order = c(1,1,1), xreg = cbind (U_10M_mean, V_10M_mean)))

checkresiduals(fit5)
#pacf(fit5$residuals)
qqnorm(fit5$residuals)
cpgram(fit5$residuals)

pred5 <- predict(fit5, newxreg = cbind(U_10M_mean,  V_10M_mean))

plot(v_10, type = "l",  main="Prediction with ARIMA(1,1,1)",xlab="Time", ylab="Value of the wind v", xlim=c(5112,10224), ylim=c(-2,12), col="darkolivegreen1")
lines(pred5$pred,col="red")


#Compute the mean square error and mean absolute error
err5 <- c(0,0)
for (i in 1:5112) {
  err5[1] <- err5[1] + (v10[i]-pred5$pred[i])^2
  err5[2] <- err5[2] + abs(v10[i]-pred5$pred[i])
}
err5[1] <- sqrt(err5[1]/5112)
err5[2] <- err5[2]/5112
err5[1]
err5[2]


#Compare prediction 4 and 5
plot(pred4$pred, col="red")
lines(pred5$pred, col="green")
plot(pred4$pred, pred5$pred)


plot(u10,v10,  main="Prediction with ARIMA(1,1,1)",xlab="Value of the wind u", ylab="Value of the wind v", col="darkolivegreen1", ylim=c(-1,16))
points(pred3$pred, pred5$pred, col="red")
