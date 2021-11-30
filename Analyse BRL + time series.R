library(readr)
library(dplyr)
library(tidyverse)
library(glmnet)
library(lubridate)
library(ggplot2)
library(boot)

data <- subset(data_stud, select = -X)
data = as.data.frame(data)


#==============================Focus sur la ville de La Brevine =====================================

data_brl = subset(data, station == "BRL", select = c(station, datetime, U_10M_mean, V_10M_mean, U_10M, V_10M, u10, v10))
nrow(data_brl)

##===================Information sur sur les données==========================

#-5112 ligne de donnéee, ce qui représentent 213 jours de l'année sur 24 heures
#-Les mois présents sont septembre, octobre, novembre, décembre, janvier, février et mars

pairs(data_brl[,-(1:2)])

#-avec le graph, lorsqu'on regarde u10 en fonction de v10 ou réciproquement, on remarque 2 ensembles distincts


##===================Essayons de couper les données en 2, la journée de minuit à midi==================
limite = 12


data_brl_jour = subset(data_brl, hour(datetime) <= limite)
data_brl_nuit = subset(data_brl, hour(datetime) > limite)

plot(data_brl_jour$u10, data_brl_jour$v10,
     main="Comparaison jour versus nuit",
     xlab = "u10",
     ylab="v10",
     col="red")
points(data_brl_nuit$u10, data_brl_nuit$v10, col="blue")
legend("topleft",
       c("jour","nuit"),
       fill=c("red","blue"))

#=================Ca va être compliqué, on va se focus sur un mois=======================
mois = 9

data_brl_sep = subset(data_brl, month(datetime) == mois)

plot(data_brl_sep$u10, data_brl_sep$v10)

#=============0n se focus sur un jour, on plot en ligne pour voir la continuité================

data_brl_sep_10 = subset(data_brl_sep, day(datetime) == 10)
plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_10$u10, data_brl_sep_10$v10, col="red")
points(data_brl_sep_10$u10[1], data_brl_sep_10$v10[1], col="green")

data_brl_sep_1 = subset(data_brl_sep, day(datetime) == 1)
plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_1$u10, data_brl_sep_1$v10, col="red")

data_brl_sep_2 = subset(data_brl_sep, day(datetime) == 2)
plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_2$u10, data_brl_sep_2$v10, col="red")

data_brl_sep_3 = subset(data_brl_sep, day(datetime) == 3)
plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_3$u10, data_brl_sep_3$v10, col="red")

data_brl_sep_4 = subset(data_brl_sep, day(datetime) == 4)
plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_4$u10, data_brl_sep_4$v10, col="red")

data_brl_sep_5 = subset(data_brl_sep, day(datetime) == 5)
plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_5$u10, data_brl_sep_5$v10, col="red")

data_brl_sep_6 = subset(data_brl_sep, day(datetime) == 6)
plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_6$u10, data_brl_sep_6$v10, col="red")

data_brl_sep_7 = subset(data_brl_sep, day(datetime) == 7)
plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_7$u10, data_brl_sep_7$v10, col="red")

data_brl_sep_8 = subset(data_brl_sep, day(datetime) == 8)
plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_8$u10, data_brl_sep_8$v10, col="red")

data_brl_sep_9 = subset(data_brl_sep, day(datetime) == 9)
plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_9$u10, data_brl_sep_9$v10, col="red")

plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_10$u10, data_brl_sep_10$v10, col="red")

data_brl_sep_15 = subset(data_brl_sep, day(datetime) == 15)
plot(data_brl_sep$u10, data_brl_sep$v10)
lines(data_brl_sep_15$u10, data_brl_sep_15$v10, col="red")

##============================ Série temporelle ===================================

UM_m1 = rep(0, 5112)
VM_m1 = rep(0, 5112)
U_m1 = rep(0, 5112)
V_m1 = rep(0, 5112)
for (i in 2:5112) {
  UM_m1[i] = data_brl$U_10M_mean[i-1]
  VM_m1[i] = data_brl$V_10M_mean[i-1]
  U_m1[i] = data_brl$U_10M[i-1]
  V_m1[i] = data_brl$V_10M[i-1]
}

data_brl <- mutate(data_brl, UM_m1 = UM_m1, VM_m1 = VM_m1, U_m1 = U_m1, V_m1 = V_m1)

set.seed(1809)
glm.fit_m1 <- glm(u10 ~ U_10M_mean + V_10M_mean + U_10M + V_10M + UM_m1 + VM_m1 + U_m1 + V_m1, data = data_brl)
cv.err_m1 <- cv.glm(data_brl, glm.fit_m1)
cv.err_m1$delta

glm.fit <- glm(u10 ~ U_10M_mean + V_10M_mean + U_10M + V_10M, data = data_brl)
cv.err <- cv.glm(data_brl, glm.fit)
cv.err$delta

glm.fit_m1

#====En utilisant les variables précédentes, on améliore (un peu) le modèle, essayons maintenant d'utiliser la prédiction

u_p = predict(glm.fit, data_brl)
up_m1 = rep(0, 5112)
for (i in 2:5112) {
  up_m1[i] = u_p[i-1]
}
data_brl <- mutate(data_brl, up_m1 = up_m1)

glm.fit_p1 = glm(u10 ~ U_10M_mean + V_10M_mean + U_10M + V_10M + up_m1, data = data_brl)
cv.err_p1 <- cv.glm(data_brl, glm.fit_p1)
cv.err_p1$delta

residub = data_brl$u10 - u_p
qqnorm(residub)
acf(residub)

#=====================Utilisant de deux jours avant==========================
#Avec 2 heures avants
UM_m2 = rep(0, 5112)
VM_m2 = rep(0, 5112)
U_m2 = rep(0, 5112)
V_m2 = rep(0, 5112)
for (i in 3:5112) {
  UM_m2[i] = data_brl$U_10M_mean[i-2]
  VM_m2[i] = data_brl$V_10M_mean[i-2]
  U_m2[i] = data_brl$U_10M[i-2]
  V_m2[i] = data_brl$V_10M[i-2]
}

data_brl <- mutate(data_brl, UM_m2 = UM_m2, VM_m2 = VM_m2, U_m2 = U_m2, V_m2 = V_m2)
glm.fit_m2 <- glm(u10 ~ U_10M_mean + V_10M_mean + U_10M + V_10M + UM_m1 + VM_m1 + U_m1 + V_m1 +
                    UM_m2 + VM_m2 + U_m2 + V_m2, data = data_brl)
cv.err_m2 <- cv.glm(data_brl, glm.fit_m2)
cv.err_m2$delta








