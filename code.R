library(readr)
library(dplyr)
library(AICcmodavg)
library(tidyverse)

data <- subset(data_stud, select = -X1)
head(data)

data = as.data.frame(data)

data_bern = subset(data, station == "BER", select = c(station, datetime, U_10M_mean, V_10M_mean, U_10M, V_10M, u10, v10))
head(data_bern)

nrow(data_bern)
nrow(data)

#Création d'un modèle linéaire à ajout de variable "stepforward"

#======================1 variables ============================================

u_bern.fit0 <- lm(u10 ~ 1, data_bern)
u_bern.fit1 <- lm(u10 ~ U_10M_mean, data_bern)
u_bern.fit2 <- lm(u10 ~ V_10M_mean, data_bern)
u_bern.fit3 <- lm(u10 ~ U_10M, data_bern)
u_bern.fit4 <- lm(u10 ~ V_10M, data_bern)

models <- list(u_bern.fit0, u_bern.fit1, u_bern.fit2, u_bern.fit3, u_bern.fit4)
model.names <- c('const.mod', 'u_mean.mod', 'v_mean.mod', 'u.mod', 'v.mod')

aictab(cand.set = models, modnames = model.names)

#================2 variables ============================================

u_bern.fit5 <- lm(u10 ~ U_10M_mean + V_10M_mean, data_bern)
u_bern.fit6 <- lm(u10 ~ U_10M_mean + U_10M, data_bern)
u_bern.fit7 <- lm(u10 ~ U_10M_mean + V_10M, data_bern)

models <- list(u_bern.fit1, u_bern.fit5, u_bern.fit6, u_bern.fit7)
model.names <- c('u_mean.mod', 'u_mean/v_mean.mod', 'u_mean/u.mod', 'u_mean/v.mod')

aictab(cand.set = models, modnames = model.names)

#===========================3 variables =======================================

u_bern.fit8 <- lm(u10 ~ U_10M_mean + V_10M_mean + U_10M, data_bern)
u_bern.fit9 <- lm(u10 ~ U_10M_mean + V_10M_mean + V_10M, data_bern)

models <- list(u_bern.fit5, u_bern.fit8, u_bern.fit9)
model.names <- c('old.mod', 'add_u.mod', 'add_v.mod')

aictab(cand.set = models, modnames = model.names)

#=============================4 variables =====================================

u_bern.fit10 <- lm(u10 ~ U_10M_mean + V_10M_mean + U_10M + V_10M, data_bern)

models <- list(u_bern.fit8, u_bern.fit10)
model.names <- c('old.mod', 'add_v.mod')

aictab(cand.set = models, modnames = model.names)


