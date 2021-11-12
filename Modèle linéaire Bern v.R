library(readr)
library(dplyr)
library(AICcmodavg)
library(tidyverse)

data <- subset(data_stud, select = -X1)
head(data)

data = as.data.frame(data)

#==============================Focus sur la ville de Bern =====================================
#==============================================================================================

data_bern = subset(data, station == "BER", select = c(station, datetime, U_10M_mean, V_10M_mean, U_10M, V_10M, u10, v10))
head(data_bern)

nrow(data_bern)
nrow(data)

#Création d'un modèle linéaire à ajout de variable "stepforward" pour la variable v

#======================1 variables ============================================

v_bern.fit0 <- lm(v10 ~ 1, data_bern)
v_bern.fit1 <- lm(v10 ~ U_10M_mean, data_bern)
v_bern.fit2 <- lm(v10 ~ V_10M_mean, data_bern)
v_bern.fit3 <- lm(v10 ~ U_10M, data_bern)
v_bern.fit4 <- lm(v10 ~ V_10M, data_bern)

models <- list(v_bern.fit0, v_bern.fit1, v_bern.fit2, v_bern.fit3, v_bern.fit4)
model.names <- c('const.mod', 'u_mean.mod', 'v_mean.mod', 'u.mod', 'v.mod')

aictab(cand.set = models, modnames = model.names)

#================2 variables ============================================

v_bern.fit5 <- lm(v10 ~ V_10M_mean + U_10M_mean, data_bern)
v_bern.fit6 <- lm(v10 ~ V_10M_mean + U_10M, data_bern)
v_bern.fit7 <- lm(v10 ~ V_10M_mean + V_10M, data_bern)

models <- list(v_bern.fit2, v_bern.fit5, v_bern.fit6, v_bern.fit7)
model.names <- c('v_mean.mod', 'v_mean/u_mean.mod', 'v_mean/u.mod', 'v_mean/v.mod')

aictab(cand.set = models, modnames = model.names)

#===========================3 variables =======================================

v_bern.fit8 <- lm(v10 ~ U_10M_mean + V_10M_mean + U_10M, data_bern)
v_bern.fit9 <- lm(v10 ~ U_10M_mean + V_10M_mean + V_10M, data_bern)

models <- list(v_bern.fit5, v_bern.fit8, v_bern.fit9)
model.names <- c('old.mod', 'add_u.mod', 'add_v.mod')

aictab(cand.set = models, modnames = model.names)

#=============================4 variables =====================================

v_bern.fit10 <- lm(v10 ~ U_10M_mean + V_10M_mean + U_10M + V_10M, data_bern)

models <- list(v_bern.fit8, v_bern.fit10)
model.names <- c('old.mod', 'add_v.mod')

aictab(cand.set = models, modnames = model.names)


