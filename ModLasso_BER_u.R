library(readr)
library(dplyr)
library(tidyverse)
library(glmnet)

data <- subset(data_stud, select = -X)
head(data)

data = as.data.frame(data)

#==============================Focus sur la ville de Bern =====================================
#==============================================================================================

data_bern = subset(data, station == "BER", select = c(station, datetime, U_10M_mean, V_10M_mean, U_10M, V_10M, u10, v10))
head(data_bern)
attach(data_bern)

nrow(data_bern)
nrow(data)

#Création d'un modèle lasso pour la variable u avec cross-validation pour 10 folds

u_x <- matrix(c(U_10M_mean, V_10M_mean,U_10M, V_10M), nrow = nrow(data_bern), ncol = 4, dimnames = list(1:nrow(data_bern), c("U_10M_mean", "V_10M_mean","U_10M", "V_10M")))
u_bern_cv.lasso <- cv.glmnet(u_x, u10, alpha = 1)

#==========Modèle avec lambda.min, calcul de l'erreur==========

u_bern_cv.lasso$lambda.min
model1 <- glmnet(u_x, u10, alpha = 1, lambda = u_bern_cv.lasso$lambda.min)
coef(model1)

ind<- which(u_bern_cv.lasso$lambda == u_bern_cv.lasso$lambda.min)
u_err_min<-u_bern_cv.lasso$cvm[ind]
u_err_min

#==========Modèle avec lambda.1se, calcul de l'erreur==========

u_bern_cv.lasso$lambda.1se
model2 <- glmnet(u_x, u10, alpha = 1, lambda = u_bern_cv.lasso$lambda.1se)
coef(model2)

ind<- which(u_bern_cv.lasso$lambda == u_bern_cv.lasso$lambda.1se)
u_err_1se<-u_bern_cv.lasso$cvm[ind]
u_err_1se

#==========Choix du modèle avec l'erreur minimale==========

u_err <- min(u_err_min, u_err_1se)
u_err

#On choisit lambda.min
