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

nrow(data_bern)
nrow(data)

#Création d'un modèle lasso pour la variable v avec cross-validation pour 10 folds

v_x <- matrix(c(U_10M_mean, V_10M_mean,U_10M, V_10M), nrow = nrow(data_bern), ncol = 4, dimnames = list(1:nrow(data_bern), c("U_10M_mean", "V_10M_mean","U_10M", "V_10M")))
v_bern_cv.lasso <- cv.glmnet(v_x, v10, alpha = 1)

#==========Modèle avec lambda.min, calcul de l'erreur==========

v_bern_cv.lasso$lambda.min
model1 <- glmnet(v_x, v10, alpha = 1, lambda = v_bern_cv.lasso$lambda.min)
coef(model1)

ind<- which(v_bern_cv.lasso$lambda == v_bern_cv.lasso$lambda.min)
v_err_min<-v_bern_cv.lasso$cvm[ind]
v_err_min

#==========Modèle avec lambda.1se, calcul de l'erreur==========

v_bern_cv.lasso$lambda.1se
model2 <- glmnet(v_x, v10, alpha = 1, lambda = v_bern_cv.lasso$lambda.1se)
coef(model2)

ind<- which(v_bern_cv.lasso$lambda == v_bern_cv.lasso$lambda.1se)
v_err_1se<-v_bern_cv.lasso$cvm[ind]
v_err_1se

#==========Choix du modèle avec l'erreur minimale==========

v_err <- min(v_err_min, v_err_1se)
v_err

#On choisit lambda.min
