data_brl_test = data_brl

u_passe = rep(0, 5112)
for (i in 2:5112) {
  u_passe[i] = data_brl_test$u10[i-1]
}

data_brl_test = mutate(data_brl_test, u_passe = u_passe)
glm.fin = glm(u10 ~ U_10M_mean + V_10M_mean + U_10M + V_10M + u_passe, data = data_brl_test)
cv.err_fin <- cv.glm(data_brl_test, glm.fin)
cv.err_fin$delta

glm.fin

u_predit = u_passe

for (j in 3:5112) {
  u_predit[j] = predict(glm.fin, data_brl_test[(j-1),])
  data_brl_test = mutate(data_brl_test, u_passe = u_predit)
}

data_brl_test = data_brl_test[-1,]
nrow(data_brl_test)

cor(data_brl_test[,-(1:2)])

residu = rep(0, 5112)
for (k in 2:5111) {
  residu[k] = data_brl_test$u10[k] - data_brl_test$u_passe[k+1]
}
qqnorm(residu)