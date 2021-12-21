#create X and y
set.seed(1809) 
n<-90
p<-1000
const<-0.95
X<-matrix(rnorm(n*p), nrow=n)
b<-const*c(rep(1,4), rep(0,p-4))
prob<-1/(1+exp(-X%*%b))
y<-(runif(n)<prob)

#create 2 samples : train and test
train.Index <- rbinom(nrow(X), 1, 0.15)
train.X <- X[train.Index==0,]
train.y <- y[train.Index==0]
test.X <- X[train.Index==1,]
test.y <- y[train.Index==1]


#lasso on the train sample
cv.lasso <- cv.glmnet(train.X, train.y, alpha = 1, family = "binomial")
min(cv.lasso$cvm)

model <- glmnet(train.X, train.y, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min)
head(coef(model))
coef(model)


model_simple <- glmnet(train.X, train.y, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.1se)
head(coef(model_simple))
coef(model_simple)



#compute the bayes
bayes<-0
y_es<-rep(0,n)

for (i in 1:n){
  y_es[i]<-(prob[i]>0.5)
  if(y[i]==y_es[i]){bayes<-bayes} else {bayes<-bayes+1}
}
bayes<-bayes/n



#lasso on the whole
cv.lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial")
#plot(cv.lasso)


model <- glmnet(X, y, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min)
cv.lasso$lambda.min
head(coef(model))
coef(model)

model_simple <- glmnet(X, y, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.1se)
head(coef(model_simple))
coef(model_simple)

#create 2 samples : train and test
erreur = matrix(0, nrow = 300, ncol = 2)
colnames(erreur)=c('True error','CV estimated error')
erreur_cv = rep(0, 5)

for(i in 1:300) {
  set.seed(i)
  index <- runif(nrow(X))
  X <- X[order(index),]
  y = y[order(index)]
  train.X <- X[19:90,]
  train.y <- y[19:90]
  test.X <- X[1:18,]
  test.y <- y[1:18]
  
  #lasso on the train sample
  cv.lasso <- cv.glmnet(train.X, train.y, alpha = 1, family = "binomial")
  
  model <- glmnet(train.X, train.y, family = "binomial", alpha = 1, standardize=FALSE, lambda = cv.lasso$lambda.min)
  
  for(j in 1:5) {
    dt_0 = ((j-1)*18)+1
    dt_1 = 18*j
    x_test = X[dt_0:dt_1,]
    x_train = X[-(dt_0:dt_1),]
    y_test = y[dt_0:dt_1]
    y_train = y[-(dt_0:dt_1)]
    cv.lasso_1 <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")
    model_cv <- glmnet(x_train, y_train, family = "binomial", alpha = 1, standardize=FALSE, lambda = cv.lasso_1$lambda.min)
    erreur_cv[j] <- sum(y_test != predict(model_cv, x_test, type="class")) /nrow(x_test)
  }
  
  erreur[i, 1] <- sum(test.y != predict(model, test.X, type="class")) /nrow(test.X)
  erreur[i, 2] <- sum(erreur_cv) / 5
  
}

erreur

plot(erreur)


x_train
