#Ridge And Lasso

library(glmnet)

library(ISLR)
data("Hitters")
Hitters1<-data.frame(Hitters)
Hitters1<-na.omit(Hitters1)
dim(Hitters1)
str(Hitters1)

#Input design matrix, gives converted and dummy variable created
x<-model.matrix(Salary~.,Hitters1)
head(x)
str(x)
x<-x[,-1]
y<-Hitters1$Salary
head(y)


#use tuning parameter for lambda, use log, R expects from bigger to smaller, grid is lambda
#1, 10,100, 1000, 10000, 1000000
grid<-10^seq(from=10, to=-2, length.out = 100)
head(grid)
tail(grid)

#alpha=0, for Ridge and alpha=1 for lasso
ridge.model<-glmnet(x,y,alpha=0, lambda=grid)
ridge.model
coef(ridge.model)
dim(ridge.model)
lambda50<-ridge.model$lambda[50]
beta50<-coef(ridge.model)[,50]

#check as Lambda increases beta decreases
lambda<-ridge.model$lambda[1,43:56]
lambda
beta<-coef(ridge.model)[c(2:5),43:56]
beta
rbind(lambda,beta)


#training and test sets, nrows(Hitters1), gives the row index
set.seed(1)
train<-sample(1:length(y), 2/3*length(y))# gives the row index
test<--train# gives the row index

ridge.model<-glmnet(x[train,],y[train],alpha=0,lambda=grid)
ridge.pred.1<-predict(ridge.model, s=1, newx=x[test,])
error1<-mean((y[test]-ridge.pred.1)^2)

ridge.pred.5<-predict(ridge.model, s=5, newx=x[test,])
error5<-mean((y[test]-ridge.pred.5)^2)

ridge.pred.10<-predict(ridge.model, s=10, newx=x[test,])
error10<-mean((y[test]-ridge.pred.10)^2)

ridge.pred.50<-predict(ridge.model, s=50, newx=x[test,])
error50<-mean((y[test]-ridge.pred.50)^2)

ridge.pred.100<-predict(ridge.model, s=100, newx=x[test,])
error100<-mean((y[test]-ridge.pred.100)^2)

ridge.pred.200<-predict(ridge.model, s=200, newx=x[test,])
error200<-mean((y[test]-ridge.pred.200)^2)

ridge.pred.500<-predict(ridge.model, s=500, newx=x[test,])
error500<-mean((y[test]-ridge.pred.500)^2)

ridge.pred.1000<-predict(ridge.model, s=1000, newx=x[test,])
error1000<-mean((y[test]-ridge.pred.1000)^2)

ridge.pred.2000<-predict(ridge.model, s=2000, newx=x[test,])
error2000<-mean((y[test]-ridge.pred.2000)^2)

ridge.pred.5000<-predict(ridge.model, s=5000, newx=x[test,])
error5000<-mean((y[test]-ridge.pred.5000)^2)

ridge.pred.20000<-predict(ridge.model, s=20000, newx=x[test,])
error20000<-mean((y[test]-ridge.pred.20000)^2)

errorvector<-c(error1,error5,error10,error50,error100,error200,error500,error1000,error2000,error5000,error20000)
errorvector

logvalues<-c(log10(1),log10(5),log10(10),log10(50),log10(100),log10(200),log10(500),log10(1000),log10(2000),log10(5000),log10(20000))
plot(logvalues,errorvector)
log(5000)
log(1)
log(5,base=10)
log(5000,base=10)




set.seed(1)

#cv.mod<-cv.glmnet(x[train,],y[train],alpha=0,lambda=grid)
#bestlambda<-cv.mod$lambda.min
#bestlambda
cv.mod1<-cv.glmnet(x[train,],y[train],alpha=0)
bestlambda1<-cv.mod1$lambda.min
bestlambda1

pred1<-predict(cv.mod1,s=bestlambda1,newx=x[test,])

error<-mean((y[test]-pred1)^2)
error


cv.mod2<-cv.glmnet(x[train,],y[train],alpha=1)
bestlambda2<-cv.mod1$lambda.min
bestlambda2

pred2<-predict(cv.mod2,s=bestlambda2,newx=x[test,])

error2<-mean((y[test]-pred1)^2)
error2

coef(cv.mod2)
lasso.model<-glmnet(x,y,alpha=1, lambda=grid)
beta<-coef(lasso.model)[,30:50]
beta

plot(ridge.model)
abline(v=bestlambda1)
plot(lasso.model)
