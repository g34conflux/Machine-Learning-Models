library(e1071)
set.seed(1)
x=matrix(rnorm(20*2),ncol=2)#sd is -3to +3
x
y=c(rep(-1,10),rep(1,10))
y

plot(x[,2],x[,1],col=(3-y))
x[y==1,]<-x[y==1,]+1
plot(x[,2],x[,1],col=(3-y))
dat=data.frame(x=x,y=as.factor(y))
dat

svmfit=svm(y~.,data=dat,kernel='linear',cost=10,scale=FALSE)
#High cost meand low budget, less points will cross the margin

plot(svmfit,dat)
#7 x's in [plot] are the support vectors, and the plot shows only the hyperplane
svmfit$index
summary(svmfit) #gamma is 1/no of parameters, its mostly for non linear no of support vectors in each class

svmfit1=svm(y~.,data=dat,kernel='linear',cost=0.1,scale=FALSE)
#High cost meand low budget, less points will cross the margin

plot(svmfit1,dat)
svmfit1$index
summary(svmfit1)



tune.out=tune(svm,y~.,data=dat,kernel='linear',range=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)#gives the best error
bestmod=tune.out$best.model
summary(bestmod)


#Test Data

set.seed(1)
xtest=matrix(rnorm(20*2),ncol=2)
ytest=sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat=data.frame(x=xtest,y=as.factor(ytest))

plot(xtest,col=(3-ytest))


ypred=predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)#2 misclassification , 10%error

svmfit2=svm(y~.,data=dat,kernel='linear',cost=0.01,scale=FALSE)
ypred=predict(svmfit2,testdat)
table(predict=ypred,truth=testdat$y)#5 misclassification


#Test data non linear

#Test Data

set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat<-data.frame(x=x, y=as.factor(y))
plot(x,col=y+7)


train=sample(200,150)
length(train)
svmfit3=svm(y~.,data=dat[train,],kernel='radial',gamma=1,cost=1)

plot(svmfit3,dat[train,])

summary(svmfit3)
table(svmfit3$fitted,y[train])
svmfit31<-svm(y~.,data=dat[train,],kernel='radial',gamma=1,cost=100000)



svmfit311<-tune(svm,y~.,data=dat[train,],kernel='radial',range=list(cost=c(0.01,0.1,1,10,100,1000,10000,100000),gamma=c(0.5,1,2,3,4)))
summary(svmfit311)
svmfit311$best.model
pred=predict(svmfit311$best.model)
length(pred)
length(-train)
table(true=dat[-train,'y'],
      pred=predict(svmfit311$best.model,dat[-train,]))

svmfit4=svm(y~.,data=dat[train,],kernel='polynomial',degree=2,cost=1)

plot(svmfit4,dat[train,])
plot(svmfit31,dat[train,])
summary(svmfit4)


#SVM with multiple classes

set.seed(1)
x=rbind(x,matrix(rnorm(50*2),ncol=2))
y=c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor((y)))
plot(x,col=(y+1))

svmfit5=svm(y~.,data=dat,kernel="radial",gamma=1,cost=10)
plot(svmfit5,dat)

svmfit51<-tune(svm,y~.,data=dat,kernel="radial",range=list(cost=c(0.01,0.1,1,10,100,1000,10000,100000),gamma=c(0.5,1,2,3,4)))
summary(svmfit51)
svmbestfit51<-svmfit51$best.model

ypred=predict(svmbestfit51)
table(predict=ypred,truth=dat$y)#5 misclassification