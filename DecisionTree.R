library(boots)
library(ISLR)
data(Hitters)

Hitters1<-data.frame(Hitters)
Hitters1<-na.omit(Hitters1)
Salary1<-log(Hitters1$Salary)
Hitters1<-data.frame(Hitters1,Salary1)
HighSal<-ifelse(Hitters1$Salary1<6.62,"No","Yes")
Hitters1<-data.frame(Hitters1,HighSal)
head(Hitters1)

library(tree)
tree.reg.mod<-tree(Hitters1$Salary~Hitters1$Hits+Hitters1$Years,data=Hitters1)
par(mfrow=c(1,1))
plot(tree.reg.mod)
text(tree.reg.mod)
partition.tree(tree.reg.mod)
summary(tree.reg.mod)
#255 - no of rows - number of leaves 25820000 is the RSS
tree.reg.mod
#devianve - RSS if RSS is less than 10% then it will not split it further

tree.reg.mod1<-tree(Hitters1$Salary~.-Salary1-HighSal,data=Hitters1)
#par(mfrow=c(1,2))
plot(tree.reg.mod1)
text(tree.reg.mod1)

summary(tree.reg.mod1)
#255 - no of rows - number of leaves 25820000 is the RSS
tree.reg.mod1

tree.reg.mod2<-tree(Hitters1$HighSal~Hits+Years,data=Hitters1)
par(mfrow=c(1,2))
plot(tree.reg.mod2)
text(tree.reg.mod2)
partition.tree(tree.reg.mod2)
summary(tree.reg.mod2)
#255 - no of rows - number of leaves 25820000 is the RSS
tree.reg.mod2




library(randomForest)
set.seed(1)
train<-sample(1:nrow(Hitters1),2/3*nrow(Hitters1))
test<--train
Hitters1.test<-Hitters1[test,]
HighSal.test<-Hitters1$HighSal[test]

bag.mod<-randomForest(HighSal~.-Salary-Salary1,data=Hitters1,subset=train,ntree=100,mtry=19,importance=TRUE)#if mtry is specified equal to all the predictors then bagging
#average error of all 100 trees is the OOB out of bag error
bag.mod
str(Hitters1)
pred.bag.mod<-predict(bag.mod,newdata = Hitters1.test,type="class")
pred.bag.mod
table(pred=pred.bag.mod,true=HighSal.test)
varImpPlot(bag.mod)

ran.mod<-randomForest(HighSal~.-Salary-Salary1,data=Hitters1,subset=train,ntree=100,mtry=5,importance=TRUE)#if mtry is specified equal to all the predictors then bagging
#average error of all 100 trees is the OOB out of bag error
ran.mod
str(Hitters1)
pred.ran.mod<-predict(ran.mod,newdata = Hitters1.test,type="class")
pred.ran.mod
table(pred=pred.ran.mod,true=HighSal.test)
varImpPlot(ran.mod)
``