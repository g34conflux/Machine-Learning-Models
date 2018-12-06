library(ISLR)
library(boot)

data("Hitters")
HittersCopy1=data.frame(Hitters)
HittersCopy1=na.omit(HittersCopy1)
set.seed(1)
SalaryCopy1<-log(HittersCopy1$Salary)
HittersCopy1=data.frame(HittersCopy1,SalaryCopy1)
HighSal=ifelse(HittersCopy1$SalaryCopy1<6.62,"No","Yes")#6.62 - Q3 25% high salary, 75% low salaried
HittersCopy1=data.frame(HittersCopy1,HighSal)
head(HittersCopy1)
str(HittersCopy1)
dim(HittersCopy1)
fivenum(HittersCopy1$SalaryCopy1)
plot(HittersCopy1$CHits,HittersCopy1$HighSal)

#glm- generalized linear model

log.mod<-glm(HighSal~HittersCopy1$CAtBat+HittersCopy1$Hits+HittersCopy1$Years, data=HittersCopy1, family=binomial)
coef(log.mod) #B0,B1,B2,B3

summary(log.mod)#z=2 then p value 0.05 p<.05 is significant hence alternate hypothesis
#


log.mod<-glm(HighSal~HittersCopy1$CAtBat+HittersCopy1$Hits+HittersCopy1$Years+HittersCopy1$Walks, data=HittersCopy1, family=binomial)
coef(log.mod) #B0,B1,B2,B3
summary(log.mod)


log.mod<-glm(HighSal~HittersCopy1$AtBat+HittersCopy1$Hits+HittersCopy1$Years, data=HittersCopy1, family=binomial)
coef(log.mod) #B0,B1,B2,B3

summary(log.mod)


log.mod<-glm(HighSal~HittersCopy1$CAtBat+HittersCopy1$Hits+HittersCopy1$Years+HittersCopy1$Walks+HittersCopy1$Runs, data=HittersCopy1, family=binomial)
coef(log.mod) #B0,B1,B2,B3

summary(log.mod)


log.mod<-glm(HighSal~HittersCopy1$CAtBat+HittersCopy1$Hits+HittersCopy1$Years+HittersCopy1$Walks+HittersCopy1$Runs+HittersCopy1$HmRun, data=HittersCopy1, family=binomial)
coef(log.mod) #B0,B1,B2,B3

summary(log.mod)



log.mod<-glm(HighSal~HittersCopy1$AtBat+HittersCopy1$Hits+HittersCopy1$Years+HittersCopy1$Walks+HittersCopy1$Runs+HittersCopy1$HmRun, data=HittersCopy1, family=binomial)
coef(log.mod) #B0,B1,B2,B3

summary(log.mod)

log.mod<-glm(HighSal~HittersCopy1$AtBat+HittersCopy1$CHits+HittersCopy1$Years+HittersCopy1$Walks+HittersCopy1$PutOuts, data=HittersCopy1, family=binomial)
coef(log.mod) #B0,B1,B2,B3

summary(log.mod)


log.mod<-glm(HighSal~HittersCopy1$AtBat+HittersCopy1$CHits+HittersCopy1$Years+HittersCopy1$PutOuts, data=HittersCopy1, family=binomial)
coef(log.mod) #B0,B1,B2,B3

summary(log.mod)

log.mod<-glm(HighSal~HittersCopy1$AtBat+HittersCopy1$CHits+HittersCopy1$Years+HittersCopy1$PutOuts+HittersCopy1$RBI, data=HittersCopy1, family=binomial)
coef(log.mod) #B0,B1,B2,B3

summary(log.mod)


log.mod<-glm(HighSal~HittersCopy1$AtBat+HittersCopy1$Hits+HittersCopy1$Years+HittersCopy1$PutOuts+HittersCopy1$CRBI, data=HittersCopy1, family=binomial)
coef(log.mod) #B0,B1,B2,B3

summary(log.mod)


#Best Model

log.mod<-glm(HighSal~AtBat+CHits+Years+PutOuts+Division, data=HittersCopy1, family=binomial)
coef(log.mod) #B0,B1,B2,B3

summary(log.mod)
log.prob<-predict(log.mod,type="response")
log.pred<-ifelse(log.prob>.5,"Yes","No")
log.tab<-table(True=HighSal,Pred=log.pred)
log.tab


cv.mod<-cv.glm(HittersCopy1,log.mod,K=10)
cv.mod$delta[1]




library(class)
train=sample(1:nrow(HittersCopy1,0.6*nrow(HittersCopy1)))
test=-train

x.train=scale(HittersCopy1[train,c(1,9,7)])
x.test=scale(HittersCopy1[test,c(1,9,7)])


y.train.reg=HittersCopy1$Salary[train]
y.test.reg=HittersCopy1$Salary[test]


y.train.class=HittersCopy1$HighSal[train]
y.test.class=HittersCopy1$HighSal[test]


knn.mod.2<-knn(x.train,x.test,y.train.class,k=2)

knn.tab.2<-table(True=y.test.class, Pred=knn.mod.2)

knn.tab.2

(19/(54+4+15+15))

knn.mod.3<-knn(x.train,x.test,y.train.class,k=3)

knn.tab.3<-table(True=y.test.class, Pred=knn.mod.3)

knn.tab.3

(16/(54+4+12+18))

knn.mod.4<-knn(x.train,x.test,y.train.class,k=4)

knn.tab.4<-table(True=y.test.class, Pred=knn.mod.4)

knn.tab.4

(18/(52+6+12+18))

knn.mod.5<-knn(x.train,x.test,y.train.class,k=5)

knn.tab.5<-table(True=y.test.class, Pred=knn.mod.5)

knn.tab.5

14/(52+6+12+18)


knn.mod.6<-knn(x.train,x.test,y.train.class,k=6)

knn.tab.6<-table(True=y.test.class, Pred=knn.mod.6)

knn.tab.6

15/(52+6+12+18)


knn.reg.6<-knn(x.train,x.test,y.train.reg,k=2)

knn.reg.6<-table(True=y.test.reg, Pred=knn.reg.6)

knn.reg.6

#Question
RSS=mean((y.test.reg-as.numeric(knn.mod.6))^2)
RSS


knn.reg.5<-knn(x.train,x.test,y.train.reg,k=5)

knn.reg.5<-table(True=y.test.reg, Pred=knn.reg.5)

knn.reg.5

RSS1=mean((y.test.reg-as.numeric(knn.mod.5))^2)
RSS1
RSS


#use scale





