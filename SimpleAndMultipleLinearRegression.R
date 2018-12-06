library(ISLR)

data(mtcars)
head(mtcars)
str(mtcars)
cars<-data.frame(mtcars)
hist(cars$mpg)
pairs(cars[1:6])

plot(cars$mpg~cars$wt)

cars$mpg #Y

carslm1<-lm(mpg~wt, data=cars)
carslm1$coefficients #Intercept is B0, B1 is wt 
carslm1$fitted.values #Y^
par(mfrow=c(1,2))
plot(cars$wt,cars$mpg)
plot(cars$mpg~cars$wt)
abline(carslm1)
plot(cars$wt,carslm1$fitted.values)
abline(carslm1)

#Y-Y^
residual1<-cars$mpg-carslm1$fitted.values
residual1

carslm1$residuals#residual


sum(carslm1$residuals**2)

#RSS is Sum((Y-Y^)**2)

RSS1<-sum(residual1**2)#RSS

mean(carslm1$residuals)
shapiro.test(carslm1$residuals)#p value is p> 0.05 so it follows normal distribution, null hypothesis

TSS=sum((cars$mpg-mean(carslm1$fitted.values))**2)
TSS1=sum((cars$mpg-mean(cars$mpg))**2)
TSS1
TSS
RSquare<-(TSS-RSS1)/TSS
RSquare

summary(carslm1)$r.squared
summary(carslm1)$adj.r.squared


RSE<-summary(carslm1$sigma)
RSE
summary(carslm1)


confint(carslm1)
mean


#overall model is determined by F statistics

plot(carslm1,1)#fitted value vs Residual
plot(cars$wt,cars$mpg)




#Multiple linear regression


#Using (mpg ~ wt + hp)

carslm2<-lm(mpg ~ wt + hp, data=cars)
RSS2<-sum(((cars$mpg-carslm2$fitted.values)**2))
RSS2
plot(carslm2,1)
summary(carslm2)


##Using (mpg ~ wt + hp +cy)


cars$cyl<-as.factor((cars$cyl))

mpg4<-cars[cars$cyl==4, 1]
mean(mpg4)

mpg6<-cars[cars$cyl==6, 1]
mean(mpg6)

mpg8<-cars[cars$cyl==8, 1]
mean(mpg8)

cyl1<-c(mean(mpg4),mean(mpg6),mean(mpg8))


lm(cars$mpg~cars$cyl, data=cars)



carslm3<-lm(mpg ~ wt + hp + cyl, data=cars)
summary(carslm3)


contrasts(cars$cyl)

RSS<-carslm3$residuals



carslm4<-lm(mpg ~ wt + hp + cyl +wt*hp, data=cars)
summary(carslm4)

carslm5<-lm(mpg ~ wt + hp + wt*hp, data=cars)
summary(carslm5)
RSS5<-sum(carslm5$residuals**2)
RSS5
plot(carslm5,1)



carssub<-data.frame(head(mtcars,10))
str(carssub)

carssublm<-lm(mpg~wt, data=carssub)
xa<-carssub$wt
xa
x1<-carssub$wt-mean(carssub$wt)
x1
y1<-carssub$mpg-mean(carssub$mpg)
y1
b1<-(sum(x1*y1)/sum(x1**2))
b1
b0<-mean(carssub$mpg)-b1*(mean(carssub$wt))
b0
summary(carssublm)
library(dplyr)
sum(carssub$mpg-carssublm$fitted.values)
sum(carssublm$residuals**2)
RSSsub<-sum((carssub$mpg-carssublm$fitted.values)**2)
TSSsub<-sum((carssub$mpg-mean(carssub$mpg))**2)
RSSsub
TSSsub
R2sub<-(TSSsub-RSSsub)/TSSsub
R2sub
n<-nrow(carssub)
p<-1
#R2Ajusub<-1-((RSSsub/((nrow(carssub)-1)))/(TSSsub/(nrow(carssub)-1)))
R2Ajusub<-(1-((1-R2sub)*9)/8)
R2Ajusub
RSEsub<-sqrt((RSSsub/(nrow(carssub)-1-1)))
RSEsub
summary(carssublm)$sigma
carssublm$coefficients

str(summary(carssublm))
RSSsub<-sum(carssub$mpg-carssublm$fitted.values)
RSSsub




#

plot(cars$hp,cars$mpg)
carslm7<-lm(cars$mpg~cars$hp, data=cars)
summary(carslm7)

RSS7<-sum(carslm7$residuals**2)
RSS7

RSq7<-summary(carslm7)$r.squared
RSq7

RSq7Adj<-summary(carslm7)$adj.r.squared
RSq7Adj

RSE7<-summary(carslm7)$sigma
RSE7


carslm8<-lm(cars$mpg~I(cars$hp**2), data=cars)
summary(carslm8)
RSS8<-sum(carslm8$residuals**2)
RSS8

RSq8<-summary(carslm8)$r.squared
RSq8

RSq8Adj<-summary(carslm8)$adj.r.squared
RSq8Adj

RSE8<-summary(carslm8)$sigma
RSE8

carslm9<-lm(cars$mpg~poly(cars$hp,3), data=cars)
summary(carslm9)
RSS9<-sum(carslm9$residuals**2)
RSS9

RSq9<-summary(carslm9)$r.squared
RSq9

RSq9Adj<-summary(carslm9)$adj.r.squared
RSq9Adj

RSE9<-summary(carslm9)$sigma
RSE9



carslm10<-lm(mpg~wt+hp+wt*hp+I(hp^2), data=cars)
summary(carslm10)

carslm11<-lm(mpg~.+wt*hp+I(hp^2), data=cars)
summary(carslm11)

anova(carslm1,carslm2,carslm5,carslm10,carslm11)




newCarData<-data.frame(wt=c(2.5,3.5,4.5,5.5), hp=c(100,200,250,350))
str(newCarData)
pred1<-predict(carslm1,newdata=newCarData)
pred2<-predict(carslm2,newdata=newCarData)
pred3<-predict(carslm5,newdata=newCarData)
cbind(pred1,pred2,pred3)


pred1<-predict(carslm1,newdata=cars)
pred2<-predict(carslm2,newdata=cars)
pred3<-predict(carslm5,newdata=cars)
cbind(pred1,pred2,pred3,cars$mpg)

RSS1<-sum((cars$mpg-pred1)**2)
RSS2<-sum((cars$mpg-pred2)**2)
RSS3<-sum((cars$mpg-pred3)**2)
RSS1
RSS2
RSS3


