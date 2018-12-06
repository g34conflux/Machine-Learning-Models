library(quantmod)
library(forecast)
library(fpp)

mGoog<-read.csv("mGoog.csv")
str(mGoog)
head(mGoog)

googOpen<-mGoog[,2]
head(googOpen)
str(googOpen)

#create time series object
ts1<-ts(googOpen,frequency = 12)
ts1

#plot
plot(ts1,xlab="Years+1",ylab="Goog")
#decompose time series
plot(decompose(ts1),xlab="Years+1")
ts1Train<-window(ts1,start=1,end=3.5)
ts1Test<-window(ts1,start=3.5,end=4.5)
#moving average
goog_ma<-ma(ts1Train,order=2)
#forecast using moving average
goog_ma_forecast<-forecast(goog_ma)
accuracy(goog_ma_forecast,ts1Test)[,5]
#use MAPE

plot(ts1,col="black")
plot(goog_ma_forecast)
lines(goog_ma_forecast$fitted,col="green")
lines(goog_ma_forecast$mean,col="green")


goog_ma<-ma(ts1Train,order=4)
goog_ma_forecast<-forecast(goog_ma)
plot(goog_ma_forecast)
accuracy(goog_ma_forecast,ts1Test)[,5]
lines(goog_ma_forecast$fitted,col="red")
lines(goog_ma_forecast$mean,col="red")

goog_ma<-ma(ts1Train,order=8)
goog_ma_forecast<-forecast(goog_ma)



accuracy(goog_ma_forecast,ts1Test)[,5]
lines(goog_ma_forecast$fitted,col="blue")
lines(goog_ma_forecast$mean,col="blue")


#simple exponential smoothing model
plot(ts1)
fit<-ses(ts1Train,h=13,alpha=0.9)
lines(fit$fitted,col='blue')
lines(fit$mean,col='blue')

fit<-ses(ts1Train,h=13,alpha=0.5)
lines(fit$fitted,col='green')
lines(fit$mean,col='green')

fit<-ses(ts1Train,h=13,alpha=0.2)
lines(fit$fitted,col='red')
lines(fit$mean,col='red')

#Double exponential smoothing model

plot(ts1)
fit<-holt(ts1Train,h=13,alpha=0.9, beta=0.9)
lines(fit$fitted,col='blue')
lines(fit$mean,col='blue')

fit<-holt(ts1Train,h=13,alpha=0.9, beta=0.5)
lines(fit$fitted,col='green')
lines(fit$mean,col='green')

fit<-holt(ts1Train,h=13,alpha=0.5, beta=0.5)
lines(fit$fitted,col='red')
lines(fit$mean,col='red')

fit<-holt(ts1Train,h=13)
lines(fit$fitted,col='yellow')
lines(fit$mean,col='yellow')

#Model="ETS"
#E = Error Type, T=Trend, S=Season
#N=None, A=Additive, M=Multiplicative, Z=Automatically select
ets1<-ets(ts1Train,model="MMZ")
fcast<-forecast(ets1)
plot(fcast)
lines(ts1Test,col="red")

accuracy(fcast,ts1Test)[,5]
#fcast
