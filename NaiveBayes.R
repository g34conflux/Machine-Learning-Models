library(e1071)
#Load training and test data

buys.computer<-read.csv("buys.computer.csv")
buys.computer.test<-read.csv("buys.computer.test.csv")

#Explore data

head(buys.computer)
str(buys.computer)
head(buys.computer.test)


#Train data using Naive Bayes
NB<-naiveBayes(buy~.,data=buys.computer)
NB

#Predict Class - Category
pred_class<-predict(NB,newdata=buys.computer.test, type="class")
pred_class


#Predict raw probability - Probability value
pred_raw<-predict(NB,newdata=buys.computer.test, type="raw")
pred_raw