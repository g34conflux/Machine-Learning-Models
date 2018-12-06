library(neuralnet)

set.seed(101)
iris1=data.frame(iris)

sample.size<-100

iristrain<-iris1[sample(1:nrow(iris1),sample.size),]
nnet_iristrain<-iristrain
#Covert cetgorical output to three binary variables
nnet_iristrain<-cbind(nnet_iristrain,iristrain$Species=='setosa')
nnet_iristrain<-cbind(nnet_iristrain,iristrain$Species=='versicolor')
nnet_iristrain<-cbind(nnet_iristrain,iristrain$Species=='virginica')
head(nnet_iristrain)
names(nnet_iristrain)[6]<-'setosa'
names(nnet_iristrain)[7]<-'versicolor'
names(nnet_iristrain)[8]<-'virginica'
head(nnet_iristrain)
#run neuralnet function to train the neural networks using backpropagation
#hidden=vector of integers specifying the number of hidden neurons in 1st hidden layer 3 neurons, 2nd hidden layer 2 neurons

nn<-neuralnet(setosa+versicolor+virginica ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
              data=nnet_iristrain,hidden=c(3,2),rep=5,
              algorithm ='backprop',learningrate = 0.15,
              threshold=0.01, linear.output=F)

plot(nn)

nn<-neuralnet(setosa+versicolor+virginica ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
              data=nnet_iristrain,hidden=c(3,2),rep=10,
              algorithm ='backprop',learningrate = 0.04,
              threshold=0.01, linear.output=F)

plot(nn)


nn$act.fct#activation function
nn$err.fct#error function
nn$net.result#a list containing the overall result for every rep
nn$weights#fitted weights
nn$result.matrix#
nn$startweights#list of startweights


mypredict<-compute(nn,iris[-5],rep=1)$net.result
maxidx<-function(arr){
  return(which(arr==max(arr)))
}
idx<-apply(mypredict,1,maxidx)
prediction<-c('setosa','versicolor','virginica')[idx]
table(pred=prediction,true=iris$Species)
