#Unsupervised learning

set.seed(2)
#create matrix of 50 observation where first 25 obs has a mean shift relative to next 25 obs

x=matrix(rnorm(50*2),ncol=2)
x

plot(x)
plot(x,col=c(rep(3,25),c(rep(4,25))))
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
plot(x,col=c(rep(3,25),c(rep(4,25))))

km.out=kmeans(x,centers=7,nstart=2)
km.out$cluster

plot(x,col=(km.out$cluster+2),
     main="K-Means Clustering Results with K=2",
     xlab="",ylab="",pch=km.out$cluster+16,cex=2)
points(km.out$centers,col=1:2,pch=3,cex=3,lwd=3)

km.out$tot.withinss #128.6066, 97.97927, 69.75431,50.
km.out$betweenss #345.0113, 375.6386, 403.8636
km.out$totss #473.6179, 473.6179, 473.6179
km.out$tot.withinss/km.out$totss  #0.2715409, 0.20687, 0.14727,0.107,0.113,0.081
dist(x)

plot(cbind(noOfClusts=2:10,withinByTotss=c(27,20,14,10,11,8)))

hist(dist(x))#euclidean distance

#Linkages
hc.complete=hclust(dist(x),method="complete")
plot(hc.complete)
hc.average=hclust(dist(x),method="average")
plot(hc.average)
hc.single=hclust(dist(x),method="single")
plot(hc.single)
hc.centroid=hclust(dist(x),method="centroid")
plot(hc.centroid)

cutree(hc.complete,k=2)
cutree(hc.average,k=2)
cutree(hc.single,k=2)
cutree(hc.single,h=1.2)
cutree(hc.centroid,k=2)

plot(x,col=cutree(hc.complete,k=2)+2)
plot(x,col=cutree(hc.average,k=2)+2)
plot(x,col=cutree(hc.single,k=2)+2)
plot(x,col=cutree(hc.single,h=1.2))
plot(x,col=cutree(hc.centroid,k=2)+2)


#scale variables before performing hierarchical c;ustering of the obs

xsc=scale(x)
plot(hclust(dist(xsc),method="complete"),main="Hierarchical clustering")



#DBScan
library(fpc)
data(iris)
iris=data.frame(iris)
str(iris)
table(iris$Species)
newiris<-iris[-5]
hist(dist(newiris))
ds<-dbscan(newiris,eps=0.42,MinPts = 5, showplot = 1)
table(ds$cluster,iris$Species)
plot(ds,newiris)





#Association Rule Mining

library(arules)
library(arulesViz)

DTdata(Groceries)
Rules<-apriori(Groceries,parameter = list(support=0.01, confidence=.5))
inspect(Rules[])
Rules_new=sort(Rules, by="confidence")
quality(Rules_new)=round(quality(Rules_new),digits=3)
inspect(Rules_new[])
plot(Rules_new)
