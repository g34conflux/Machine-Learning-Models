#scalar is single value
#vector is multiple values
#R has 6 datastructures

#Vector
#No mixed datatypes allowed


x<- c(2,3,4,5,6)
x
z<-50:300
z
y<-c(x,7,x)
y
x+y
x-y
x*y
x/y
x^y
x%/%y
x%%y
myvec<-x
myvec
exp(myvec)
sign(myvec)

sign(-myvec)
factorial(myvec)
length(myvec)
range(myvec)
t1<-fivenum(myvec)
t<-fivenum(y)
sort(y)
sort(y, decreasing = TRUE)
IQR<-t[4]-t[2]
IQR

v<-c(1,2,3,NA)
v
mean(v)
g<-FALSE
class(g)
length(v)
mean(v, na.rm= TRUE)
median(v, na.rm= TRUE)
u<-c(1,2,3,"z","a")
u
mode(u)
range((u))


z[-1]
z[-(1:5)]
z[-c(10,20,30)]


seq(from=5, to=10)
seq(0,10,by=2)

seq(10,0,by=-2)
seq(0,10)

#a, a+d, a+2d, a+3d, a+4d
seq(0,10,length.out=10)


rep(0, times=10)

rep(1:5, times=2)

rep(1:5, each=2)
rep(1:5, times=2, each=2)
rep(1:5, each=2, times=2)
rep("a", times=2)

quarters<-paste("Quarters",1:4, sep="-")
.Last.value
cat("quaters",1:4,sep="-")
.Last.value


length(quarters)
substr(quarters, start=10, stop=10)
nchar(quarters)
tolower(quarters)
toupper(quarters)


m2<-c("do","do-not","not")
grep("do", m2)
m2<-c(m2,"1")
m2

grep("[a-z 1]",m2)
gsub("do","did",m2)


m2<-c(m2, "not do")
m2
regexpr("find", m2)

#If else function for vector
#compares each element in vector and returns the greater value

f<-c(6,5,7)
s<-c(5,6,6)
ifelse(f>s,f,s)


#is.na(v)will check if each element is NA
v<-c(1,2,3,NA,5,NA)

#removes NA from the vector
v[c(!is.na(v))]
v[(!is.na(v))]






#Matrix
#fills the col first and then the row

m1<-matrix(c(4,5,6,7), ncol=2, nrow=2)
m1

#fills the rows first
m2<-matrix(c(4,5,6,7), ncol=2, nrow=2, byrow=TRUE)
m2

#fills the 2 cols and the repeats the values if not sufficient
m3<-matrix(c(4,5,6,7,8), ncol=2)
m3


#fills the 3 by 3 matrix with 1's as it repeats
m4<-matrix(1,ncol=3, nrow=3)
m4

#fills the 3 by 3 matrix with NA's as it repeats
m5<-matrix(NA,2,3)
m5

v2<-1:5
v3<-v2^2


#binds the rows
v11<-rbind(v2,v3)

#binds the cols
v12<-cbind(v2,v3)
v12
dim(v12)





colnames(v12)<-c("A","B")

rownames(v12)<-c("a","b","c","d","e")
v12


x<-c("a","b","d")

x[x %in% c("a","b","c","d","e")]

#identity matrix
diag(4)


diag(v2)


m4<-matrix(1:12,nrow=4)

m4[2,2]

m4[,1]


#returns 3rd row all cols
m4[3,]

#returns 2 and 4the row
m4[c(2,4),]

#returns all rows, cols 1 and 2
m4[,1:2]


#return all rows where value in 2nd col >6

m5<-m4[c(m4[,2]>6),]
m5



library(MASS)
library(dplyr)
a<-matrix(c(2,3,4,3,1,2,1,1,3), ncol=3)
a

x<-matrix(c(x,y,z))
x

b<-matrix(c(6,5,9))

res<-ginv(a)%*%b
res


a%*%res

ginv(a)

solve(a)

#Array(data,dimension)

arr1<-array(1:8, c(2,2,2))
arr1


arr2<-array(1:27, c(3,3,3))

arr2

#2nd col in 1st and 3rd page
arr3<-arr2[,2,c(1,3)]
arr3




#List() can be accessed using name and position. Use double square brackets for positional access

l1<-list(name="Fred", wife="Mary", children=3, childrenAges=c(3,5,7))
l1

l1$name 
l1$childrenAges[2]

#by position
l1[[1]]

l1[[4]]


l1[[4]][2]


#factor by default ordered = FALSE, the levels are internally stored as intergers. 1,2,3,4,5

f1<-factor(c("DI","NE","AG","DI","FD","FA","AG"), levels=c("FD","DI","NE","AG","FA"), ordered=TRUE)
f1

levels(f1)
table(f1)
mode(f1)
str(f1)


f2<-factor(c("DI","NE","AG","DI","FD","FA","AG"), levels=c("FD","DI","NE","AG","FA"))
f2

levels(f2)
table(f2)
mode(f2)
str(f2)


#Dataframes  

student<-data.frame(roll_no=c(1:4), name=c("Anil","Bikash", "Chintan","Sunil"))
student
nrows(students)
ncol(student)
row.names(student)<-c("First","Second","Third","Fourth")
student
dim(student)

student<-edit(student)
student


#data.table package is more faster and more efficient thab data.frame
getwd()

dir()


dir.create("datafile")
setwd( "C:/Users/aadit/Downloads/RClass/StudyMaterial/Basics")
getwd()
dir()

numb_in_file<-scan("numb_in_file.txt")
numb_in_file
mode(numb_in_file)


numb_in_file<-scan("numb_in_file.txt", what="character")
numb_in_file
mode(numb_in_file)

numb_in_file<-scan("numb_in_file.txt", what="character", nmax=5)
numb_in_file


numb_in_file<-scan("numb_in_file.txt", what="character", sep=" ", skip=1)
numb_in_file



numb_in_file<-scan("numb_in_file.txt", what="character", sep=" ", skip=1, nlines=2)
numb_in_file


numb_in_file<-scan("numb_in_file.txt", sep=",")
numb_in_file

#import file using data.table

empdata<-read.table("EMP.csv", sep=",")
head(empdata)

empdata<-read.table("EMP.csv", sep=",", header=TRUE)
head(empdata)


str(empdata)

empdata<-read.table("EMP.csv", sep=",", header=TRUE, nrows=10)
empdata


empdata<-read.table("EMP.csv", sep=",", header=FALSE, nrows=10, skip=5)
empdata
library(dplyr)
empdataHeader<-read.table("EMP.csv", sep=",", header=FALSE, nrows=1)
empdataHeader

empdata<-read.table("EMP.csv", sep=",", header=FALSE, nrows=10, skip=5)
empdata
mode(empdata)


colnames(empdata)<-unlist(empdataHeader)
empdata



#import files using data.csv
empdatacsv <- read.csv("EMP.csv")


head(empdatacsv)
mode(empdatacsv)

library(dplyr)
names(empdatacsv)

#select

head(select(empdatacsv,FIRST_NAME,LAST_NAME,SALARY))
head(select(empdatacsv,FIRST_NAME:SALARY))
head(select(empdatacsv,-(EMAIL:SALARY)))


#FILTER
filter(select(empdatacsv,FIRST_NAME:SALARY),SALARY>15000&JOB_ID=="AD_VP")

#rename col name

empdatacsv1<-rename(empdatacsv, EMPID=EMPLOYEE_ID, FN=FIRST_NAME, LN=LAST_NAME)
head(empdatacsv1)

#mutate - transform existing variable or create a new variable

empdatacsv1<-mutate(empdatacsv1,z_salary=(SALARY-mean(SALARY))/(sd(SALARY)))
head((select(empdatacsv1,SALARY,z_salary)))
tail(select(empdatacsv1,SALARY,z_salary))
#group by

job_group<-group_by(empdatacsv1,JOB_ID)
summarize(job_group, sal = mean(SALARY), nos = length(SALARY))


dir()

png(filename="./plot1.png", width=480, height=900, units="px")

par(mfrow=c(2,1))

hist(empdatacsv1$SALARY)
boxplot(empdatacsv1$SALARY)

dev.off()

student<-data.frame(roll_no=c(1:4), name=c("Anil","Bikash", "Chintan","Sunil"))
student
x<-2
y<-3
x
y
nrows(students)
ncol(student)
row.names(student)<-c("First","Second","Third","Fourth")
student
dim(student)
#Saving the data, making it persistent
save(list=c("empdata","x","y"),file="empdata1.RData")
rm(empdata,x,y)
empdata
x
y
load("empdata1.RData")
save(list=c("student","x","y"),file="student.RData")
rm(student,x,y)
student
x
y
load("student.RData")


empdata
x
y

student
x
y
#student<-edit(student)
#student


student





##########################################################################################




#Run batch script
source("example.R")




#divert output to file
sink("record.lis")

o1<-35
o2<-36

o1
o2

print("The output is diverted to a file")
print(o1*o2)

sink()
o1-02

sink("record.lis")

o1-o2
print("write again")
sink()

library(xlsx)


#Export datra from R

write.table(empdata,file="./EMP1.txt", sep="|",col.name=TRUE)
write.csv(empdata, file="./EMP2.csv")

write.table(empdata,"./EMP3.xlsx")



#Apply functions , internally loops
library(MASS)
x<-list(a=1:5, b=rnorm(10))
x$a
m1<-mean(x$a)
m2<-mean(x$b)
m1
m2

x3<-lapply(x,mean)
x3

lapply(x,sqrt)

lapply(x,range)


#display results in a structured format
sapply(x,mean)

sapply(x,sqrt)

sapply(x,range)

fivenum(x$a)
fivenum(x$b)

vapply(x,fivenum, c(min=0,"Q1"=0,median =0,"Q3"=0 ,max=10))

vapply(x,range, c(min=1,max=0))


x<-matrix(1:50, ncol=5)
x


s1<-apply(x,1,sum)
m1<-apply(x,2,mean)

df1<-data.frame(x)
df1<-cbind(df1,s1)
df1<-rbind(df1,m1)
df1


set.seed(1)
ht<-(c(runif(5,150,170),runif(5,160,180),runif(5,190,200)))

gender<-c(rep("F",5), rep("M",10))

tapply(ht,gender,mean)

#multivariate
seq(1,5,0.5)
seq(11,20,1)

mapply(seq,c(1,11),c(5,20),c(0.5,1))

mapply(rep,c("F","M"),c(5,10))
?matrix
