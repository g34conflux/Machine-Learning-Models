#Attrition Case Study 

#Reading the data from file to DataFrame

dfEmpDetail<-read.csv('Attrition Case Study.csv', header=TRUE)
str(dfEmpDetail)
#Transformation of data, Gender to factor
dfEmpDetail$Gender<-factor(dfEmpDetail$Gender, levels=c('Male','Female'))

str(dfEmpDetail) # Dataframe with 1471 obs of 35 variables
head(dfEmpDetail)
#Initial Analysis of Data

#Mean salary of Employees
table(is.na(dfEmpDetail$MonthlyIncome)) # Has 1 NA
mean(na.omit(dfEmpDetail$MonthlyIncome))
fivenum(dfEmpDetail$MonthlyIncome)

#Visualization of Monthly Income
boxplot(dfEmpDetail$MonthlyIncome)

meanSal=mean(na.omit(dfEmpDetail$MonthlyIncome))
sdSal=sd(na.omit(dfEmpDetail$MonthlyIncome))

#Visualization of Monthly Income
#Observation: more employees with less salary
hist(dfEmpDetail$MonthlyIncome,col=c(1:7),freq=FALSE)
lines(density(dfEmpDetail$MonthlyIncome,na.rm=TRUE),adjust=2, add=TRUE,lwd=2, col='blue')#check why line is flat
curve(dnorm(x,mean=meanSal,sd=sdSal),add=TRUE,lw=2, col=2)

#Visualization of Monthly Income
#Observation: more employees with less salary
ggplot(dfEmpDetail, aes(x = MonthlyIncome)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density()
ggplot(dfEmpDetail, aes(x = MonthlyIncome)) + 
  geom_histogram(aes(y = ..density..,fill=..count..),binwidth = 1000)+
geom_density(col=2)


#Visualization of Gender Vs Income, 
#Observation : The plot shows that there is no significant difference in mean of Salary
ggplot(dfEmpDetail, aes(Gender,MonthlyIncome))+
  geom_boxplot(outlier.colour = 'red',col=c(4:5))

#Test of Hypothesis - t test for population means
#2 sampled T test - H0:meanSalary(Male)=meanSalary(Female), Ha:mean Salary(male)!=mean(female)
#Observation: p value 0.2218, so null hypothesis
t.test(dfEmpDetail$MonthlyIncome~dfEmpDetail$Gender) 

#Test of Hypothesis - t test for population means
#Observation p.value = 4.4e-14
t.test(dfEmpDetail$MonthlyIncome~dfEmpDetail$Attrition) 

#Visualization of Gender vs attrition using proportion table
Gender_Attrition_table<-table(dfEmpDet$Attrition,dfEmpDet$Gender)
Gender_Attrition_table

Monthly_Income_Age_lm<-lm(MonthlyIncome~Age, data=dfEmpDetail)
summary(Monthly_Income_Age_lm)
cor.test(dfEmpDetail$MonthlyIncome,dfEmpDetail$Age)
cov(dfEmpDetail$MonthlyIncome,dfEmpDetail$Age)
str(dfEmpDetail$Age)
str(dfEmpDetail$MonthlyIncome)



#X (chi) sq testfor gender and Attrition
#Observation p value=0.2906
#Result shows that the two are not dependent
Gender_Attrition_chi<-chisq.test(Gender_Attrition_table)
Gender_Attrition_chi$expected
Gender_Attrition_chi 



#Visualization of Education vs attrition using proportion table
Education_Attrition_table<-table(dfEmpDet$Attrition,dfEmpDet$Education)
Education_Attrition_table
prop.table(Education_Attrition_table)*100

#X (chi) sq test for Education and Attrition
#Observation p value=0.5455
#Result shows that the two are not dependent
Education_Attrition_chi<-chisq.test(Education_Attrition_table)
Education_Attrition_chi$expected
Education_Attrition_chi
Education_Attrition_chi$statistic #Result shows that the two are not dependent


#Visualization of Gender vs attrition using proportion table
JobLevel_Attrition_table<-table(dfEmpDet$Attrition,as.factor(dfEmpDet$JobLevel))
prop.table(JobLevel_Attrition_table)*100

#X (chi) sq test for Job Level and Attrition
#Observation p-value=6.635e-15
#Result shows that the two are dependent
JobLevel_Attrition_chi<-chisq.test(JobLevel_Attrition_table)
JobLevel_Attrition_chi$expected
JobLevel_Attrition_chi
JobLevel_Attrition_chi$statistic #Result shows that the two are not dependent


#Visualization of JobSatisfaction vs attrition using proportion table
JobSatisfaction_Attrition_table<-table(dfEmpDet$Attrition,dfEmpDet$JobSatisfaction)
JobSatisfaction_Attrition_table


#X (chi) sq testfor JobSatisfaction and Attrition
#Observation p value=0.0005563
#Result shows that the two are dependent
JobSatisfaction_Attrition_chi<-chisq.test(JobSatisfaction_Attrition_table)
JobSatisfaction_Attrition_chi$expected
JobSatisfaction_Attrition_chi 



#Visualization of PerformanceRating vs attrition using proportion table
PerformanceRating_Attrition_table<-table(dfEmpDet$Attrition,dfEmpDet$PerformanceRating)
PerformanceRating_Attrition_table


#X (chi) sq testfor PerformanceRating_Attrition_table and Attrition
#Observation p value=0.9901
#Result shows that the two are not dependent
PerformanceRating_Attrition_chi<-chisq.test(PerformanceRating_Attrition_table)
PerformanceRating_Attrition_chi$expected
PerformanceRating_Attrition_chi 

#Visualization for Percent Salary Hike vs Attrition
#Observation: Shows that the mean hike in both cases is not significantly different
boxplot(dfEmpDetail$PercentSalaryHike~dfEmpDetail$Attrition)

#Test of Hypothesis - t test for population means
#2 sampled T test - H0:meanSalaryHike(Attrition)=meanSalaryHike(Non-Attrition), Ha:meanSalaryHike(Attrition)!=meanSalaryHike(Non-Attrition)
#Observation: p value 0.2218, so null hypothesis
t.test(dfEmpDetail$PercentSalaryHike~dfEmpDetail$Attrition) 


library(caret)
#Splitting the Data into Training and Test Sets in the ratio 8:2
validation_index<-createDataPartition(na.omit(dfEmpDetail$Attrition),p=0.8,list=FALSE)
#Training set contains 80% of the rows
training_set<-na.omit(dfEmpDetail[validation_index,])
#Test Set contains 20% of the rows
test_set<-na.omit(dfEmpDetail[-validation_index,])

dim(training_set)
dim(test_set)


MonthlyIncome_lm<-lm(formula=MonthlyIncome~TotalWorkingYears+JobLevel,data=training_set)
#Observation: Multiple R-squared:  0.9073,	Adjusted R-squared:  0.9071 
#F-statistic:  5740 on 2 and 1173 DF,  p-value: < 2.2e-16
summary(MonthlyIncome_lm)
monthlyIncome_Pred<-predict(MonthlyIncome_lm, newdata=test_set)
str(monthlyIncome_Pred)


sum(MonthlyIncome_Residuals)
par(mfrow=c(2,2))
plot(MonthlyIncome_lm)

#Getting the Actual and predicted values into data frame 


Actual_Predicted<-cbind(test_set['MonthlyIncome'],monthlyIncome_Pred,test_set['JobLevel'])
str(Actual_Predicted)


#Plotting the Actual and Predicted values from model, The model looks real good
ggplot(Actual_Predicted, aes(MonthlyIncome))+
  geom_line(aes(y=MonthlyIncome, col='blue'))+
  geom_line(aes(y=monthlyIncome_Pred, col='green'))


#Regression using Synergy effect #BestModel
MonthlyIncome_Synergy_lm<-lm(formula=MonthlyIncome~TotalWorkingYears*JobLevel,data=training_set)
#Multiple R-squared:  0.916,	Adjusted R-squared:  0.9158 
#F-statistic:  4260 on 3 and 1172 DF,  p-value: < 2.2e-16
summary(MonthlyIncome_Synergy_lm)
monthlyIncome_Synergy_Pred<-predict(MonthlyIncome_Synergy_lm, newdata=test_set)
str(monthlyIncome_Synergy_Pred)

#Regression using Synergy effect with individual factors #BestModel
MonthlyIncome_Individual_Synergy_lm<-lm(formula=MonthlyIncome~TotalWorkingYears+JobLevel+TotalWorkingYears*JobLevel,data=training_set)
#Multiple R-squared:  0.916,	Adjusted R-squared:  0.9158 
#F-statistic:  4260 on 3 and 1172 DF,  p-value: < 2.2e-16
summary(MonthlyIncome_Individual_Synergy_lm)
monthlyIncome_Individual_Synergy_Pred<-predict(MonthlyIncome_Individual_Synergy_lm, newdata=test_set)
str(monthlyIncome_Individual_Synergy_Pred)

#Best Model
#Regression using Polynomial with individual factors
MonthlyIncome_Individual_Poly_lm<-lm(formula=MonthlyIncome~TotalWorkingYears+JobLevel+I(JobLevel^2),data=training_set)
#Multiple R-squared:  0.9203,	Adjusted R-squared:  0.9201 
#F-statistic:  4513 on 3 and 1172 DF,  p-value: < 2.2e-16
summary(MonthlyIncome_Individual_Poly_lm)
monthlyIncome_Individual_Poly_Pred<-predict(MonthlyIncome_Individual_Poly_lm, newdata=test_set)
str(monthlyIncome_Individual_Poly_Pred)
Actual_Predicted<-cbind(test_set['MonthlyIncome'],monthlyIncome_Individual_Poly_Pred)
sum((monthlyIncome_Individual_Poly_Pred-test_set['MonthlyIncome']))#52544.7
sum((monthlyIncome_Individual_Synergy_Pred-test_set['MonthlyIncome']))#47861.87
sum((monthlyIncome_Synergy_Pred-test_set['MonthlyIncome']))#47861.87
sum((monthlyIncome_Pred-test_set['MonthlyIncome']))#52286.42



#Attrition Model Building
#Run algorithms using 10 fold cross validation
control_param<-trainControl(method='cv', number=10)
metric<-'Accuracy'# 95% accurate
#training_set$BusinessTravel<-as.integer(training_set$BusinessTravel)
#training_set$Department<-as.integer(training_set$Department)

#training_set$Gender<-as.integer(training_set$Gender)
#training_set$EducationField<-as.integer(training_set$EducationField)

#training_set$JobRole<-as.integer(training_set$JobRole)
#training_set$MaritalStatus<-as.integer(training_set$MaritalStatus)
#training_set$Over18<-as.integer(training_set$Over18)
#training_set$OverTime<-as.integer(training_set$OverTime)
#training_set$Attrition<-as.integer(training_set$Attrition)

fit_knn<-train(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel+Age+YearsWithCurrManager+WorkLifeBalance+RelationshipSatisfaction+JobInvolvement+TrainingTimesLastYear+MonthlyIncome,data=training_set,method='knn',trControl=control_param)
str(training_set)


#glm- generalized linear model #use the prediction and true value table

log.mod<-glm(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel, data=training_set, family=binomial)
coef(log.mod) #B0,B1,B2,B3

summary(log.mod)#AIC: 967.12, this value should be low for best model

log.mod1<-glm(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel+YearsWithCurrManager, data=training_set, family=binomial)
coef(log.mod1) #B0,B1,B2,B3

summary(log.mod1)#AIC: 958.25, this value should be low for best model

log.mod2<-glm(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel+YearsWithCurrManager+Age, data=training_set, family=binomial)
coef(log.mod2) #B0,B1,B2,B3

summary(log.mod2)#AIC: 961.36, this value should be low for best model


log.mod3<-glm(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel+Age+YearsWithCurrManager+Department, data=training_set, family=binomial)
coef(log.mod3) #B0,B1,B2,B3

summary(log.mod3)#AIC: 947.14, this value should be low for best model


log.mod4<-glm(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel+Age+YearsWithCurrManager+Department+WorkLifeBalance, data=training_set, family=binomial)
coef(log.mod4) #B0,B1,B2,B3

summary(log.mod4)#AIC: 942.79, this value should be low for best model


log.mod5<-glm(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel+Age+YearsWithCurrManager+Department+WorkLifeBalance+RelationshipSatisfaction, data=training_set, family=binomial)
coef(log.mod5) #B0,B1,B2,B3

summary(log.mod5)#AIC: 941.38, this value should be low for best model


log.mod6<-glm(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel+Age+YearsWithCurrManager+Department+WorkLifeBalance+RelationshipSatisfaction+JobInvolvement, data=training_set, family=binomial)
coef(log.mod6) #B0,B1,B2,B3

summary(log.mod6)#AIC: 927.8, this value should be low for best model


##glm- generalized linear model, #Best Model
log.mod7<-glm(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel+Department+Age+YearsWithCurrManager+WorkLifeBalance+RelationshipSatisfaction+JobInvolvement+TrainingTimesLastYear, data=training_set, family=binomial)
coef(log.mod7) #B0,B1,B2,B3

summary(log.mod7)#AIC: 923.8, this value should be low for best model


#This predicts better than the last model
log.mod8<-glm(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel+Department+Age+YearsWithCurrManager+WorkLifeBalance+RelationshipSatisfaction+JobInvolvement+TrainingTimesLastYear+MonthlyIncome, data=training_set, family=binomial)
coef(log.mod8) #B0,B1,B2,B3

summary(log.mod8)#AIC: 925.8, this value should be low for best model


log.prob<-predict(log.mod7,type="response")
log.pred<-ifelse(log.prob>.5,"Yes","No")
str(log.pred)
log.tab<-table(True=training_set$Attrition,Pred=log.pred)
log.tab


#This prediction gives less misclassification error than the best model log.mod7
log.prob<-predict(log.mod8,type="response")
log.pred<-ifelse(log.prob>.5,"Yes","No")
str(log.pred)
log.tab<-table(True=training_set$Attrition,Pred=log.pred)
log.tab

log.prob<-predict(log.mod6,type="response")
log.pred<-ifelse(log.prob>.5,"Yes","No")
str(log.pred)
log.tab<-table(True=training_set$Attrition,Pred=log.pred)
log.tab


cv.mod<-cv.glm(training_set,log.mod8,K=10)
cv.mod$delta[1] #0.1193421

cv.mod<-cv.glm(training_set,log.mod7,K=10)
cv.mod$delta[1] #0.1191161

#KNN- K Nearest Neighbour
library(class)


x.train<-training_set[,c(14,15,17,24,25,26,29,30,31,35)]
str(x.train)
x.test<-test_set[,c(14,15,17,24,25,26,29,30,31,35)]
str(x.test)
y.train.class<-training_set[,1]
y.test.class<-test_set[,1]

y.train.reg<-training_set[,19]
str(y.train.reg)
y.test.reg<-test_set[,19]

#KNN models for MonthlyIncome
knn.pred.reg<-knn(x.train,x.test,y.train.reg,k=2)
knn.pred.reg<-as.numeric(knn.pred.reg)
RSS<-mean((y.test.reg-knn.pred.reg)^2)#RSS=55879693.09

knn.pred.reg1<-knn(x.train,x.test,y.train.reg,k=3)
knn.pred.reg1<-as.numeric(knn.pred.reg1)
RSS1<-mean((y.test.reg-knn.pred.reg1)^2)#RSS=55975473.77

knn.pred.reg2<-knn(x.train,x.test,y.train.reg,k=4)
knn.pred.reg2<-as.numeric(knn.pred.reg2)
RSS2<-mean((y.test.reg-knn.pred.reg2)^2)#RSS=55909210


knn.pred.reg3<-knn(x.train,x.test,y.train.reg,k=5)
knn.pred.reg3<-as.numeric(knn.pred.reg3)
RSS3<-mean((y.test.reg-knn.pred.reg3)^2)#RSS=56004676.81

#Best Model
knn.pred.reg4<-knn(x.train,x.test,y.train.reg,k=6)
knn.pred.reg4<-as.numeric(knn.pred.reg4)
RSS4<-mean((y.test.reg-knn.pred.reg4)^2)#RSS=55807965.22

#KNN Models for Attrition
knn.mod<-knn(x.train,x.test,y.train.class,k=2)
knn.tab<-table(True=y.test.class,Pred=knn.mod)
knn.tab #30% miss classification
71/223*100

knn.mod1<-knn(x.train,x.test,y.train,k=3)
knn.tab1<-table(True=y.test.class,Pred=knn.mod1)
knn.tab1 #26.7% miss classification
62/232*100

knn.mod2<-knn(x.train,x.test,y.train,k=4)
knn.tab2<-table(True=y.test.class,Pred=knn.mod2)
knn.tab2 #25.1% miss classification
59/235*100

knn.mod3<-knn(x.train,x.test,y.train,k=5)
knn.tab3<-table(True=y.test.class,Pred=knn.mod3)
knn.tab3 #24.05% miss classification
57/237*100

knn.mod4<-knn(x.train,x.test,y.train,k=6)
knn.tab4<-table(True=y.test.class,Pred=knn.mod4)
knn.tab4#24.57% miss classification
58/236*100


#Best Model with K=7
knn.mod5<-knn(x.train,x.test,y.train,k=7)
knn.tab5<-table(True=y.test.class,Pred=knn.mod5)
knn.tab5#23.01% miss classification
55/239*100

knn.mod6<-knn(x.train,x.test,y.train,k=8)
knn.tab6<-table(True=y.test.class,Pred=knn.mod6)
knn.tab6#23.52% miss classification
56/238*100

knn.mod7<-knn(x.train,x.test,y.train,k=9)
knn.tab7<-table(True=y.test.class,Pred=knn.mod7)
knn.tab7#24.05% miss classification
57/237*100


library(tree)
##Decision Tree
par(mfrow=c(1,1))
#tree.EmpDetails<-tree(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel+Department+Age+YearsWithCurrManager+WorkLifeBalance+RelationshipSatisfaction+JobInvolvement+TrainingTimesLastYear+MonthlyIncome)
#tree.EmpDetails<-tree(Attrition~JobSatisfaction+PercentSalaryHike+JobLevel+Age+YearsWithCurrManager+WorkLifeBalance+RelationshipSatisfaction+JobInvolvement+TrainingTimesLastYear+MonthlyIncome)
tree.EmpDetails<-tree(Attrition~.,dfEmpDetail)
str(dfEmpDetail)
#partition.tree(tree.EmpDetails)#Used for single or two predictors
plot(tree.EmpDetails)
text(tree.EmpDetails)

#Residual mean deviance:  0.09531 = 138.8 / 1456 
#Variables actually used in tree construction:
# [1] "OverTime"                "TotalWorkingYears"       "JobRole"                
#[4] "HourlyRate"              "EnvironmentSatisfaction" "MonthlyIncome"          
#[7] "DailyRate"               "YearsInCurrentRole"      "StockOptionLevel"       
#[10] "MonthlyRate"             "TrainingTimesLastYear"  
summary(tree.EmpDetails)
tree.EmpDetails

library(caret)
#train<-sample(1:nrow(dfEmpDetail,nrow(dfEmpDetail)*2/3))
#length(train)
#training.set<-dfEmpDetail[train,]
#test.set<-dfEmpDetail[-train,]
#length(test.set)
#MonthlyIncome.test<-MonthlyIncome[-train]
#Attrition.test<-test.set$Attrition[-train]

set.seed(1)
train<-sample(1:nrow(na.omit(dfEmpDetail)),2/3*nrow(na.omit(dfEmpDetail)))
length(train)
test<--train
dfEmpDetail.test<-dfEmpDetail[test,]
length(dfEmpDetail.test)
MonthlyIncome.test<-dfEmpDetail$MonthlyIncome[test]
length(MonthlyIncome.test)
#Random Forest
#For MontlyIncome
#Random forest regression mtry=no:of predictors/3
#Best Model
rf.MonthlyIncome<-randomForest(MonthlyIncome~.,data=na.omit(dfEmpDetail),subset=train,mtry=12,ntree=500,importance=TRUE)
rf.MonthlyIncome # Mean of squared residuals: 1165548 ,% Var explained: 95.05

rf.ypred<-predict(rf.MonthlyIncome, newdata=dfEmpDetail.test)
rf.ypred

cbind(true=MonthlyIncome.test,pred=rf.ypred)
RSS=sum((na.omit(rf.ypred-MonthlyIncome.test))^2)
RSS#19563544534
varImpPlot(rf.MonthlyIncome)#shows the important variables JobLevel, JobRole,TotalWorkingExperience,YearsAtCompany,Age,YearsInCurrentRole,YearsWithCurrManager
plot(rf.MonthlyIncome)#Shows error stabilized after 250-500

#Bagging 
#Monthly Income
#Bagging regression mtry=no:of predictors
bag.MonthlyIncome<-randomForest(MonthlyIncome~.,data=na.omit(dfEmpDetail),subset=train,mtry=34,ntree=500,importance=TRUE)
bag.MonthlyIncome # Mean of squared residuals: 1131103 ,% Var explained: 95.2

bag.ypred<-predict(bag.MonthlyIncome, newdata=dfEmpDetail.test)
bag.ypred

cbind(na.omit(MonthlyIncome.test),na.omit(bag.ypred))
RSS=sum(na.omit((bag.ypred-MonthlyIncome.test)^2))
RSS#521949652

varImpPlot(bag.MonthlyIncome)#shows the important variables JobLevel, JobRole,TotalWorkingExperience,YearsAtCompany,Age
#Traing and test datafor Attrition - Categorical variable
set.seed(1)
train<-sample(1:nrow(na.omit(dfEmpDetail)),2/3*nrow(na.omit(dfEmpDetail)))
length(train)
test<--train
dfEmpDetail.test<-dfEmpDetail[test,]
length(dfEmpDetail.test)
Attrition.test<-dfEmpDetail$Attrition[test]


#Random Forest
#For Attrition

rf.Attrition<-randomForest(as.factor(Attrition)~JobSatisfaction+PercentSalaryHike+JobLevel+Department+Age+YearsWithCurrManager+WorkLifeBalance+RelationshipSatisfaction+JobInvolvement+TrainingTimesLastYear+MonthlyIncome,data=dfEmpDetail, subset=train, ntree=500,mtry=5,importance=TRUE)
rf.Attrition#OOB estimate of  error rate: 16.73%

Attrition_ypred<-predict(rf.Attrition, newdata=dfEmpDetail.test, type='Class')
Attrition_ypred
str(Attrition_ypred)
plot(rf.Attrition)
varImpPlot(rf.Attrition)
table(pred=Attrition_ypred,true=Attrition.test)#20% missclassification error
83/407*100
#Random forest categorical mtry = sqrt(no:of predictors)
rf.Attrition1<-randomForest(as.factor(Attrition)~.,data=dfEmpDetail, subset=train, ntree=500,mtry=6,importance=TRUE)
rf.Attrition1#OOB estimate of  error rate: 13.27%

Attrition_ypred1<-predict(rf.Attrition1, newdata=dfEmpDetail.test, type='Class')
Attrition_ypred1
str(Attrition_ypred)
plot(rf.Attrition1)
varImpPlot(rf.Attrition1)
table(pred=Attrition_ypred1,true=Attrition.test)#18.64% missclassification error
77/413*100

#Bagging
#For Attrition 
#Bagging mtry=no:of predictors
#Best Model
bag.Attrition<-randomForest(as.factor(Attrition)~.,data=dfEmpDetail, subset=train, ntree=500,mtry=34,importance=TRUE)
bag.Attrition#OOB estimate of  error rate: 12.86%

Attrition_ypred<-predict(bag.Attrition, newdata=dfEmpDetail.test, type='Class')
Attrition_ypred
str(Attrition_ypred)
plot(rf.Attrition1)
varImpPlot(rf.Attrition)
table(pred=Attrition_ypred,true=Attrition.test)#18.35% missclassification error
76/414*100



#NaiveBayes

library(e1071)

Attrition.Bayes<-naiveBayes(as.factor(Attrition)~.,data=training_set)
Attrition.Bayes

#Class prediction
Attrition.pred<-predict(Attrition.Bayes, newdata=test_set, type='class')  
Attrition.pred

table(true=test_set$Attrition,pred=Attrition.pred)#26.18%missclassification
61/233*100
#predict raw probabilities
Attrition.pred<-predict(Attrition.Bayes, newdata=test_set, type='raw')  
Attrition.pred




library(e1071)

#Splitting the Data into Training and Test Sets in the ratio 8:2
validation_index<-createDataPartition(na.omit(dfEmpDetail$Attrition),p=0.8,list=FALSE)
#Training set contains 80% of the rows
training_set<-(dfEmpDetail[validation_index,])
#Test Set contains 20% of the rows
test_set<-na.omit(dfEmpDetail[-validation_index,])
#SVM
dfEmpDetail$Attrition<-as.factor(dfEmpDetail$Attrition)
class(dfEmpDetail$Attrition)


SVM_fit<-svm(Attrition~MonthlyIncome+Age+JobRole+PercentSalaryHike+YearsWithCurrManager+JobInvolvement+RelationshipSatisfaction+WorkLifeBalance+TotalWorkingYears+BusinessTravel+Department+EducationField+Gender+JobRole,data=dfEmpDetail, kernel='linear',cost=0.05,scale=FALSE)
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  0.05 
#gamma:  0.03703703704 
#Number of Support Vectors:  484
summary(SVM_fit)

length(validation_index)
#Linear SVM
tune.out=tune(svm,Attrition~MonthlyIncome+Age+JobRole+PercentSalaryHike+YearsWithCurrManager+JobInvolvement+RelationshipSatisfaction+WorkLifeBalance+TotalWorkingYears+BusinessTravel+Department+EducationField+Gender+JobRole,data=dfEmpDetail,kernel='linear',range=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)#gives the best error
bestmod=tune.out$best.model
summary(bestmod)

ypred=predict(bestmod,test_set)
table(predict=ypred,truth=test_set$Attrition)#17% misclassification error

#ANN
library(neuralnet)
nn <- neuralnet(as.factor(Attrition)~JobSatisfaction+PercentSalaryHike+JobLevel+Department+Age+YearsWithCurrManager+WorkLifeBalance+RelationshipSatisfaction+JobInvolvement+TrainingTimesLastYear+MonthlyIncome,
                data=training_set, hidden=c(3,2), rep = 5, 
                algorithm = 'backprop', learningrate = 0.03,
                threshold = 0.01, linear.output = F)

library(neuralnet)
nn <- neuralnet(Attrition~.,data=dfEmpDetail, hidden=c(3,2), rep = 5, 
                algorithm = 'backprop', learningrate = 0.03,
                threshold = 0.01, linear.output = F)

#Unsupervised Learning

# DBSCAN
library(fpc)
newData<-dfEmpDetail[,2:35]
head(newData)
newData$BusinessTravel<-as.integer(newData$BusinessTravel)
newData$Department<-as.integer(newData$Department)

newData$Gender<-as.integer(newData$Gender)
newData$EducationField<-as.integer(newData$EducationField)

newData$JobRole<-as.integer(newData$JobRole)
newData$MaritalStatus<-as.integer(newData$MaritalStatus)
newData$Over18<-as.integer(newData$Over18)
newData$OverTime<-as.integer(newData$OverTime)
#newData$Attrition<-as.integer(newData$Attrition)
hist(dist(newData))
str(newData)
ds <- dbscan(newData, eps=0.45, showplot=1)
ds