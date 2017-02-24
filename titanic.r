############################
#KAGGLE TITANIC PRACTICE
#
#
#
############################
setwd("/Users/unilectric/R")#set working directory and import datafiles.
getwd()
train<-read.csv("train.csv",header=T,sep = ",")
train
test<-read.csv("test.csv",header=T,sep=",")
test
str(train)
train<-read.csv("train.csv",stringsAsFactors = F)
train
table(train$Survived)
prop.table(table(train$Survived))
test$Survived<- rep(0,418)
submit<-data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
write.csv(submit,file="theyallperish.csv",row.names = F)
summary(train$Sex)
prop.table(table(train$Sex,train$Survived))
prop.table(table(train$Sex,train$Survived),1)
test$Survived<-0
test$Survived[test$Sex=='female']<-1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
summary(train$Age)
train$child<-0
train$child[train$Age<18]<-1
aggregate(Survived~ child+Sex,data = train,FUN = sum)
aggregate(Survived~ child+Sex,data = train,FUN = length)
aggregate(Survived ~ child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
train$Fare2<-'30+'
train$Fare2[train$Fare<30&train$Fare>=20]<-'20-30'
train$Fare2[train$Fare<20&train$Fare>=10]<-'10-20'
train$Fare2[train$Fare<10]<-'10'
aggregate(Survived~Fare2+Pclass+Sex,data = train,FUN = function(x){sum(x)/length(x)})
test$Survived<-0
test$Survived[test$Sex=='female']<-1
test$Survived[test$Sex=='female'& test$Pclass==3 & test$Fare>=20]<-0
submit<-data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
write.csv(submit,file="theyallperish.csv",row.names = F)
library(rpart)
fit<- rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,method="class")
plot(fit)
text(fit)
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
prediction<-predict(fit,test,type="class")
submit<-data.frame(passengerId=test$PassengerId,Survived=prediction)
write.csv(submit,file = "myfirsttree.csv",row.names=F)
fit<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,method="class",control=rpart.control(minsplit = 2,cp=0))  
fancyRpartPlot(fit)
submit<-data.frame(PassengerId=test$PassengerId,Survived=Prediction)
write.csv(submit,file="myfirsttree.csv",row.names=F)
train$Name[1]
train$Name[2]
test$Survived <- NA
combi <- rbind(train,test)
combi$Name<-as.character(combi$Name)
combi$Name[1]
strsplit(combi$Name[1],split = '[,.]')
strsplit(combi$Name[1],split = '[,.]')[[1]][2]
combi$Title<-sapply(combi$Name,FUN = function(x){strsplit(x, split ='[,.]')[[1]][2] })
combi$Title<-sub(' ','',combi$Title)
table(combi$Title)
combi$Title[combi$Title%in%c('Mme','Mlle')]<-'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$familySize<-combi$SibSp+combi$Parch+1
combi$Surname<-sapply(combi$Name,FUN = function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID<-paste(as.character(combi$familySize),combi$Surname,sep="")
combi$FamilyID[combi$familySize<=2]<-'Small'
table(combi$FamilyID)
famIDs<-data.frame(table(combi$FamilyID))
famIDs<-famIDs[famIDs$Freq<=2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1]<-'Small'
combi$FamilyID<-factor(combi$FamilyID)
train<-combi[1:891,]
test<-combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + familySize+ FamilyID,data=train,method="class")
submit<-data.frame(PassengerId=test$PassengerId,Survived=prediction)
write.csv(submit,file="myfirsttree.csv",row.names=F)
sample(1:10,replace=T)
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + familySize,data=combi[!is.na(combi$Age),],method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi)
summary(combi$Embarked)
which(combi$Embarked=='')
combi$Embarked[c(62,830)]="S"
combi$Embarked<-factor(combi$Embarked)
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044]<-median(combi$Fare,na.rm=T)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
install.packages('randomForest')
library(randomForest)
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare+Embarked + Title + familySize + FamilyID2,data=train,importance=TRUE,ntree=2000)
varImpPlot(fit)
boxplot(fit)
