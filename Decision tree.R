library(rpart)
library(rpart.plot)
library(rattle)
library(readr)
library(car)
library(caret)
library(tidyverse)
library(gridExtra)
library(GGally)
Data<-read.csv("C:/Users/santu/OneDrive/Desktop/Iris (1).csv")
dim(Data)
str(Data)
names(Data)
View(Data)
summary(Data)
#Data Audit and Transformation
#Perform exploration
set.seed(123)
plot(Data)
plot(Data$SepalLengthCm)
plot(Data$SepalWidthCm)
plot(Data$PetalLengthCm)
plot(Data$PetalWidthCm)
plot(Data$Species)
table(Data$Species)
# runif function returns a uniform distribution which can be further conditionally split into 75-25 ratio
Data[, 'train'] <- ifelse(runif(nrow(Data)) < 0.75, 1, 0)
trainSet <- Data[Data$train == 1,]
testSet <- Data[Data$train == 0, ]
nrow(trainSet)
ncol(trainSet)
dim(trainSet)
dim(testSet)
trainColNum <- grep('train', names(trainSet))
trainSet <- trainSet[, -trainColNum]
testSet <- testSet[, -trainColNum]
treeFit <- rpart(Species~.,data=trainSet,method = 'class')
print(treeFit)
rpart.plot(treeFit, box.col=c("red", "green"))
#prediction
result<-predict(treeFit,newdata = Data,type="class")
result

