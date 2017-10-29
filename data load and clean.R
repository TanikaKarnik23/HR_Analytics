#load all libraries
install.packages("ggplot2")
install.packages("reshape2")
install.packages("lattice")  # Used for Data Visualization
install.packages("caret")    # for data pre-processing
install.packages("pROC")     # for ROC Curves
install.packages("ipred")    # for bagging and  k fold cv
install.packages("e1071")    # for SVM
install.packages("corrplot")
install.packages("tree")
install.packages("MASS")
install.packages("randomForest")
install.packages("gbm")
install.packages("glmnet")
install.packages("leaps")
install.packages("ROCR")
library(ROCR)
library(leaps)
library(boot)
library(glmnet)
library(lattice)  # Used for Data Visualization
require(caret)    # for data pre-processing
require(pROC)     # for ROC Curves
library(ipred)    # for bagging and  k fold cv
library(e1071)    # for SVM
library(ggplot2)
library(reshape2)
library(corrplot)
library(tree)
library(MASS)
library(randomForest)
library(gbm)


#read and cleanup data
data <- read.csv("HR_comma_sep.csv")


#summary of data
summary(data)


#correlation plot
plot.new()
dev.off()
cor(data[,1:8])
corrplot(cor(data[,1:8]), method="color", type="lower")
data$Work_accident <- as.factor(data$Work_accident)
data$left <- as.factor(data$left)
data$promotion_last_5years <- as.factor(data$promotion_last_5years)
data$salary<-ordered(data$salary,levels=c("low","medium","high"))



#sampling
set.seed(12345)
train<- sample(nrow(data),0.7*nrow(data))
data.train<- data[train,]
data.validation <- data[-train,]
