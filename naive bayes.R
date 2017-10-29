#Naive Bayes
datafile<- read.csv("HR_comma_sep.csv")
#datafile[,1] <- NULL
set.seed(12345)
str(datafile)

datafile$Work_accident<- as.factor(datafile$Work_accident)
datafile$left<- as.factor(datafile$left)
datafile$promotion_last_5years<- as.factor(datafile$promotion_last_5years)
str(datafile)


train<- sample(nrow(datafile),0.7*nrow(datafile))
data.train<- datafile[train,]
data.validation<- datafile[-train,]

#install.packages("caret")
library("caret")
#install.packages("e1071") 
library("e1071")

naivemodel <- naiveBayes(left~., data=data.train)
naivemodel
prediction <- predict(naivemodel, newdata = data.validation[,-7])
cm<-table(data.validation$left,prediction,dnn=list('actual','predicted'))
cm
naivemodel$apriori


Accuracy.full<- (cm[1,1]+cm[2,2])/sum(cm)
Accuracy.full

predicted.probability <- predict(naivemodel, newdata = data.validation[,-7], type="raw")
#
# The first column is class 0, the second is class 1
PL <- as.numeric(data.validation$left)-1
prob <- predicted.probability[,2]
df1 <- data.frame(prediction, PL, prob)
#
#
df1S <- df1[order(-prob),]
df1S$Gains <- cumsum(df1S$PL)
plot(df1S$Gains,type="n",main="Lift Chart",xlab="Number of Cases",ylab="Cumulative Success")
lines(df1S$Gains,col="blue")
abline(0,sum(df1S$PL)/nrow(df1S),lty = 2, col="red")
