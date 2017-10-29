#logistic regression
logisticModel <- glm(left~., family="binomial", data = data.train)
summary(logisticModel)
logisticProbabilityPrediction <- predict(logisticModel, newdata = data.validation, type="response")
cutoffVector <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)

#plotting accuracy vs cutoff
logisticAccuracy <- seq(0,0,length=8)
for(i in 1:8){
  logisticPrediction <- ifelse(logisticProbabilityPrediction > cutoffVector[i], 1, 0)
  logisticConfusionMatrix <- table(data.validation$left, logisticPrediction)
  logisticConfusionMatrix
  logisticAccuracy[i] <- (logisticConfusionMatrix[1,1] + logisticConfusionMatrix[2,2])/sum(logisticConfusionMatrix)
}
plot(cutoffVector, logisticAccuracy, xlab="Cutoff", ylab="Accuracy Rate")
lines(cutoffVector, logisticAccuracy)
cutoffVector[which.max(logisticAccuracy)]

#Make data for a ROC curve
cutoff <- seq(0, 1, length = 100)
fpr <- numeric(100)
tpr <- numeric(100)

## We'll collect it in a data frame.  
roc.table1 <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)

## TPR is the Sensitivity; FPR is 1-Specificity
for (i in 1:100) {
  roc.table1$FPR[i] <- sum(logisticProbabilityPrediction > cutoff[i] & data.validation$left == 0)/sum(data.validation$left == 0)
  roc.table1$TPR[i] <- sum(logisticProbabilityPrediction > cutoff[i] & data.validation$left == 1)/sum(data.validation$left == 1)
}

## The first line plots the Sensitivity against 1-Specificity
plot(TPR ~ FPR, data = roc.table, type = "o",xlab="1 - Specificity",ylab="Sensitivity",col="blue")
## Next line adds the central daigonal
abline(a = 0, b = 1, lty = 2,col="red")
