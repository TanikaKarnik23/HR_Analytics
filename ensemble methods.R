#BAgging
bag.hr=randomForest(left~.,data=data.train,mtry=9,importance=TRUE)
bag.hr
yhat.bag = predict(bag.hr,newdata=data.validation)
importance(bag.hr)
varImpPlot(bag.hr)
baggingConfusionMatrix <- table(data.validation$left, yhat.bag)
baggingConfusionMatrix
baggingAccuracy <- sum(baggingConfusionMatrix[1,1],baggingConfusionMatrix[2,2])/sum(baggingConfusionMatrix[,])
baggingAccuracy

#Random Forest
rf.hr=randomForest(left~.,data=data.train,mtry=4,importance=TRUE)
rf.hr
yhat.rf = predict(rf.hr,newdata=data.validation)
importance(rf.hr)
varImpPlot(rf.hr)
rfConfusionMatrix <- table(data.validation$left, yhat.rf)
rfConfusionMatrix
rfAccuracy <- sum(rfConfusionMatrix[1,1],rfConfusionMatrix[2,2])/sum(rfConfusionMatrix[,])
rfAccuracy
