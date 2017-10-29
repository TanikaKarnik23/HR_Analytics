#Classification Tree
classificationTree <- tree(left~., data = data.train)
summary(classificationTree)
plot(classificationTree)
text(classificationTree, pretty=0)
yhat=predict(classificationTree,newdata=data.validation, type="class")
treeConfusionMatrix <- table(data.validation$left, yhat)
treeConfusionMatrix
classificationTreeAccuracy <- sum(treeConfusionMatrix[1,1],treeConfusionMatrix[2,2])/sum(treeConfusionMatrix[,])
classificationTreeAccuracy

# Pruning still gives us full plot
cv.hrData <- cv.tree(classificationTree,FUN = prune.misclass, K = 10)
names(cv.hrData)
cv.hrData

par(mfrow = c(1,2))
plot(cv.hrData$size,cv.hrData$dev,type = "b")
plot(cv.hrData$k,cv.hrData$dev,type = "b")
prune.classificationTree <- prune.misclass(classificationTree,best=cv.hrData$size[which.min(cv.hrData$dev)])  #this is still giving us full plot
plot.new()
plot(prune.classificationTree)
text(prune.classificationTree, pretty=0)

#Pruning by checking error rate
nodes <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
accuracyRate <- c(0,0,0,0,0,0,0,0,0,0)
for(i in 2:11){
  prune.classificationTree <- prune.misclass(classificationTree, best=i)
  yhat.prune <- predict(prune.classificationTree, newdata=data.validation, type="class")
  prune.treeConfusionMatrix <- table(data.validation$left, yhat.prune)
  prune.treeConfusionMatrix
  accuracyRate[i-1] <- sum(prune.treeConfusionMatrix[1,1],prune.treeConfusionMatrix[2,2])/sum(prune.treeConfusionMatrix[,])
}
accuracyRate
plot(nodes, accuracyRate, xlab="Number of nodes", ylab="Accuracy")
lines(nodes, accuracyRate)

prune.classificationTree <- prune.misclass(classificationTree, best=5)
plot(prune.classificationTree)
text(prune.classificationTree, pretty=0)
yhat.prune <- predict(prune.classificationTree, newdata=data.validation, type="class")
prune.treeConfusionMatrix <- table(data.validation$left, yhat.prune)
prune.treeConfusionMatrix
accuracyRate <- sum(prune.treeConfusionMatrix[1,1],prune.treeConfusionMatrix[2,2])/sum(prune.treeConfusionMatrix[,])
accuracyRate
