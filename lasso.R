#LASSO
set.seed(12345)
testdata<- read.csv ("HR_comma_sep.csv")
str(testdata)
#traning data matrix

train<- sample(nrow(testdata),0.7*nrow(testdata))
data.train<- testdata[train,]
data.validation<- testdata[-train,]

x<- model.matrix(left~., data.train)[,-1]
y<- data.train$left

#test data matrix
x1<- model.matrix(left~., data.validation)[,-1]
y1<- data.validation$left
grid<- 10^seq(10,-2, length=100)
lasso.mod<-glmnet(x,y, alpha=1, lambda=grid)
plot(lasso.mod)
lasso.mod
cv.out<- cv.glmnet(x,y, alpha=1)
plot(cv.out)
lasso.summary<-summary(lasso.mod)
lasso.summary
plot(cv.out$cvm~cv.out$lambda, xlab="Lambda", ylab="Cross-Validation Error")

#bestlam
bestlam<- cv.out$lambda.min
bestlam
log(bestlam)
lasso.coef<- predict(lasso.mod, type="coefficients", s=bestlam)[1:19,]
lasso.coef
str(x)
lasso.pred<-  predict(lasso.mod, s=bestlam, newx=x1)

#mse for lasso
classified<- ifelse(lasso.pred<0.5,0,1)
CM.full<-table(data.validation$left,classified)
CM.full
Accuracy.full<- (CM.full[1,1]+CM.full[2,2])/sum(CM.full)
Accuracy.full
mse1<- mean((lasso.pred-data.validation$left)^2)
mse1

data.validation$left_new<- lasso.pred
## Make data for a ROC curve
cutoff <- seq(0, 1, length = 100)
fpr <- numeric(100)
tpr <- numeric(100)

## We'll collect it in a data frame.  
roc.table <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)

## TPR is the Sensitivity; FPR is 1-Specificity
for (i in 1:100) {
  roc.table$FPR[i] <- sum(data.validation$left_new > cutoff[i] & data.validation$left == 0)/sum(data.validation$left == 0)
  roc.table$TPR[i] <- sum(data.validation$left_new > cutoff[i] & data.validation$left == 1)/sum(data.validation$left == 1)
}

## The first line plots the Sensitivity against 1-Specificity
plot(TPR ~ FPR, data = roc.table, type = "o",xlab="1 - Specificity",ylab="Sensitivity",col="blue")
## Next line adds the central daigonal
abline(a = 0, b = 1, lty = 2,col="red")
