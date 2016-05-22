redWine <- read.csv("~/Desktop/Data Mining/Project3/winequality-red.csv", sep=";")
View(redWine)
require(rpart)
require(caTools)
require(MASS)
y = redWine[,12]
train_rows = sample.split(y, SplitRatio = 0.8)
train = redWine[train_rows,]
test = redWine[!train_rows,]
train2= train[,-12]
select(lm.ridge(train$quality~.,data=train2,lambda = 10))
ridgereg = (lm.ridge(train$quality~.,data=train2,lambda = 10))
lm.ridge(train$quality~.,data=train2,lambda = 10)
pred_data <- scale(test[,1:11],center = F,scale = ridgereg$scales)%*%ridgereg$coef
sumh<-0
tt <- nrow(test)
#Add the intercept value obtained from lm.ridge function.
for(i in 1:tt){
     sumh=sumh+(pred_data[i]+INTERCEPT_VALUE-test[i,12])^2 
}
error = sumh/tt
error
test2= test[,12]
pred_data1 = pred_data + INTERCEPT_VALUE
pdf("Part3_ridge1.pdf")
plot(test2,pred_data1)
dev.off()