setwd("~/Desktop/Data Mining/Project3")
redWine <- read.csv("~/Desktop/Data Mining/Project3/winequality-red.csv", sep=";")
View(redWine)
require(rpart)
null=lm(quality~1, data=redWine)
full=lm(quality~., data=redWine)
step(null, scope=list(lower=null, upper=full), direction="forward")
data = redWine[,c(1,5,6,7,9,10,11,12)]
require(caTools)
y= data[,8]
train_rows = sample.split(y, SplitRatio=0.8)
head(train_rows)
train = data[ train_rows,]
test=data[!train_rows,]
lm.fit = lm(quality~.,data=train)
testdata = data.frame(test)
pred_data = predict(lm.fit, testdata)
test_quality = test[,8]
error <- sqrt((sum((test_quality-pred_data)^2))/length(test_quality))
error
pdf("Proj3_Linear_Predictions.pdf")
plot(test_quality,pred_data)
dev.off()


