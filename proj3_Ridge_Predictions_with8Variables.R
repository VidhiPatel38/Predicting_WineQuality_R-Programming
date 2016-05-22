redWine <- read.csv("~/Desktop/Data Mining/Project3/winequality-red.csv", sep=";")
View(redWine)
require(rpart)
require(MASS)
require(caTools)
null=lm(quality~1, data=redWine)
full=lm(quality~., data=redWine)
step(null, scope=list(lower=null, upper=full), direction="forward")
data = redWine[,c(1,5,6,7,9,10,11,12)]
y= data[,8]
train_rows = sample.split(y, SplitRatio=0.8)
head(train_rows)
train = data[ train_rows,]
test=data[!train_rows,]
train2 = train[,-8]
testdata = data.frame(test)
select(lm.ridge(train$quality~.,data=train2,lambda=seq(0,10,0.001)))
ridgereg = lm.ridge(train$quality~.,data=train2,lambda=10)
lm.ridge(train$quality~.,data=train2,lambda=10)
pred_data <-scale(testdata[,1:7],center=F,scale=ridgereg$scales)%*% ridgereg$coef
sumh =0;
#Add the intercept value obtained from lm.ridge function.
tt <-nrow(testdata)
for(i in 1:tt){
sumh = sumh+(pred_data[i]+INTERCEPT_VALUE-testdata[i,8])^2
}
sumg = sumh/tt
sumg
head(pred_data)
pred_data1 = pred_data + INTERCEPT_VALUE
head(pred_data1)
test2 = testdata[,8]
pdf("part3Ridge.pdf")
plot(test2,pred_data1)
dev.off()
