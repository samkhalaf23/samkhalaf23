rm(list=ls())
library(ISLR2)
data(OJ)
summary(OJ)
dim(OJ)

#Question A
OJ$STORE = as.factor(OJ$STORE)
OJ$StoreID = as.factor(OJ$StoreID)
set.seed(1949)
n = nrow(OJ)
train.index=sample(1:1070, 1070/2)
train= OJ[train.index,]
test= OJ[-train.index,]
y.test = OJ$Purchase[-train.index]
tree = tree(Purchase~., data = train)
plot(tree)
text(tree, pretty = 0)
summary(tree)
#QuestionB 
pred = predict(tree, newdata = test, type= 'class')
pred
confusionMatrix(data=pred, reference = y.test)
yprob.tree = predict(tree, test)
##Question C 
set.seed(984)
tree = tree(Purchase~., data = train)
cv.oj = cv.tree(tree, FUN = prune.misclass)
cv.oj
prune= prune.misclass(tree, best=4)
plot(prune)
text(prune)
plot(cv.oj$size, cv.oj$dev, type ='b')
##QuestionD 
pred1 = predict(prune, newdata = test, type ='class')
pred1
confusionMatrix(data=pred1, reference = y.test)

#QuestionE
confusionMatrix(data=pred, reference = y.test)
confusionMatrix(data=pred1, reference = y.test)

###QuestionF
library(pROC)
tree.roc = roc(response= y.test, predictor=yprob.tree[,2])
tree.roc
auc(tree.roc)
ggroc(tree.roc)
pred.prob = predict(prune, newdata = test)
prune.roc = roc(response= y.test, predictor=pred.prob[,2])
ggroc(list(tree.roc=tree.roc, prune.roc=prune.roc))
auc(prune.roc)


####Part 2 
rm(list=ls())
data(Carseats)
summary(Carseats)
n= nrow(Carseats)
set.seed(47)
dim(Carseats)
train.index=sample(1:n, n*.70)
train= Carseats[train.index,]
test= Carseats[-train.index,]
y.test = Carseats$Sales[-train.index]

##Question A
library(ISLR2)
library(tree)
library(pROC)

tree = tree(Sales~., data =train)
summary(tree)
plot(tree)
text(tree, pretty = 0)
yhat = predict(tree, newdata = test)
plot(y.test, yhat)
abline(a = 0, b = 1)
mean((y.test - yhat)^2)


#Question B 
set.seed(47)
cv.carseat = cv.tree(tree, FUN = prune.tree)
cv.carseat
plot(cv.carseat$size, cv.carseat$dev, type = 'b')
cv.carseat$size[which.min(cv.carseat$dev)]

prune = prune.tree(tree, best =16)
plot(prune)
text(prune)

yhat1 = predict(prune, newdata = test)
plot(y.test, yhat1)
abline(a= 0, b= 1)
mean((y.test - yhat1)^2)

####QuestionC 
install.packages("randomForest")
library(randomForest)
set.seed(47)
bag = randomForest(Sales~., data = train, mtry = 13, importance = TRUE)
bag
yhat.bag = predict(bag, newdata = test)
plot(y.test, yhat.bag)
abline(0,1)
mean((y.test - yhat.bag)^2)
importance(bag)
varImpPlot(bag)

####QuestionD
set.seed(47)
rf = randomForest(Sales~., data = train, mtry = 4, importance = TRUE)
yhat.rf = predict(rf, newdata = test)
importance(rf)
varImpPlot(rf)
mean((y.test - yhat.rf)^2)
