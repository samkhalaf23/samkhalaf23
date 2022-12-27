library(MASS)
library(ISLR2)
library(caret) 

##
data= read.csv("binary.csv", header = TRUE)
head(data)

names(data)
dim(data)
#Question A
set.seed(1210)
sample = sample(1:400, replace = FALSE)
train.index = sample[1:(0.7*400)]
test.index = sample[(0.7*400+1):400]
train = data[train.index, ]
test= data[test.index, ]

#logistic regression of admit
?glm
glm.fits = glm(admit ~ gre + gpa + rank, data = train, family = binomial)
summary(glm.fits)
cor(data)
coef(glm.fits)
summary(glm.fits)$coef
#B Confusion Matrix
glm.probs = predict(glm.fits, test,type ="response")
glm.probs[1:10]


glm.class = rep("0",120)          
glm.class[glm.probs > .5] = "1" 
library(caret) 
confusionMatrix(data= as.factor(glm.class), reference=as.factor(test$admit), positive="1") ##prediction accuracy is 0.6917. Logistic regression correctly predicts the admission of applicants 69.2% of the time.

#C LDA 
library(MASS)
lda.fit=lda(admit ~ gre + gpa + rank, data = train)
lda.fit
plot(lda.fit)
lda.pred= predict(lda.fit, test)
sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)
names(lda.pred)    
lda.class = lda.pred$class
lda.sum= confusionMatrix(data= as.factor(lda.class), reference=as.factor(test$admit), positive="1")
lda.sum


#D QDA 
qda.fit = qda(admit ~ gre + gpa +rank, data = train)
qda.fit
qda.pre =predict(qda.fit,test)
qda.pre
qda.class = qda.pre$class
qda.sum = confusionMatrix(data = as.factor(qda.class), reference=as.factor(test$admit), positive="1")
qda.sum

S#E KNN K = 3
library(class)
train.X = train[,-1]
test.x = train[,-1]
train.admit = train[,1]
test.admit=train[,1]
set.seed(1)
knn.pred = knn(train.X, test.x, train.admit, k=3)
knn3.sum = confusionMatrix(data=as.factor(knn.pred),reference=as.factor(test.admit),positive="1")
knn3.sum

#F Naive Bayes 
library(e1071)
nb.fit = naiveBayes(admit ~ gre+gpa+rank, data = train)
nb.fit
nb.class = predict(nb.fit, test, type= "raw")
nb.sum= confusionMatrix(data= as.factor(nb.class), reference=as.factor(test$admit), positive="1")
nb.sum

###########


#Part2 
#Question1
data= read.csv("default.csv", header = TRUE)
head(data)
dim(data)
set.seed(100)
sample = sample(1:10000, replace = FALSE)
train.index = sample[1:(0.7*10000)]
test.index = sample[(0.7*10000+1):10000]
train = data[train.index, ]
test= data[test.index, ]

head(data)
class(data$default) #glm() function can only deal with a response which is numeric or a factor
data$response= 1  #we create a numeric variable, which is 1 for default, 0 for not default
data$response[data$default=="No"]= 0

#Q2 Fit a model using only the training observations 
#(GLM)
glm.fits = glm(response ~ balance + income, data = train, family = binomial)
summary(glm.fits) 

#Q3 
glm.probs = predict(glm.fits, test, type = "response")
glm.probs[1:100]
summary(glm.probs)

#4 
library(pROC)
glm.roc=roc(test$default~ glm.probs, plot = TRUE, print.auc = TRUE)  #ROC curve
auc(glm.roc)
ggroc(glm.roc)

##############
lda.fit1 = lda(default ~ income + balance, data = train)
lda.fit1
lda.probs= predict(lda.fit1,test)
lda.roc= roc(response = test$default, predictor = lda.probs$posterior[,1], plot = TRUE, print.auc = TRUE)
auc(lda.roc)
ggroc(lda.roc)

ggroc(list(glm=glm.roc, lda=lda.roc))

###############
qda.fit1 = qda(default ~ income + balance, data = train)
qda.fit1
qda.probs1= predict(qda.fit1,test)
qda.roc = roc(response = test$default, predictor = qda.probs1$posterior[,1], plot = TRUE, print.auc = TRUE)
auc(qda.roc)
ggroc(qda.roc)


ggroc(list(glm=glm.roc, lda=lda.roc, qda=qda.roc))
