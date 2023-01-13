rm(list=ls())
#install.packages('ISLR2')
library(ISLR2)
library(caret)
library(pROC)
library(ROCR)
library(MASS)
library(broom)
library(e1071)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(dplyr)
library(stats)
library(GGally)
library(tidyr)
library(tree)
library(randomForest)
library(plyr)
library(readxl)
library("readr")

#attach(data)


############reading data and omitting missing data##################

setwd("C:/Users/Mohammed/Desktop/my data/SDSU/SDSU/720 BDA/Project")

attach(data)
data <- read.csv("~/Desktop/data.csv")


#Exploratory Analysis (statistics)

#1. Statistics for Passengers
sum(data$Gender== 'Male') / nrow(data)  ##49% are men
sum(data$Gender== 'Female') / nrow(data) #51% are women


table(data$Customer.Type)  #we have 2 types of loyal and disloyal customer
sum(data$Customer.Type == 'disloyal Customer') / nrow(data)
sum(data$Customer.Type == 'Loyal Customer') / nrow(data) #82% are loyal

length(which(data$Age < 18)) / nrow(data) #8% are under 18 
length(which(data$Age < 25 & data$Age > 17)) / nrow(data) #11% between 18-24

length(which(data$Age < 35 & data$Age > 24)) / nrow(data) #19% between 25-34
length(which(data$Age < 45 & data$Age > 34)) / nrow(data) #23% between 35-44
length(which(data$Age < 55 & data$Age > 44)) / nrow(data) #21% between 45-54
length(which(data$Age < 65 & data$Age > 54)) / nrow(data) #13% between 55-64
length(which(data$Age > 64)) / nrow(data) #5% 65 and above 




#2. Statistics for Flight 
#short-haul routes as being shorter than 600–800 nmi (1,100–1,500 km), long-haul as being longer than 2,200–2,600 nmi (4,100–4,800 km), and medium-haul as being in-between.

length(which(data$Flight.Distance < 600)) / nrow(data) #38% short haul trips
length(which(data$Flight.Distance >= 600 & data$Flight.Distance < 2100)) / nrow(data) #42% Medium Haul
length(which(data$Flight.Distance >= 2100)) / nrow(data) #20% Medium Haul
sum(data$Class== 'Business') / nrow(data) #44% economy and 48% business
summary(data$Departure.Delay.in.Minutes)
summary(data$Arrival.Delay.in.Minutes)

sum(data$`Type of Travel`== 'Personal Travel') / nrow(data) #30% trips are personal


# Categorizing certain columns for classification purposes
#visualize_data is only used for visualization purposes


visualize_data <- data %>% mutate(AgeGroup =
                                    case_when(Age <= 18 ~ "Under 18",
                                              Age < 25 & Age >=18 ~ "18 to 24",
                                              Age < 35 & Age >=25 ~ "25 to 34",
                                              Age < 45 & Age >=35 ~ "35 to 44",
                                              Age < 55 & Age >=45 ~ "45 to 54",
                                              Age < 65 & Age >=55 ~ "55 to 64",
                                              Age >= 65 ~ "Above 65",
                                    )
)


visualize_data <- visualize_data %>% mutate('Flight Length' =
                                              case_when( Flight.Distance <= 600 ~ "Short Haul", 
                                                         Flight.Distance < 2100 & Flight.Distance >= 600 ~ "Medium Haul",
                                                         Flight.Distance >=2100 ~ "Long Haul",
                                              )
)


visualize_data <- visualize_data %>% mutate('Arrival Delay' =
                                              case_when( Arrival.Delay.in.Minutes <= 15 ~ "No delay",
                                                         Arrival.Delay.in.Minutes < 61 & Arrival.Delay.in.Minutes > 15 ~ "Minor Delay",
                                                         Arrival.Delay.in.Minutes < 300 & Arrival.Delay.in.Minutes > 60 ~ "Considerable Delay",
                                                         Arrival.Delay.in.Minutes >=300 ~ "Extreme delay",
                                              )
)


visualize_data <- visualize_data %>% mutate('Departure Delay' =
                                              case_when( Departure.Delay.in.Minutes <= 15 ~ "No delay",
                                                         Departure.Delay.in.Minutes < 61 & Departure.Delay.in.Minutes > 15 ~ "Minor Delay",
                                                         Departure.Delay.in.Minutes < 300 & Departure.Delay.in.Minutes > 60 ~ "Considerable Delay",
                                                         Departure.Delay.in.Minutes >=300 ~ "Extreme delay",
                                              )
)


#Pre-Processing for visualization

str(visualize_data)

visualize_data[sapply(visualize_data, is.character)] <- lapply(visualize_data[sapply(visualize_data, is.character)], 
                                                               as.factor)

visualize_data[sapply(visualize_data, is.numeric)] <- lapply(visualize_data[sapply(visualize_data, is.numeric)], 
                                                             as.factor)

visualize_data$Departure.Delay.in.Minutes <- as.numeric(visualize_data$Departure.Delay.in.Minutes)
visualize_data$Arrival.Delay.in.Minutes <- as.numeric(visualize_data$Arrival.Delay.in.Minutes)
visualize_data$Age <- as.numeric(visualize_data$Age)

str(visualize_data)

visualize_data <- na.omit(visualize_data)


# Visualization

library(ggplot2)
ggplot(visualize_data, aes(x=Gender, fill=satisfaction)) + geom_bar() + ggtitle("Satisfaction based on Gender")

ggplot(visualize_data, aes(x=Class, fill=satisfaction)) + geom_bar() + ggtitle("Satisfaction based on Flight Class")

ggplot(visualize_data, aes(x=`Flight Length`, fill=satisfaction)) + geom_bar() + ggtitle("Satisfaction based on Flight Duration")


ggplot(visualize_data, aes(x=Type.of.Travel, fill=satisfaction)) + geom_bar() + ggtitle("Satisfaction based on Type of Travel")


ggplot(visualize_data, aes(AgeGroup, fill=satisfaction)) + geom_bar() + ggtitle("Satisfaction based on Type of Travel")


#### Male and female - how many males and females/ data is balanced

table(data$Gender)

table <- table(data$Gender, data$satisfaction)
test <- chisq.test(table)
test

#H0: (null hypothesis) The two variables are independent.
#H1: (alternative hypothesis) The two variables are not independent.  
# we reject the null and establish dependence between gender and satisfaction

table <- table(data$satisfaction, data$Customer.Type)
test <- chisq.test(table)
test

#H0: (null hypothesis) The two variables are independent.
#H1: (alternative hypothesis) The two variables are not independent.  
# we reject the null and establish dependence between gender and satisfaction

ggcorr(data, label = TRUE) # satisfaction needs to be numeric


#### Deleting NAs and columns not used in the model

sum(!complete.cases(data))
data <- data[,-c(1,2)]
data <- na.omit(data)
sum(!complete.cases(data))



############Cross Validation##################

set.seed(1210)
dim(data)
sample = sample(1:129487, replace = FALSE)
train.index = sample[1:(0.7*129487)]
test.index = sample[(0.7*129487+1):129487]
train = data[train.index, ]
test= data[test.index, ]
test.y = test$satisfaction



############Logistic Regression##################

set.seed(5678)
index <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
glm.fits = glm(as.factor(satisfaction) ~ ., data = train, family = binomial)
summary(glm.fits)
glm.prob = predict(glm.fits,test, type = "response")
classes <- ifelse(glm.prob > 0.5, "satisfied", "neutral or dissatisfied")
test$classes <- classes 
glm.sum=confusionMatrix(data=as.factor(classes), reference= as.factor(test$satisfaction), positive = "satisfied")
glm.sum #87 accuracy rate
coef(glm.fits)
roc_score=roc(test$satisfaction, glm.prob) #AUC score
plot(roc_score ,main ="ROC ", colorize=TRUE, print.cutoffs.at=seq(0,1,by =0.2), text.adj=c(-0.3,1.6), text.cex = 1.2, xlims=c(0,-1))
auc(roc_score) #97%

############Naive Bayes##################
attach(train)
nb.fit = naiveBayes(satisfaction ~., data = train, family = binomial)
nb.fit
nb.class = predict(nb.fit, test, type = "class")
nb.prob = predict(nb.fit, test, type = "raw")
nb.sum= confusionMatrix(data= as.factor(nb.class), reference=as.factor(test$satisfaction),positive = "satisfied")
nb.sum #85% Accuracy Rate 
roc_score=roc(test$satisfaction, nb.prob) #AUC score
plot(roc_score ,main ="ROC ", colorize=TRUE, print.cutoffs.at=seq(0,1,by =0.2), text.adj=c(-0.3,1.6), text.cex = 1.2, xlims=c(0,-1))
auc(roc_score) #92%


############Decision Tree##################

set.seed(720)

sample = sample(1:129487, replace = FALSE)
train.index = sample[1:(0.7*129487)]
test.index = sample[(0.7*129487+1):129487]
train = data[train.index, ]
test= data[test.index, ]
test.y = as.factor(test$satisfaction)


tree = tree(as.factor(satisfaction) ~ ., data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0)
#prune the tree
tree.pred <- predict(tree, test, type = "class")
cm = confusionMatrix(data= as.factor(tree.pred), reference=as.factor(test.y))
cm

tree.probs <- predict(tree, test)[,2]
tree.roc <- roc(test.y, tree.probs)
print(tree.roc)
plot(tree.roc)

############Random Forest##################

attach(train)
rf = randomForest(as.factor(satisfaction) ~ ., data = train)
summary(rf)
plot(rf)
rf_pred <- predict(rf, test, type = "class")
test$rf_pred <- rf_pred
cm = confusionMatrix(data= as.factor(rf_pred), reference=as.factor(test.y))
cm

rf_pred <- predict(rf, test, type = "prob")[, 2]
tree.roc <- roc(rf_pred, test.y)
print(tree.roc)
plot(tree.roc)

varimp <- as.matrix(varImp(rf))
varimp <- varimp[order(varimp),]
barplot(t(varimp), horiz=TRUE, main = "Variable Importance", las=2, cex.names = 0.5)
