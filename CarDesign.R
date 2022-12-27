rm(list=ls())
library(faraway)
data(seatpos)
head(seatpos)
dim(seatpos)
seatpos = na.omit(seatpos)
dim(seatpos)
sum(is.na(seatpos$hipcenter))
sum(is.na(seatpos))
dim(seatpos)
#############
library(leaps)
regfit.full = regsubsets(hipcenter ~., seatpos)
regfit.full
summary(regfit.full)
###
regfit.full = regsubsets(hipcenter ~ ., data = seatpos, nvmax = 8)
reg.summary = summary(regfit.full)
####
names(reg.summary)
########
reg.summary$adjr2
reg.summary$rsq
reg.summary$bic
###############
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
#####
which.max(reg.summary$adjr2)
points(3,reg.summary$adjr2[3],col = "red", cex = 2, pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(1,reg.summary$bic[1],col = "red", cex = 2, pch = 20)
coef(regfit.full, 1)
plot (regfit.full , scale ="bic")

#############
##Problem2
library(MASS)
data(Boston)
head(Boston)
dim(Boston)
set.seed(1251)
sample = sample(1:506,replace=FALSE)
train.index = sample[1:(0.1*506)]
test.index = sample[(0.9*506+1):506]
train = Boston[train.index,]
test= Boston[test.index,]
#########
#ProblemB
lm= lm(medv~.,data=train)
summary(lm)
lm.pred=predict(lm, newdata = test)
test.y = test$medv
mean((lm.pred - test.y)^2)
mean(lm$residuals^2)
###ProblemC
x = model.matrix(medv ~., Boston)[,-1]
y =Boston$medv
train.x= x[train.index,]
test.x= x[-train.index,]
train.y= y[train.index]
test.y = y[-train.index]
#############Ridge Regression
library(glmnet)
grid = 10^seq(10, -2, length = 100) 
set.seed(19)
cv.out = cv.glmnet(train.x,train.y,alpha =0 )
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
#cv.glmnet is used to select the best lambda. glmnent is used to fit a ridge regression 
ridge.mod = glmnet(train.x, train.y, alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s = bestlam, newx = test.x) #prediction
mean((ridge.pred - test.y)^2) #test error
#######
out = glmnet(x,y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:14,]
########
lasso.mod = glmnet(train.x, train.y, alpha = 1, lambda = grid)
plot(lasso.mod)
set.seed(19)
cv.out = cv.glmnet(train.x, train.y, alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = test.x)
mean((lasso.pred - test.y)^2)
lasso.coef = predict(lasso.mod, type = "coefficients", s = bestlam)
lasso.coef
bestlam
#######################

########Problem3#############
attach(Boston)
fit = lm(nox ~poly(dis,3,raw = T), data = Boston)
summary(fit)
coef(summary(fit))
dislims = range(dis)
dis.grid = seq(from = dislims[1], to = dislims[2])
preds = predict(fit, newdata= list(dis = dis.grid), se = TRUE)
se.bands = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
##################
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(dis,nox, xlim = dislims, cex = .5, col ="darkgrey")
title("Cubic Polynomial", outer = T)
matlines(dis.grid, se.bands, lwd =1, col = "red", lty = 3)
lines(dis.grid, preds$fit, lwd = 2, col = "red")
########################
fit.1 = lm(nox ~ dis, data = Boston)
fit.2 = lm(nox ~ poly(dis, 2, raw = T), data = Boston)
fit.3 = lm(nox ~ poly(dis, 3, raw = T), data = Boston)
fit.4 = lm(nox ~ poly(dis, 4, raw = T), data = Boston)
fit.5 = lm(nox ~ poly(dis, 5, raw = T), data = Boston)

pred1 = predict(fit.1, newdata = list(dis = dis.grid))
pred2 = predict(fit.2, newdata = list(dis = dis.grid))
pred3 = predict(fit.3, newdata = list(dis = dis.grid))
pred4 = predict(fit.4, newdata = list(dis = dis.grid))
pred5 = predict(fit.5, newdata = list(dis = dis.grid))

plot(dis, nox, cex = .5, col = "darkblue", xlim = dislims)
title("Polynomial Fit")
lines(dis.grid, pred1, lwd = 2, col =1)
lines(dis.grid, pred2, lwd = 2, col =2)
lines(dis.grid, pred3, lwd = 2, col = 3)
lines(dis.grid, pred4, lwd = 2, col = 4)
lines(dis.grid, pred5, lwd = 2, col = 5)
legend.txt= c("degree=1", "degree=2", "degree=3", "degree=4", "degree=5")
legend("topright", legend.txt, col=1:5, lty=rep(1,5))

summary(fit.1)$r.squared
summary(fit.2)$r.squared
summary(fit.3)$r.squared
summary(fit.4)$r.squared
summary(fit.5)$r.squared

sum(fit.1$residuals^2)
sum(fit.2$residuals^2)
sum(fit.3$residuals^2)
sum(fit.4$residuals^2)
sum(fit.5$residuals^2)

summary(fit.1)$adj.r.squared
summary(fit.2)$adj.r.squared
summary(fit.3)$adj.r.squared
summary(fit.4)$adj.r.squared
summary(fit.5)$adj.r.squared

##############

#####Problem C
library(splines)
fit = lm(nox ~ bs(dis, knots =c(4,6,8)), data = Boston)
pred= predict(fit, newdata = list(dis = dis.grid, se = T))
plot(dis, nox, col = "gray")
lines(dis.grid, preds$fit, lwd = 2)
lines(dis.grid, preds$fit + 2 * preds$se, lty = "dashed")
lines(dis.grid,preds$fit - 2* preds$se, lty = "dashed")
summary(fit)$r.squared
mean((fit1$residuals)^2)
summary(fit)$adj.r.squared
##R^2
fit1 = lm(nox ~ bs(dis, knots = c (4,6,8), degree = 1), data = Boston)
summary(fit1)$r.squared
pred1 = predict(fit1, newdata = list( dis = dis.grid))
mean((fit1$residuals)^2)


