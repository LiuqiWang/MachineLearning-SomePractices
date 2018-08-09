# Support Vector Machines and Splines

rm(list=ls())
##############################################
##############################################
################ Section 1 ###################
##############################################
##############################################
# Support Vector Machines
# use OJ data set from ISLR package
#####################
#### Part a
#####################
installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}
installIfAbsentAndLoad('ISLR')
installIfAbsentAndLoad('e1071')
library(MASS)
library(class)
library(ISLR)
library(boot) #to use cv.glm()
attach(Wage)
library(splines)

set.seed(5082)
n = dim(OJ)[1] # 1070 data points, with 18 dimensions. 
train_inds = sample(1:n,800) # create train and test indices to help selecting data from OJ. 
test_inds = (1:n)[-train_inds]
#####################
#### Part b
#####################
d <- data.frame(OJ) # make the dataset OJ into a dataframe for easy viewing and editing 
train <- d[c(train_inds),] # select the data indicated by train_inds and make it train data
test <- d[c(test_inds),] # same as train data, make the rest as test data
svm.train <- svm(Purchase ~., kernel="linear", data = train, cost=0.01, scale=FALSE) 
summary(svm.train)
#### Comment: A linear kernel is used with cost=0.01, and there are 614 support vectors. 309 in one class and 305 in the other class. 
#####################
#### Part c 
#####################
train.pred = predict(svm.train, train)
table(truth=train$Purchase, predict=train.pred)
train.pred ==! train$Purchase
(train.error=sum(train.pred != train$Purchase)/length(train$Purchase))
test.pred = predict(svm.train, test) 
table(truch=test$Purchase, predict=test.pred)
(test.error=sum(test.pred != test$Purchase)/length(test$Purchase))

#####################
#### Part d
#####################
tune.out <- tune(svm,Purchase ~ ., data=train, kernel="linear", ranges=list(cost=seq(0.01, 10, 1)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)
#####################
#### Part e
#####################
train.pred1 = predict(bestmod, train)
table(truth=train$Purchase, predict=train.pred1)
(train.error1=sum(train.pred1 != train$Purchase)/length(train$Purchase))
test.pred1 = predict(bestmod, test)
table(truch=test$Purchase, predict=test.pred1)
(test.error1=sum(test.pred1 != test$Purchase)/length(test$Purchase))

#####################
#### Part f
#####################
# b) fit svm, cost=0.01, print summary, describe results
svm.f = svm(Purchase~., data=train, kernel="radial", cost=0.01) 
summary(svm.f)
#### Comment: A radial kernel is used with cost=0.01, and there are 613 support vectors. 308 in one class and 305 in the other class. 
# c) training & test error rates
train.pred.f = predict(svm.f, train)
table(truth=train$Purchase, predict=train.pred.f)
(train.error.f=sum(train.pred.f != train$Purchase)/length(train$Purchase))
test.pred.f = predict(svm.f, test) 
table(truch=test$Purchase, predict=test.pred.f)
(test.error.f=sum(test.pred.f != test$Purchase)/length(test$Purchase))
# d) tune(), cost rang from 0.01-10
tune.out.f <- tune(svm,Purchase ~ ., data=train, kernel="radial", ranges=list(cost=seq(0.01, 10, 1)))
summary(tune.out.f)
bestmod.f <- tune.out.f$best.model
summary(bestmod.f)
# e) training & test error rates of bestmodel
train.pred.f1 = predict(bestmod.f, train)
table(truth=train$Purchase, predict=train.pred.f1)
(train.error.f1=sum(train.pred.f1 != train$Purchase)/length(train$Purchase))
test.pred.f1 = predict(bestmod.f, test)
table(truch=test$Purchase, predict=test.pred.f1)
(test.error.f1=sum(test.pred.f1 != test$Purchase)/length(test$Purchase))

#####################
#### Part g
#####################
# b) fit svm, cost=0.01, print summary, describe results
svm.g = svm(Purchase~., data=train, kernel="polynomial", cost=0.01, degree=2) 
summary(svm.g)
#### Comment: A radial kernel is used with cost=0.01, and there are 614 support vectors. 309 in one class and 305 in the other class. 
# c) training & test error rates
train.pred.g = predict(svm.g, train)
table(truth=train$Purchase, predict=train.pred.g)
(train.error.g=sum(train.pred.g != train$Purchase)/length(train$Purchase))
test.pred.g = predict(svm.g, test) 
table(truch=test$Purchase, predict=test.pred.g)
(test.error.g=sum(test.pred.g != test$Purchase)/length(test$Purchase))
# d) tune(), cost rang from 0.01-10
tune.out.g <- tune(svm,Purchase ~ ., data=train, kernel="polynomial", ranges=list(cost=seq(0.01, 10, 1)))
summary(tune.out.g)
bestmod.g <- tune.out.g$best.model
summary(bestmod.g)
# e) training & test error rates of bestmodel
train.pred.g1 = predict(bestmod.g, train)
table(truth=train$Purchase, predict=train.pred.g1)
(train.error.g1=sum(train.pred.g1 != train$Purchase)/length(train$Purchase))
test.pred.g1 = predict(bestmod.g, test)
table(truch=test$Purchase, predict=test.pred.g1)
(test.error.g1=sum(test.pred.g1 != test$Purchase)/length(test$Purchase))

#####################
#### Part h
#####################
test.error1
test.error.f1
test.error.g1
### Comment: Kernal="radio" gives the best result.


##############################################
##############################################
################ Section 2 ###################
##############################################
##############################################
# Polnomial and Step Function Regression 
# use Wage dataset from ISLR package.
# perform polynomial regression to predict wage usign age with polynomial degrees from 1-10.
# then use 10 fold cross validation to select the optimal degree d from those 10 choises. 
#####################
#### Part b
#####################
set.seed(5082)
cv.error=rep(0,10)
for (i in 1:10){
  glm.fit=glm(wage~poly(age,i), data=Wage)
  cv.error[i]=cv.glm(Wage, glm.fit, K=10)$delta[1]
}
cv.error
which.min(cv.error)
#####################
#### Part c
#####################
plot(cv.error)
lines(cv.error)
points(which.min(cv.error),cv.error[which.min(cv.error)],col="red")
#####################
#### Part d
#####################
plot(age,wage,col="gray") # plot the age/wage data
agelims=range(age) 
#age.grid=seq(from=1,to=100) 
age.grid=seq(from=agelims[1],to=agelims[2],length.out = 100)
glm.best=glm(wage~poly(age,9), data=Wage)
pred=predict(glm.best,newdata=list(age=age.grid),se=T)
lines(age.grid,pred$fit,lwd=2, col="red")
#####################
#### Part e 
#####################
set.seed(5082)
cv.error.c=rep(0,12)
for (i in 1:12){
  Wage$step=cut(age,i+1)
  cut.fit=glm(wage~step, data=Wage)
  cv.error.c[i]=cv.glm(Wage, cut.fit, K=10)$delta[1]
}
cv.error.c
which.min(cv.error.c)+1

#####################
#### Part f
#####################
plot(cv.error.c)
lines(cv.error.c)
which.min(cv.error.c)+1
#points(which.min(cv.error),cv.error[which.min(cv.error)],col="red")

#####################
#### Part g
#####################
age.grid <- seq(range(age)[1], range(age)[2])
fit.c = glm(wage~cut(age,8),data=Wage)
pred.c=predict(fit.c, newdata=list(age=age.grid), se=T)
plot(age,wage,col="gray") # plot the age/wage data
lines(age.grid,pred.c$fit, lwd=2, col="red")


##############################################
##############################################
################ Section 3 ###################
##############################################
##############################################
# Regression Splines and Smoothing Splines
# use Boston data set 
# fit polinomial degrees from 1-10 and report the associated residual sums of squares for the training error. 
# perform cross-validation to select optimal degree for the polynomial. 
# use bs() function to fit regression spline to predict nox using dis. 
#####################
#### Part a
#####################
Boston
b = data.frame(Boston)
b = b[,c(5,6,7,8)]
disrange <- range( Boston$dis )
dissamples <- seq(from=disrange[1], to=disrange[2], length.out=100)

#####################
#### Part b
#####################
pred = rep(0,10)
RSS = rep(0,10)
for (i in 1:10){
  fit=lm(nox~poly(dis,i),data=b)
  predd=predict(fit,newdata=list(dis=dissamples),se=T)
  pred[i]=list(predd)
  RSS[i]=sum(fit$residuals^2)
}
data.frame(RSS)

plot(x=b$dis, y=b$nox)
c=c("black","red","green","blue","light blue","purple","yellow","grey","black","red")
for (i in 1:10){
  lines(dissamples,pred[[i]]$fit,lwd=0.05,col=c[i])
}
title('Polynomial Fit with Various Degrees')
legend("topright", c("Degree1","Degree2","Degree3","Degree4","Degree5","Degree6","Degree7","Degree8","Degree9","Degree10"),fill=c, horiz=FALSE, cex = 0.5)


#####################
#### Part c
#####################
set.seed(5082)
cv.error=rep(0,10)
for (i in 1:10){
  glm.fit=glm(nox~poly(dis,i), data=b)
  cv.error[i]=cv.glm(b, glm.fit, K=10)$delta[1]
}
cv.error
which.min(cv.error)
fit=lm(nox~poly(dis,3),data=b)
pred=predict(fit,newdata=list(dis=dissamples),se=T)
plot(x=b$dis, y=b$nox)
lines(dissamples,pred$fit,lwd=2,col=c[i])
#####################
#### Part d
#####################
k=attr(bs(b$dis,df=4),"knots")
fit=lm(nox~bs(dis,knots=k),data=b)
summary(fit)
pred=predict(fit,newdata=list(dis=dissamples),se=T) 
plot(x=b$dis, y=b$nox)
lines(dissamples,pred$fit,lwd=2,col="red")
# Comment 1: We chose dof=4. dof=K+d+1. The default poly is degree3. this bs function default at intercept=false. So dof=K+4+1-1 --> K=1. 
# so we have 1 knot. 
# Comment 2: I use attr(bs()) function to get the middle point of the dis, and place the knot in the middle. 
# The knot is placed at 3.20745

#####################
#### Part e 
#####################
pred = rep(0,8)
for (i in 1:8){
  k=attr(bs(b$dis,df=i+2),"knots")
  fit=lm(nox~bs(dis,knots=k),data=b)
  predd1=predict(fit,newdata=list(dis=dissamples),se=T) 
  pred[i]=list(predd1)
}

plot(x=b$dis, y=b$nox)
c=c("green","blue","light blue","purple","yellow","grey","black","red")
for (i in 1:8){
  lines(dissamples,pred[[i]]$fit,lwd=0.05,col=c[i])
}
title('Regression Splines with Various DOF')
legend("topright", c("df3","df4","df5","df6","df7","df8","df9","df10"),fill=c, horiz=FALSE, cex = 0.5)

#RSS for all different dof
k=attr(bs(b$dis,df=4),"knots")
fit=lm(nox~bs(dis,knots=k),data=b)
pred=predict(fit,data=b,se=T) 
RSS = sum((pred$fit-b$nox)^2)

RSS = rep(0,8)
for (i in 1:8){
  k=attr(bs(b$dis,df=i+2),"knots")
  fit=lm(nox~bs(dis,knots=k),data=b)
  pred=predict(fit,data=b,se=T)
  RSS[i]=sum((pred$fit-b$nox)^2)
}
RSS
data.frame(RSS)
dof=c("df3","df4","df5","df6","df7","df8","df9","df10")
RSS.table=cbind(dof,RSS)
#####################
#### Part f ??? x out of boundary knots
#####################
set.seed(5082)
cv.error.c=rep(0,8)
for (i in 1:8){
  #k=attr(bs(b$dis,df=i+2),"knots")
  fit=glm(nox~bs(dis,df=i+2), data=b)
  cv.error.c[i]=cv.glm(b, fit, K=10)$delta[1]
}
cv.error.c
which.min(cv.error.c)+2

fit=glm(nox~bs(dis,df=10),data=b)
pred.1=predict(fit,newdata=list(dis=dissamples),se=T) 
plot(x=b$dis, y=b$nox)
lines(dissamples,pred.1$fit,col="red")
title('Regression splines w/ best d.f. (10) chosen with c.v.')

#####################
#### Part g
#####################
set.seed(5082)
plot(x=b$dis, y=b$nox)
fit.g=smooth.spline(jitter(b$dis),b$nox,cv=TRUE)
fit.g$lambda
pred.g=predict(fit.g,newdata=list(dis=dissamples),se=T)
lines(pred.g$x,pred.g$y,col="red")
title('Smoothing spline w/ best lambda 6.9e-05 chosen w/ c.v.')




