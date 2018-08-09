# Classification and Resampling
rm(list=ls())
library(class)
library(ISLR)
library(MASS)
library(boot)
#####################
#### PART 01     ####
#####################
# Use Weekly dataset from ISLR package. 
# a) Set Seed
set.seed(5072)

# b) Perform Logistic Regression on Weekly
w <- Weekly
w1 <- w[,-c(1,8)]
# Logistic Regression: fit a logistic regression model in order to predict Direction using Lag1 through Lag5 and Volume
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=w1, family=binomial)
# All predictors are negatively associated to response, except Lag2. Only Lag2 has a small p-value which indicate a clear association btw Lag2 and Direction. 
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef[, 4]
#type="response" outputs probabilities P(Y = 1|X) instead of, for example, the logit.
glm.probs <- predict(glm.fit, type="response")   
glm.probs[1:10]
contrasts(Weekly$Direction)

# c) Confusion Matrix
# Convert the predicted probabilities into class labels Up or Down
glm.pred <- rep("Down", nrow(Weekly))
glm.pred[glm.probs>.5] <- "Up"
t <- table(Weekly$Direction,glm.pred)
t 

# d) Performance Statistics
## Overall Fraction of Correct Predictions.
mean(glm.pred==Weekly$Direction)
## Overall Error Rate
mean(glm.pred!=Weekly$Direction)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
430/(54+430)
t[1,2]/(t[1,1]+t[1,2])
### Type II Error: Accept Null when Null's False. 
48/(48+557)
t[2,1]/(t[2,1]+t[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
557/(48+557)
t[2,2]/(t[2,1]+t[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
557/(430+557)
t[2,2]/(t[1,2]+t[2,2])

# e) Train Logistic Regression on data before 2009. Lag2 as the only predictor.
train <- Weekly[Weekly$Year<=2008,]
glm.fit <- glm(Direction~Lag2, data=train, family=binomial)

# f) Test Logistic Regression trained in e) on data after 2009.
test <- Weekly[Weekly$Year>2008,]
glm.probs <- predict(glm.fit, test, type="response")
# Construct test confusion matrix
glm.pred <- rep("Down", nrow(test))
glm.pred[glm.probs>.5] <- "Up"
tt <- table(test$Direction,glm.pred)
tt 
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(glm.pred==test$Direction)
## Overall Error Rate
mean(glm.pred!=test$Direction)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
tt[1,2]/(tt[1,1]+tt[1,2])
### Type II Error: Accept Null when Null's False. 
tt[2,1]/(tt[2,1]+tt[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
tt[2,2]/(tt[2,1]+tt[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
tt[2,2]/(tt[1,2]+tt[2,2])

# g)
# Train LDA on training set
#################why not the first one?
#lda.fit = lda(Direction~Lag2, data=Weekly, subset=train)
lda.fit = lda(Direction~Lag2, data=train)
# Test LDA on testing set
lda.pred = predict(lda.fit, test)
names(lda.pred)
# Confusiom Matrix
lda.t <- table(test$Direction, lda.pred$class)
lda.t
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(glm.pred==test$Direction)
## Overall Error Rate
mean(glm.pred!=test$Direction)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
lda.t[1,2]/(lda.t[1,1]+lda.t[1,2])
### Type II Error: Accept Null when Null's False. 
lda.t[2,1]/(lda.t[2,1]+lda.t[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
lda.t[2,2]/(lda.t[2,1]+lda.t[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
lda.t[2,2]/(lda.t[1,2]+lda.t[2,2])

# h) #################3why not the first one?
# Train QDA on training set
#qda.fit = qda(Direction~Lag2, data=Weekly, subset=train)
qda.fit = qda(Direction~Lag2, data=train)
# Test QDA on testing set
qda.pred = predict(qda.fit, test)
# Confusiom Matrix
qda.t <- table(test$Direction, qda.pred$class)
qda.t
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(qda.pred$class==test$Direction)
## Overall Error Rate
mean(qda.pred$class!=test$Direction)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
qda.t[1,2]/(qda.t[1,1]+qda.t[1,2])
### Type II Error: Accept Null when Null's False. 
qda.t[2,1]/(qda.t[2,1]+qda.t[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
qda.t[2,2]/(qda.t[2,1]+qda.t[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
qda.t[2,2]/(qda.t[1,2]+qda.t[2,2])

# i) KNN, with k=1
knn.pred = knn(data.frame(train$Lag2), data.frame(test$Lag2), train$Direction, k=1)
knn.t <- table(test$Direction, knn.pred)
knn.t
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(knn.pred==test$Direction)
## Overall Error Rate
mean(knn.pred!=test$Direction)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
knn.t[1,2]/(knn.t[1,1]+knn.t[1,2])
### Type II Error: Accept Null when Null's False. 
knn.t[2,1]/(knn.t[2,1]+knn.t[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
knn.t[2,2]/(knn.t[2,1]+knn.t[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
knn.t[2,2]/(knn.t[1,2]+knn.t[2,2])

# j) Knn, with k=5
knn.pred5 = knn(data.frame(train$Lag2), data.frame(test$Lag2), train$Direction, k=5)
knn.t <- table(test$Direction, knn.pred5)
knn.t
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(knn.pred5==test$Direction)
## Overall Error Rate
mean(knn.pred5!=test$Direction)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
knn.t[1,2]/(knn.t[1,1]+knn.t[1,2])
### Type II Error: Accept Null when Null's False. 
knn.t[2,1]/(knn.t[2,1]+knn.t[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
knn.t[2,2]/(knn.t[2,1]+knn.t[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
knn.t[2,2]/(knn.t[1,2]+knn.t[2,2])

# k) Logistic & LDA better, with prediction correctness of 0.62

#####################
#### PART 02     ####
#####################
# develop a model to predict whether a given car gets high or low gas mileage based on the Auto data set. 
# try using LDA, QDA, and KNN
# a) Set seed
set.seed(5072)

#b) create binary variable mpg01
a <- Auto
mpg01 <- as.numeric(a$mpg>median(a$mpg))
a <- cbind(a,mpg01)
# c) split train and test sets
train2  <-  sample(nrow(a), 0.8 * nrow(a))
test2 <- setdiff(1:nrow(a), train2)
trainset <- a[train2,]
testset <- a[test2,]

# d) Perform Logistic Regression on Trainset, predict mpg01 with cylinders, displacement and weight.
glm.fit <- glm(mpg01~cylinders+displacement+weight, data=trainset, family=binomial)

# e) Test Logistic Regression model, build confusion matrix
# Test Logistic Regression model on Testset
glm.probs <- predict(glm.fit, testset, type="response")
# Construct test confusion matrix
glm.pred <- rep("0", nrow(testset))
glm.pred[glm.probs>.5] <- "1"
tt <- table(testset$mpg01,glm.pred)
tt 
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(glm.pred==testset$mpg01)
## Overall Error Rate
mean(glm.pred!=testset$mpg01)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
tt[1,2]/(tt[1,1]+tt[1,2])
### Type II Error: Accept Null when Null's False. 
tt[2,1]/(tt[2,1]+tt[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
tt[2,2]/(tt[2,1]+tt[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
tt[2,2]/(tt[1,2]+tt[2,2])

# f) LDA
lda.fit = lda(mpg01~cylinders+displacement+weight, data=trainset)
# Test LDA on testing set
lda.pred = predict(lda.fit, testset)
names(lda.pred)
# Confusiom Matrix
lda.t <- table(testset$mpg01, lda.pred$class)
lda.t
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(glm.pred==testset$mpg01)
## Overall Error Rate
mean(glm.pred!=testset$mpg01)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
lda.t[1,2]/(lda.t[1,1]+lda.t[1,2])
### Type II Error: Accept Null when Null's False. 
lda.t[2,1]/(lda.t[2,1]+lda.t[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
lda.t[2,2]/(lda.t[2,1]+lda.t[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
lda.t[2,2]/(lda.t[1,2]+lda.t[2,2])

# g) QDA
qda.fit = qda(mpg01~cylinders+displacement+weight, data=trainset)
# Test QDA on testing set
qda.pred = predict(qda.fit, testset)
# Confusiom Matrix
qda.t <- table(testset$mpg01, qda.pred$class)
qda.t
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(qda.pred$class==testset$mpg01)
## Overall Error Rate
mean(qda.pred$class!=testset$mpg01)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
qda.t[1,2]/(qda.t[1,1]+qda.t[1,2])
### Type II Error: Accept Null when Null's False. 
qda.t[2,1]/(qda.t[2,1]+qda.t[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
qda.t[2,2]/(qda.t[2,1]+qda.t[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
qda.t[2,2]/(qda.t[1,2]+qda.t[2,2])

# h) KNN with k=1
train.x = trainset[,c(2,3,5)]
test.x = testset[,c(2,3,5)]
train.y = trainset[,10]
test.y = testset[,10]
knn.pred = knn(train.x, test.x, train.y, k=1)
knn.t <- table(test.y, knn.pred)
knn.t
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(knn.pred==test.y)
## Overall Error Rate
mean(knn.pred!=test.y)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
knn.t[1,2]/(knn.t[1,1]+knn.t[1,2])
### Type II Error: Accept Null when Null's False. 
knn.t[2,1]/(knn.t[2,1]+knn.t[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
knn.t[2,2]/(knn.t[2,1]+knn.t[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
knn.t[2,2]/(knn.t[1,2]+knn.t[2,2])

# i) try knn with different k value
kset <- c(1:20)
test.pred <- rep(0, length(kset))
for(i in kset) {
  knn.pred = knn(train.x, test.x, train.y, k=i)
  test.pred[i] <- mean(knn.pred==test.y) 
}
print(paste("The best k is ", kset[which.max(test.pred)]))
print(paste("Overall Fraction of Correct Predictions is ", test.pred[which.max(test.pred)]))

# j) KNN when k=3 & QDA best, with Fraction of Correct Prediction of 0.9113924

#####################
#### PART 03     ####
#####################
# use Boston dataset in MASS package. creat training & test sets
# fit classification models to predict whether a given suburb has a crime rate above or below the median.
# explore logistic regression, LDA, KNN models using nox, rad and dis as predictors. 
# evaluate the models using confusion matrix. 
# a) 
# Set seed
set.seed(5072)
b <- Boston
# create binary variable crime01
crime01 <- as.numeric(b$crim>median(b$crim))
b <- cbind(b,crime01)
# split train and test sets
train2  <-  sample(nrow(b), 0.8 * nrow(b))
test2 <- setdiff(1:nrow(b), train2)
trainset <- b[train2,]
testset <- b[test2,]

# Logistic Regression
glm.fit <- glm(crime01~nox+rad+dis, data=trainset, family=binomial)
# Test Logistic Regression model on Testset
glm.probs <- predict(glm.fit, testset, type="response")
# Construct test confusion matrix
glm.pred <- rep("0", nrow(testset))
glm.pred[glm.probs>.5] <- "1"
tt <- table(testset$crime01,glm.pred)
tt 
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(glm.pred==testset$crime01)
## Overall Error Rate
mean(glm.pred!=testset$crime01)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
tt[1,2]/(tt[1,1]+tt[1,2])
### Type II Error: Accept Null when Null's False. 
tt[2,1]/(tt[2,1]+tt[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
tt[2,2]/(tt[2,1]+tt[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
tt[2,2]/(tt[1,2]+tt[2,2])

# LDA
lda.fit = lda(crime01~nox+rad+dis, data=trainset)
# Test LDA on testing set
lda.pred = predict(lda.fit, testset)
names(lda.pred)
# Confusiom Matrix
lda.t <- table(testset$crime01, lda.pred$class)
lda.t
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(glm.pred==testset$crime01)
## Overall Error Rate
mean(glm.pred!=testset$crime01)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
lda.t[1,2]/(lda.t[1,1]+lda.t[1,2])
### Type II Error: Accept Null when Null's False. 
lda.t[2,1]/(lda.t[2,1]+lda.t[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
lda.t[2,2]/(lda.t[2,1]+lda.t[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
lda.t[2,2]/(lda.t[1,2]+lda.t[2,2])

# QDA
qda.fit = qda(crime01~nox+rad+dis, data=trainset)
# Test QDA on testing set
qda.pred = predict(qda.fit, testset)
# Confusiom Matrix
qda.t <- table(testset$crime01, qda.pred$class)
qda.t
# Performance Statistics
## Overall Fraction of Correct Predictions.
mean(qda.pred$class==testset$crime01)
## Overall Error Rate
mean(qda.pred$class!=testset$crime01)
## Type I Type II Error Rate
### Type I Error: Reject Null when Null's True.
qda.t[1,2]/(qda.t[1,1]+qda.t[1,2])
### Type II Error: Accept Null when Null's False. 
qda.t[2,1]/(qda.t[2,1]+qda.t[2,2])
## The Power/Sensitivity of the Model: true defaulters correctly identified. 
qda.t[2,2]/(qda.t[2,1]+qda.t[2,2])
## The Precision of the Model: predicted defaulters correctly identified.
qda.t[2,2]/(qda.t[1,2]+qda.t[2,2])

# KNN
train.x = trainset[,c(5,9,8)]
test.x = testset[,c(5,9,8)]
train.y = trainset[,15]
test.y = testset[,15]
kset <- c(1:20)
test.pred <- rep(0, length(kset))
for(i in kset) {
  knn.pred = knn(train.x, test.x, train.y, k=i)
  test.pred[i] <- mean(knn.pred==test.y) 
}
print(paste("The best k is ", kset[which.max(test.pred)]))
print(paste("Overall Fraction of Correct Predictions is ", test.pred[which.max(test.pred)]))

# Best with KNN, k=1. Overall Fraction of Correct Prediction is 0.95098039

#####################
#### PART 04     ####
#####################
# perform cross-validation on a simulated data set. 
# a) 
set.seed(5072)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

# b) 
z <- data.frame(x,y)
colnames(z) <- c("X","Y")

# c) 
plot(x,y, main="Scatterplot", xlab="X", ylab="Y", pch=19)

# d) 
set.seed(123)
cv.error <- rep(0,4)
# loop through the process 4 times 
for (i in 1:4){
  glm.fit <- glm(Y~poly(X,i),data=z)
  cv.error[i] <- cv.glm(z,glm.fit)$delta[1]
}
cv.error

# e) 
set.seed(456)
cv.error1 <- rep(0,4)
# loop through the process 4 times 
for (i in 1:4){
  glm.fit1 <- glm(Y~poly(X,i),data=z)
  cv.error1[i] <- cv.glm(z,glm.fit1)$delta[1]
}
cv.error1
# result is the same as in d).
# Because the fitting cv.glm function, the model, does not involve random selection. 

# f) 
which.min(cv.error)
min(cv.error)
# It is what I expected because the original function has only X and X^2, 
#and model ii contains only X and X^2, which correctly simulate the orginal function. 

# g)
summary(glm(Y~poly(X,1),data=z))
#poly(x,1) *
summary(glm(Y~poly(X,2),data=z))
#ploy(x,2)1 ***
#ploy(x,2)2 ***
summary(glm(Y~poly(X,3),data=z))
#ploy(x,3)1 ***
#ploy(x,3)2 ***
summary(glm(Y~poly(X,4),data=z))
#ploy(x,4)1 ***
#ploy(x,4)2 ***
# the p-values result from fitting different models agree with the cross-validation result
# that X and X^2 are highly related to Y. 

