rm(list=ls())
#package installation for FNN
#install.packages('FNN')
library('FNN')
#install.packages('class')
library('class')
#####################
#### PART1       ####
#####################
# Predicting House Prices: The Regression Setting
HomePrices <- read.table("HomePrices.txt", sep="\t", header=T, stringsAsFactors=F)
head(HomePrices) # names already in lowercase
str(HomePrices) 

# MSE
HomePricesMean <- mean(HomePrices$medv)
MSE <- sum((HomePrices$medv - HomePricesMean)^2)/length(HomePrices$medv)
#MSE1 = mean((HomePrices$medv - HomePricesMean)^2)
# print medv variance, see as population not sample
var.medv <- var(HomePrices$medv) * (length(HomePrices$medv) - 1)/length(HomePrices$medv)
# scale the data, with medv preserved 
scaled.HomePrices <- scale(HomePrices[1:12], center=TRUE, scale=TRUE)
new.HomePrices <- cbind(scaled.HomePrices, HomePrices[13])
head(new.HomePrices, 6)

# set random seed to 5072
set.seed(5072)
# create training, validate and test data fromes from teh scaled data frame with 75/15/10 spliting proportion
trainprop <- 0.75
validateprop <- 0.15
n <- nrow(new.HomePrices)
#create a vector of random integers of training size from the vector 1:n
train  <-  sample(n, trainprop * n)
#create a vector of the remaining  integers, then create a vector of random integers
#of validate size by sampling from these
validate  <-  sample(setdiff(1:n, train), validateprop * n) 
#create a vector of the integers not in either training or validate
test <- setdiff(setdiff(1:n, train), validate)
#Create the data frames using the indices created in the three vectors above
trainset <- new.HomePrices[train,]
validateset <- new.HomePrices[validate,]
testset <- new.HomePrices[test,]
#display first row of each dataset
head(trainset,1)
head(validateset,1)
head(testset,1)
#Create the following 6 data frames
train.x <- trainset[-13]
train.y <- trainset[13]
validate.x <- validateset[-13]
validate.y <- validateset[13]
test.x <- testset[-13]
test.y <- testset[13]
#Using the knn.reg() function to predict median home value
kset <- seq(1,19,2)
validate.errors <- rep(0, length(kset))
train.errors <- rep(0, length(kset))
for(i in kset) {
  knn.pred <- knn.reg(train.x, validate.x, train.y, k = i)
  validate.errors[(i+1)/2] <- mean((validate.y - knn.pred$pred)^2) ##this whole half thing is the error rate
  
  knn.pred <- knn.reg(train.x, train.x, train.y, k = i)
  train.errors[(i+1)/2] <- mean((train.y - knn.pred$pred)^2)    
}

#Plot the training and validate MSE's as a function of the k's. 
plot(NULL, NULL, type='n', xlim=c(19, 1), ylim=c(0,max(c(validate.errors, train.errors))), xlab='Increasing Flexibility (Decreasing k)', ylab='Mean Squared Errors', main='MSEs as a Function of Flexibiliy for KNN Regression') ##set up the graph, but now it is empty. 
lines(seq(19, 1, -2), validate.errors[length(validate.errors):1], type='b', col=2, pch=16) ## col=2 means color is red. 
lines(seq(19, 1, -2), train.errors[length(train.errors):1], type='b', col=1, pch=16)
legend("topright", legend = c("Validation MSEs", "Test MSEs"), col=c(2, 1), cex=.5, pch=16) ## cex means magnifier. Now cex=0.75 means make it smaller. 

# Print the k and associated MSE that produced the lowest training MSE. 
print(paste("Minimum training set MSE occurred at k =", kset[which.min(train.errors)]))
print(paste("Minimum training MSE was ", validate.errors[which.min(train.errors)]))
# Print the k and associated MSE that produced the lowest validate MSE. 
print(paste("Minimum validate set MSE occurred at k =", kset[which.min(validate.errors)]))
print(paste("Minimum validate MSE was ", validate.errors[which.min(validate.errors)]))

#predict medv for test set, and compute MSE.
test.medv.pred <- knn.reg(train.x, test.x, train.y, k = 3)
test.MSE <- mean((test.y - test.medv.pred$pred)^2)

#####################
#### Part2       ####
#####################
# Predicting Loan Repayment: The Classification Setting
LoanData <- read.table("LoanData.csv", sep=",", header=T, stringsAsFactors=F)

# print the error rate that would result from always predicting Yes. 
# TER - training error rate 
TER <- mean(LoanData$loan.repaid != 'Yes')

# Scale 
scaled.LoanData <- scale(LoanData[1:7], center=TRUE, scale=TRUE)
new.LoanData <- cbind(scaled.LoanData, LoanData[8])
# display the first 6 rows
head(new.LoanData, 6)

# set random seed to 5072
set.seed(5072)
trainprop <- 0.75
validateprop <- 0.15
m <- nrow(new.LoanData)
train1 <- sample(m, trainprop*m)
validate1 <- sample(setdiff(1:m, train1), validateprop*m)
test1 <- setdiff(setdiff(1:m, train1), validate1)

trainset1 <- new.LoanData[train1,]
validateset1 <- new.LoanData[validate1,]
testset1 <- new.LoanData[test1,]

head(trainset1,1)
head(validateset1,1)
head(testset1,1)

#Create the following 6 data frames
train.x1 <- trainset1[-8]
train.y1 <- trainset1$loan.repaid
validate.x1 <- validateset1[-8]
validate.y1 <- validateset1$loan.repaid
test.x1 <- testset1[-8]
test.y1 <- testset1$loan.repaid

kset1 <- seq(1,19,2)
validate.errors1 <- rep(0, length(kset1))
train.errors1 <- rep(0, length(kset1))
for(i in kset1) {
  knn.pred1 <- knn(train.x1, validate.x1, train.y1, k = i)
  validate.errors1[(i+1)/2] <- mean(knn.pred1 != validate.y1) ##this whole half thing is the error rate
  
  knn.pred1 <- knn(train.x1, train.x1, train.y1, k = i)
  train.errors1[(i+1)/2] <- mean(knn.pred1 != train.y1)    
}

# plot
plot(NULL, NULL, type='n', xlim=c(19, 1), ylim=c(0,max(c(validate.errors1, train.errors1))), xlab='Increasing Flexibility (Decreasing k)', ylab='Error Rates', main='Error Rates as a Function of Flexibiliy for KNN Classification') ##set up the graph, but now it is empty. 
lines(seq(19, 1, -2), validate.errors1[length(validate.errors1):1], type='b', col=2, pch=16) ## col=2 means color is red. 
lines(seq(19, 1, -2), train.errors1[length(train.errors1):1], type='b', col=1, pch=16)
legend("topleft", legend = c("Validation Error Rate", "Test Error Rate"), col=c(2, 1), cex=.5, pch=16) ## cex means magnifier. Now cex=0.75 means make it smaller. 

# print min Error Rate k
print(paste("Minimum training set error rate occurred at k =", kset[which.min(train.errors1)]))
print(paste("Minimum training error rate was ", validate.errors1[which.min(train.errors1)]))
# Print the k and associated MSE that produced the lowest validate MSE. 
print(paste("Minimum validate set error rate occurred at k =", kset[which.min(validate.errors1)]))
print(paste("Minimum validate error rate was ", validate.errors1[which.min(validate.errors1)]))

# predict loan.repaid for the test set using optimal value of k
test.loan.repaid.pred <- knn(train.x1, test.x1, train.y1, k = 11)
test.ER <- mean(test.y1 != test.loan.repaid.pred)

#####################
#### Part3       ####
#####################
# Investigate the Variance of the knn.reg model on the home pricing dataset in Part1

trainprop <- 0.75
validateprop <- 0.15
n <- nrow(new.HomePrices)

set.seed(5072)
newset <- 1:50
validate.final <- rep(0,50)
test.final <- rep(0,50)
for(m in newset) {
  train  <-  sample(n, trainprop * n)
  validate  <-  sample(setdiff(1:n, train), validateprop * n) 
  test <- setdiff(setdiff(1:n, train), validate)
  trainset <- new.HomePrices[train,]
  validateset <- new.HomePrices[validate,]
  testset <- new.HomePrices[test,]
  train.x <- trainset[-13]
  train.y <- trainset[13]
  validate.x <- validateset[-13]
  validate.y <- validateset[13]
  test.x <- testset[-13]
  test.y <- testset[13]
  
  kset <- seq(1,19,2)
  validate.errors2 <- rep(0, length(kset))
  for(i in kset){
    knn.pred2 <- knn.reg(train.x, validate.x, train.y, k = i)
    validate.errors2[(i+1)/2] <- mean((validate.y - knn.pred2$pred)^2)
    
    #knn.pred2 <- knn.reg(train.x, train.x, train.y, k = i)
    #train.errors2[(i+1)/2] <- mean((train.y - knn.pred2$pred)^2)
    mink <- kset[which.min(validate.errors2)]
  }
  validate.final[m] <- validate.errors2[which.min(validate.errors2)]
  test.medv.pred.final <- knn.reg(train.x, test.x, train.y, k = mink)
  test.final[m] <- mean((test.y - test.medv.pred.final$pred)^2)
}

mean(validate.final)
sd(validate.final)
mean(test.final)
sd(test.final)

#plot
plot(NULL, NULL, type='n', xlim=c(0,50), ylim=c(0,max(c(validate.final, test.final))), xlab='Replication', ylab='MSEs', main='Test and Best Validation MSEs for Many Partionings of the Data')  
lines(validate.final, type='b', col=2, pch=16) ## col=2 means color is red. 
lines(test.final, type='b', col=1, pch=16)
abline(h=mean(validate.final), lty=2, col=2)
abline(h=mean(test.final), lty=2, col=1)
legend("topright", legend = c("Validation MSEs", "Validation MSE mean", "Test MSEs", "Test MSE mean"), col=c(2, 2, 1, 1), lty=c(1, 2, 1, 2), cex=.5, pch=16) ## cex means magnifier. Now cex=0.75 means make it smaller. 


