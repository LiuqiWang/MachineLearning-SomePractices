# Tree-based Methods
# develop Customer Churn Model for telecommunications company facing intense competition from industry rivals due to number of structural factors in the industry.
# the company designed a retention offer for at-risk customers. 
# The percentage of at-risk customers in the current customer base is 26.448%.
# The retention offer will cost the company $16000 per customer, one time.
# A lost customer costs the company $11,500, one time.
# The project has concluded through detailed customer survays and historical data analysis that the retention offer will be 45% effective. The remaining 55% will leave anyway. 

rm(list=ls())
library(tree)
library(ISLR)
library(randomForest)
library(gbm)
#install.packages("pROC")
library(pROC)
library(xgboost)
require(xgboost)
c=read.csv(file="A2TrainingData1.csv",header=TRUE,sep=",") # I directly deleted the null value in dataset
c=c[,-1]
train=sample(1:nrow(c),nrow(c)/2)
test.c=c[-train,]
c1=read.csv(file="A2TrainingData2.csv",header=TRUE,sep=",") # Changed Churn data into 1 and 0
c1=c1[,-1]
train1=sample(1:nrow(c1),nrow(c1)/2)
test.c1=c1[-train,]

##########################
### Classification Tree
##########################
set.seed(5072)
tree.c=tree(Churn~.,c,subset=train)
tree.pred=predict(tree.c,test.c,type="class")
table = table(tree.pred,test.c$Churn)
# calculate costs
at_risk<-mean(c1$Churn == 1)
c_do_nothing = at_risk * 11500
c_TP = 0.45*1600+0.55*13100
c_TN = 11500
c_FP = 1600
c_FN = 0

TP=table[2,2]/sum(table)
TN=table[1,2]/sum(table)
FP=table[2,1]/sum(table)
FN=table[1,1]/sum(table)
ExpCost=TP*c_TP + TN*c_TN + FP*c_FP + FN*c_FN


##########################
### Regression Tree 
##########################
set.seed(5072)
tree.c1=tree(Churn~.,c1,subset=train1)
tree.pred1 = predict(tree.c1,newdata=test.c1)
at_risk<-mean(c1$Churn == 1)
c_do_nothing = at_risk * 11500
c_TP = 0.45*1600+0.55*13100
c_TN = 11500
c_FP = 1600
c_FN = 0

# try different cutoff points (WITHOUT PRUNING THE TREE)
tree.rate <- data.frame()
for(i in seq(0, 1, .01)){
  tree.pred.assign <- ifelse(tree.pred > i, 1, 0)
  #table = table(tree.pred.assign,test.c1$Churn)
  TP <- sum(test.c1$Churn == tree.pred.assign & test.c1$Churn == 1)/length(tree.pred.assign)
  TN <- sum(test.c1$Churn != tree.pred.assign & test.c1$Churn == 1)/length(tree.pred.assign)
  FN <- sum(test.c1$Churn == tree.pred.assign & test.c1$Churn == 0)/length(tree.pred.assign)
  FP <- sum(test.c1$Churn != tree.pred.assign & test.c1$Churn == 0)/length(tree.pred.assign)
  Error <- mean(tree.pred.assign != test.c1$Churn)
  Exp <- TP*c_TP + TN*c_TN + FP*c_FP + FN*c_FN
  tree.rate <- rbind(tree.rate, c(i, FP, FN, TP, TN, Error, Exp))
}
colnames(tree.rate) <- c("Cutoff", "FP", "FN", "TP", "TN", "Error","ExpectedCost")
min(tree.rate$ExpectedCost)
tree.rate[which.min(tree.rate$ExpectedCost),]
#write.csv(tree.rate, file = "TreeCost.csv")

# try pruning the tree, with different cutoffs. (The performance is worse than unpruned trees)
tree.prune.rate <- data.frame()
for (i in 2:5){
  prune.c1=prune.tree(tree.c1,best=2)
  tree.prune.pred=predict(prune.c1,test.c1)
  for(j in seq(0, 1, .01)){
    tree.pred.assign <- ifelse(tree.prune.pred > j, 1, 0)
    table = table(tree.pred.assign,test.c1$Churn)
    TP <- sum(test.c1$Churn == tree.pred.assign & test.c1$Churn == 1)/length(tree.pred.assign)
    TN <- sum(test.c1$Churn != tree.pred.assign & test.c1$Churn == 1)/length(tree.pred.assign)
    FN <- sum(test.c1$Churn == tree.pred.assign & test.c1$Churn == 0)/length(tree.pred.assign)
    FP <- sum(test.c1$Churn != tree.pred.assign & test.c1$Churn == 0)/length(tree.pred.assign)
    Error <- mean(tree.pred.assign != test.c1$Churn)
    Exp <- TP*c_TP + TN*c_TN + FP*c_FP + FN*c_FN
    tree.prune.rate <- rbind(tree.prune.rate, c(i, j, FP, FN, TP, TN, Error, Exp))
  }
}
colnames(tree.prune.rate) <- c("best", "Cutoff", "FP", "FN", "TP", "TN", "Error","ExpectedCost")
min(tree.prune.rate$ExpectedCost)
tree.prune.rate[which.min(tree.prune.rate$ExpectedCost),]
#write.csv(tree.prune.rate, file = "TreeCost1.csv")

# Compare Classification Tree, Regression Tree, Regression Pruned Tree:
(ExpCost=TP*c_TP + TN*c_TN + FP*c_FP + FN*c_FN) #Classification Tree
tree.rate[which.min(tree.rate$ExpectedCost),] #Regression Tree
tree.prune.rate[which.min(tree.prune.rate$ExpectedCost),] #Regression Pruned Tree
# It is shown that Regression Unpruned Rree gives the min.cost , $2646.8


##########################
### Random Forest
##########################
set.seed(5072)
CRF_cost=rep(0,19)
for(i in 1:19){
  bag.c=randomForest(Churn~.,data=c,subset=train,mtry=i,importance=TRUE)
  bag.pred=predict(bag.c,test.c,type="class")
  table = table(bag.pred,test.c$Churn)
  TP=table[2,2]/sum(table)
  TN=table[1,2]/sum(table)
  FP=table[2,1]/sum(table)
  FN=table[1,1]/sum(table)
  CRF_cost=TP*c_TP + TN*c_TN + FP*c_FP + FN*c_FN
}
min(CRF_cost)
which.min(CRF_cost)

##########################
### Random Forest w/ cutoff w/test&training set
##########################
rf.rate=data.frame()
set.seed(5072)
#Exp=rep(0,19)
for(i in 1:19){
  rf.c=randomForest(Churn~.,data=c,subset=train,mtry=i,importance=TRUE)
  rf.pred=predict(rf.c,test.c,type="prob")[,2]
  for(j in seq(0, 1, .01)){
    rf.pred.assign <- ifelse(rf.pred > j, 1, 0)
    TP <- sum(test.c1$Churn == rf.pred.assign & test.c1$Churn == 1)/length(rf.pred.assign)
    TN <- sum(test.c1$Churn != rf.pred.assign & test.c1$Churn == 1)/length(rf.pred.assign)
    FN <- sum(test.c1$Churn == rf.pred.assign & test.c1$Churn == 0)/length(rf.pred.assign)
    FP <- sum(test.c1$Churn != rf.pred.assign & test.c1$Churn == 0)/length(rf.pred.assign)
    Error <- mean(rf.pred.assign != test.c1$Churn)
    Exp <- TP*c_TP + TN*c_TN + FP*c_FP + FN*c_FN
    rf.rate <- rbind(rf.rate, c(i, j, FP, FN, TP, TN, Error, Exp))
  }
}
colnames(rf.rate) <- c("mtry", "Cutoff", "FP", "FN", "TP", "TN", "Error","ExpectedCost")
min(rf.rate$ExpectedCost)
rf.rate[which.min(rf.rate$ExpectedCost),]

##########################
### Random Forest w/ cutoff w/o test&training set
##########################
rf1.rate=data.frame()
set.seed(5072)
#Exp1=rep(0,19)
for(i in 1:19){
  rf1.c=randomForest(Churn~.,data=c,mtry=i,importance=TRUE)
  rf1.pred=predict(rf.c,c,type="prob")[,2]
  for(j in seq(0, 1, .01)){
    rf1.pred.assign <- ifelse(rf1.pred > j, "Yes", "No")
    TP <- sum(c$Churn == rf1.pred.assign & c$Churn == "Yes")/length(rf1.pred.assign)
    TN <- sum(c$Churn != rf1.pred.assign & c$Churn == "Yes")/length(rf1.pred.assign)
    FN <- sum(c$Churn == rf1.pred.assign & c$Churn == "No")/length(rf1.pred.assign)
    FP <- sum(c$Churn != rf1.pred.assign & c$Churn == "No")/length(rf1.pred.assign)
    Error <- mean(rf1.pred.assign != c$Churn)
    Exp1 <- TP*c_TP + TN*c_TN + FP*c_FP + FN*c_FN
    rf1.rate <- rbind(rf1.rate, c(i, j, FP, FN, TP, TN, Error, Exp1))
  }
}
colnames(rf1.rate) <- c("mtry", "Cutoff", "FP", "FN", "TP", "TN", "Error","ExpectedCost")
min(rf1.rate$ExpectedCost)
rf1.rate[which.min(rf1.rate$ExpectedCost),]

# Compare different RF
rf.rate[which.min(rf.rate$ExpectedCost),]
rf1.rate[which.min(rf1.rate$ExpectedCost),]
# Comment: best is to use whole data to train rf. $2370.8

##########################
### Boosting
##########################
# gbm boosting
boost.rate=data.frame()
set.seed(5072)
#boost_cost=rep(0,19)
for(i in 1:5){
  for (k in seq(0,1,.01)){
    b.c1=gbm(Churn~.,data=c1[train1,],distribution="bernoulli",n.trees=5000,interaction.depth=i,shrinkage=k)
    b.pred=predict(b.c1,newdata=test.c1,n.trees=5000)
    for(j in seq(0, 1, .01)){
      b.pred.assign <- ifelse(b.pred > j, 1, 0)
      TP <- sum(test.c1$Churn == b.pred.assign & test.c1$Churn == 1)/length(b.pred.assign)
      TN <- sum(test.c1$Churn != b.pred.assign & test.c1$Churn == 1)/length(b.pred.assign)
      FN <- sum(test.c1$Churn == b.pred.assign & test.c1$Churn == 0)/length(b.pred.assign)
      FP <- sum(test.c1$Churn != b.pred.assign & test.c1$Churn == 0)/length(b.pred.assign)
      Error <- mean(b.pred.assign != test.c1$Churn)
      Exp <- TP*c_TP + TN*c_TN + FP*c_FP + FN*c_FN
      boost.rate <- rbind(boost.rate, c(i, k, j, FP, FN, TP, TN, Error, Exp))
    }
  }
}
colnames(boost.rate) <- c("depth", "shrinkage", "Cutoff", "FP", "FN", "TP", "TN", "Error","ExpectedCost")
min(boost.rate$ExpectedCost)
boost.rate[which.min(boost.rate$ExpectedCost),]
write.csv(boost.rate, file = "boostCost.csv")
# Comment: gbm boosting gives a pretty good $2480

### Overall: if use the whole dataset to train Random Forest model, it will give $2370, which is the min. cost. 
### if spliting to test set and training set, gbm boosting gives a good result of $2480.





