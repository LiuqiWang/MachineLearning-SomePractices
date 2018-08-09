# Linear Regression
rm(list=ls())

#####################
#### Part 01     ####
#####################
# Simple Linear Regression
# create simulated data sets and fit simple linear regression models to them.
set.seed(5072)
x <- rnorm(100,mean=0,sd=1)
ep <- rnorm(100,mean=0,sd=0.5)
y = -1 + 0.5*x + ep
length(y)
# e)Comment: in this model, beta0=-1, beta1=0.05
plot(x,y, main="Moderate Error in the Population", xlab="x", ylab="y")
# g)Comment: the relationship between x and y is positive, moderate linear, the amount of variability observed is around 35%. 
mod <- lm(y~x)
coef(mod)
# i)a)Comment: beta0hat=-1, beta1hat=0.435
# i)b)Comment: beta0hat is almost the same as beta0, whereas beta1hat is very close but not the same as beta1.
abline(coef(mod), lwd=3, col="black")
y1 = -1 + 0.5*x
c <- lm(y1~x)
abline(coef(c), lwd=3, col="red")
legend("bottomright", legend = c("Leaset Squares", "Population"), col=c(1, 2), cex=.5, lty=c(1,1))
mode <- lm(formula=y ~ poly(x,2))
coef(mode)
anova(mod,mode)
R2<-summary(mod)$r.squared
# n)Comment: Since the F-statistic < 1, there is no relationship between the response and predictor x^2. p-value=0.5683, which is greater than the biggest alpha 0.1, accept the null hypothesis that no significant difference between model1 and model2. 
##############################################################################################
# Run b-m again with ep_var=0.1
#set.seed(5072)
#x <- rnorm(100,mean=0,sd=1)
ep <- rnorm(100,mean=0,sd=sqrt(0.1))
y = -1 + 0.5*x + ep
length(y)
# e)Comment: in this model, beta0=-1, beta1=0.05
plot(x,y, main="Less Error in the Population", xlab="x", ylab="y")
# g)Comment: the relationship between x and y is positive, strongly linear, the amount of variability observed is around 90%. 
mod2 <- lm(y~x)
coef(mod2)
# i)a)Comment: beta0hat=-0.999, beta1hat=0.521995
# i)b)Comment: beta0hat is almost the same as beta0, whereas beta1hat is very close but not the same as beta1.
abline(coef(mod2), lwd=3, col="black")
y1 = -1 + 0.5*x
c <- lm(y1~x)
abline(coef(c), lwd=3, col="red")
legend("bottomright", legend = c("Leaset Squares", "Population"), col=c(1, 2), cex=.5, lty=c(1,1))
mode2 <- lm(formula=y ~ poly(x,2))
coef(mode2)
anova(mod2,mode2)
R2<-summary(mod2)$r.squared
# n)Comment: Since the F-statistic < 1, there is no relationship between the response and predictor x^2. p-value=0.5683, which is greater than the biggest alpha 0.1, accept the null hypothesis that no significant difference between model1 and model2. 
##################################################################################################3
# Run b-m again with ep_var=0.5
#set.seed(5072)
#x <- rnorm(100,mean=0,sd=1)
ep <- rnorm(100,mean=0,sd=sqrt(0.5))
y = -1 + 0.5*x + ep
length(y)
# e)Comment: in this model, beta0=-1.074, beta1=0.5832
plot(x,y, main="Higher Error in the Population", xlab="x", ylab="y")
# g)Comment: the relationship between x and y is positive, strongly linear, the amount of variability observed is around 90%. 
mod3 <- lm(y~x)
coef(mod3)
# i)a)Comment: beta0hat=-1, beta1hat=0.408
# i)b)Comment: beta0hat is almost the same as beta0, whereas beta1hat is very close but not the same as beta1.
abline(coef(mod3), lwd=3, col="black")
y1 = -1 + 0.5*x
c <- lm(y1~x)
abline(coef(c), lwd=3, col="red")
legend("bottomright", legend = c("Leaset Squares", "Population"), col=c(1, 2), cex=.5, lty=c(1,1))
mode3 <- lm(formula=y ~ poly(x,2))
coef(mode3)
anova(mod3,mode3)
R2<-summary(mod3)$r.squared
# n)Comment: Since the F-statistic < 1, there is no relationship between the response and predictor x^2. p-value=0.5683, which is greater than the biggest alpha 0.1, accept the null hypothesis that no significant difference between model1 and model2. 
#################################################################################################3
# q)Comment: in the previous three plot, I fitted the regression lines for y with ep_variance=0.25, 0.1, and 0.5. The linear regression line fit better when ep_variance is smaller.
confint(mod) #original dataset
confint(mod2) #less noisy dataset
confint(mod3) #nosier dataset
# s)Comment: Larger variance results in larger widths. 

#####################
#### QUESTION 02 ####
#####################
# Collinearity practice
set.seed(5072)
x1 <- runif(100)
x2 <- 0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
# b)Comment: beta0=2, beta1=2, beta2=0.3
cor(y,x1)
cor(y,x2)
cor(x1,x2)
pairs(data.frame(y,x1,x2))
# e)Comment: correlation between y,x1=0.51, which is moderate uphill (positive) relationship; between y,x2=0.409, which is moderate uphill linear relationship, between x1,x2=0.844, which is a strong uphill linear relationship. 
lm.fit.both <- lm(y~x1+x2)
lm.fit.both
summary(lm.fit.both)
# h)Comment: look at the Pr(>|t|), beta0hat and beta1hat are indicated highly significant, reject null hypothesis of not related, that beta0hat and beta1hat have relationship with y, whereas beta2hat is not significant and thus has no relationship with y. 
# i)Comment: Yes, can reject the null hypothesis H0:beta1=0. Accept the H0: beta2=0. I got this conclusion by observing the Pr(>|t|) of those two betahats. When P-value < alpha, reject null hypothesis.
lm.fit.justx1 <- lm(y~x1)
summary(lm.fit.justx1)
# k)Comment: Can reject the null hypothesis H0:beta1=0 because the summamry shows that Pr(>|t|) is *** which means highly significant. 
lm.fit.justx2 <- lm(y~x2)
summary(lm.fit.justx2)
# m)Comment: Can reject the null hypothesis H0:beta2=0 because the summary shows its highly significant. 
# n)Comment: The results contradict. Both x1 and x2 have some relationship with y if use x1 and x2 seperately to influence y. x1 has higher influence on y than x2, so when relating x1 and x2 together to indicate y, beta1hat shows more significance than beta2hat. X2 is affected by x1, means they have colinearity, in ml.fit.both, since we already have x1, then x2 is not needed in the model because it is has linear correlation with x1. 
##############################################################################
x1=c(x1,0.1)
x2=c(x2,0.8)
y=c(y,6)
lm.fit.both1 <- lm(y~x1+x2)
lm.fit.both1
summary(lm.fit.both1)
lm.fit.justx11 <- lm(y~x1)
summary(lm.fit.justx11)
lm.fit.justx21 <- lm(y~x2)
summary(lm.fit.justx21)
# q)Comment: In lm.fit.both: beta0hat is highly significant, beta1hat's significance dropped, beta2hat's significance raised. 
# q)Comment: In lm.fit.justx1 and lm.fit.justx2, x1 and x2 are still seperately highly significant.
par(mfrow=c(2,2))
plot(lm.fit.both1)
plot(lm.fit.justx11)
plot(lm.fit.justx21)
# r)Comment: point 101 is an outlier and high-leverage point. 

#####################
#### QUESTION 03 ####
#####################
# Simple vs. Multiple Regression, practice with Extraction Functions
# use Boston dataset in MASS package. 
# try to predict per capita crime rate using the other variables in this data set.
library(MASS)
set.seed(5072)
B <- Boston
#B <- data.frame(Boston)
Predictor <- names(Boston)
B1 <- B[-1] # Keep all the predictors in the Boston data

# This step filled the "Predictors" column of table PT.
n <- ncol(B1)
PT <- matrix(rep(0),nrow=n, ncol=5) # PT = PredictTable
colnames(PT) <- c("Predictor", "F-Statistic", "p-Value", "Beta0_hat", "Beta1_hat")
PT[,1] <- names(B1) # fill Predictors
for (i in 1:n){
  PT[i,2] <- summary(lm(B[,1]~B1[,i]))$fstatistic[1] #fill the "F-Statistic" column of table PT.
  PT[i,3] <- anova(lm(B[,1]~B1[,i]))$'Pr(>F)'[1] #fill the "p-Value" column of the table PT.
  PT[i,4] <- coef(lm(B[,1]~B1[,i]))[1] #fill the "beta_hat" columns of the table PT.
  PT[i,5] <- coef(lm(B[,1]~B1[,i]))[2]
}

PT <- data.frame(PT, stringsAsFactors = FALSE)
PT[,2] <- as.numeric(PT[,2])
PT[,3] <- as.numeric(PT[,3])
PT[,4] <- as.numeric(PT[,4])
PT[,5] <- as.numeric(PT[,5])

#delete p>0.05, keep p<0.05 to reject null hypothesis
alp5 <- PT[(PT[,3]<0.05),]
B2 <- B1[(PT[,3]<0.05),]
alp5[,1]

# plot each predictors
which(PT[,3]>0.05)
Bnew <- B[,-4]
B1new <- B1[,-3]

par(mfrow=c(3,4))
for (i in 1:(n-1)){
  plot(B1new[,i], Bnew[,1], main=alp5[i,1], xlab="x", ylab="Boston$crim") ##ok
  abline(coef(lm(Bnew[,1]~B1new[,i])), lwd=3, col="red")
}
par(mfrow=c(1,1))

# d, Fit a multiple regression model to predict the response using all of the predictors.
#mul <- lm(formula=B$crim~B1$zn+B1$indus+B1$chas+B1$nox+B1$rm+B1$age+B1$dis+B1$rad+B1$tax+B1$ptratio+B1$black+B1$lstat+B1$medv)
mul <- lm(formula=B$crim~B1$zn+B1$indus+B1$chas+B1$nox+B1$rm+B1$age+B1$dis+B1$rad+B1$tax+B1$ptratio+B1$black+B1$lstat+B1$medv)$coefficients

summary(mul)
anova(lm(formula=B$crim~B1$zn+B1$indus+B1$chas+B1$nox+B1$rm+B1$age+B1$dis+B1$rad+B1$tax+B1$ptratio+B1$black+B1$lstat+B1$medv))
confint(lm(formula=B$crim~B1$zn+B1$indus+B1$chas+B1$nox+B1$rm+B1$age+B1$dis+B1$rad+B1$tax+B1$ptratio+B1$black+B1$lstat+B1$medv))


# e, already created, check alp5

# f, 
plot(PT$Beta1_hat, mul[2:14], xlab="Univariate", ylab="Multiple")
# g)Comment: from the plot, can see that most of the univariate and multiple regression grouped together at the upper left cornor with the values approximation agreed. There is one outlier in this plot where univariate of one predictor is very high around 30 but multiple regression shows that this predictor has a very strong negative coefficient with the response of around -10. I would say Multiple can give a more accurate prediction. Because single predictors correspond to the response more precisely to the predictor, the parameter associated will be more accurate because the interval due to the confidence level is narrower compared to that of the multiple fitting. 

#anova(lm(Bnew$crim~B1new$medv),lm(Bnew$crim ~ poly(B1new$medv, 3, raw=TRUE)))$'F'[2]
#anova(lm(Bnew$crim~B1new$medv),lm(Bnew$crim ~ poly(B1new$medv, 3, raw=TRUE)))$'Pr(>F)'[2]
g <- matrix(rep(0),nrow=n-1, ncol=3) # PT = PredictTable
colnames(g) <- c("Predictor", "fstat", "pvalueofFstat")
g[,1] <- names(B1new) # fill Predictors

#### cannot use this one, dont know why. Error: invalid type (list)
#for (i in 1:n-1){
#  g[i,2] <- anova(lm(Bnew$crim~B1new[,i]),lm(Bnew$crim ~ poly(B1new[,i], 3, raw=TRUE)))$'F'[2] #fill the "F-Statistic" column of table g.
#  g[i,3] <- anova(lm(Bnew$crim~B1new[,i]),lm(Bnew$crim ~ poly(B1new[,i], 3, raw=TRUE)))$'Pr(>F)'[2] #fill the "p-Value" column of the table g.
#}

for (i in 1:12){
  mode1 <- lm(Bnew$crim~B1new[,i])                                                           
  mode2 <- lm(Bnew$crim ~ poly(B1new[,i], 3, raw=TRUE))
  g[i,2] <- anova(mode1, mode2)$'F'[2]  
  g[i,3] <- anova(mode1, mode2)$'Pr(>F)'[2]
}
g <- data.frame(g, stringsAsFactors = FALSE)
g[,2] <- as.numeric(g[,2])
g[,3] <- as.numeric(g[,3])
g <- g[order(g$pvalueofFstat),] # Sort by pvalueofFstat in ascending order.

#delete p>0.05, keep p<0.05 to reject null hypothesis
alpha5 <- g[(g[,3]<0.05),]
alpha5[,1] # we can reject the null hypothesis with these predictors.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      




