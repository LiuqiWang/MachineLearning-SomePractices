# Generalized Additive Models and Unsupervised Learning

rm(list=ls())
#####################
#### Part 1
#####################
# Calculate PVE in two ways using USArrests dataset 
# a)
d=USArrests
pr.out=prcomp(USArrests, scale=TRUE)
pr.var=pr.out$sdev^2 # for each variable? or for each PCp
pve=pr.var/sum(pr.var)

# b)
## if use pr.out$x
x=pr.out$x
# top+bottom
PVE=rep(0,4)
for (i in 1:4){
  PVE[i]=sum(x[,i]^2)/sum(x^2)
}

## if only use pr.out$rotation 
d1=d2=d3=d4=data.frame(scale(d,center = TRUE,scale = TRUE))
for (i in 1:4){
  d1[,i]=d1[,i]*pr.out$rotation[,1][i]
  d2[,i]=d2[,i]*pr.out$rotation[,2][i]
  d3[,i]=d3[,i]*pr.out$rotation[,3][i]
  d4[,i]=d4[,i]*pr.out$rotation[,4][i]
}
PC1=rowSums(d1)
PC2=rowSums(d2)
PC3=rowSums(d3)
PC4=rowSums(d4)

x=data.frame(cbind(PC1,PC2,PC3,PC4))
PVE1=rep(0,4)
for (i in 1:4){
  PVE1[i]=sum(x[,i]^2)/sum(x^2)
}

#####################
#### Part 2
#####################
# the use of correlation-based distance and Euclidean distance as dissimilarity measures for hierarchical clustering, turns out almost equivalent. 
# This code is to show that quantity 1-rij is proportional to the squared Euclidean distance between the ith and the jth observations.
set.seed(5072)
d=USArrests
d1=data.frame(scale(d,center = TRUE,scale = TRUE))
distance=dist(d1)
sq_distance=distance^2
r=as.dist(1-cor(t(d1)))
plot(r/sq_distance)

#####################
#### Part 3
#####################
# PCA and Clustering
# generate simulate data and perform PCA and K-means clustering on the data.
# a)
set.seed(5072)
n<-20
p<-50
x1<-matrix(rnorm(n*p),nrow=n,ncol=p)
x2<-matrix(rnorm(n*p),nrow=n,ncol=p)
x3<-matrix(rnorm(n*p),nrow=n,ncol=p)
for (i in 1:n){
  x1[i,] <- x1[i,]+rep(1,p)
  x2[i,] <- x2[i,]+rep(-1,p)
  x3[i,] <- x3[i,]+c(rep(+1,p/2),rep(-1,p/2))
}
x.values <- rbind(x1,x2,x3)
nrow(x.values)
ncol(x.values)

# b)
y.values <- c(rep(1,n),rep(2,n),rep(3,n))
length(y.values)

# c)
pr.out=prcomp(x.values, scale=TRUE)
vect = pr.out$x[,c(1,2)]
plot(vect,col=1:3)

# d) 
km.out=kmeans(x.values,3,nstart=20)
km.out$cluster
km.out$tot.withinss
table(km.out$cluster,y.values)

# e) 
km.out1=kmeans(x.values,2,nstart=20)
km.out1$tot.withinss
# comment: here we seperate the data into 2 clusters. The within cluster sum of square is 3589, which is 1000 more than sperating by 3 clusters. 
# comment: K=2 performs worse than K=3
# comment: only 51.6% proportion of variance explained.  

# f) 
km.out2=kmeans(x.values,4,nstart=20)
km.out2$tot.withinss
# comment: here we seperate the data into 4 clusters. The within cluster sum of square is 2724, which is a bit less than that of K=3.
# comment: K=4 performs better than K=3.
# comment: only 51.6% proportion of variance explained. 

# g) 
km.out3=kmeans(vect,3,nstart=20)
km.out3$tot.withinss
table(km.out3$cluster,y.values)
# comment: the k-means cluster seperate the data into 3 clusters, with each cluster contains 20 observations. 
# comment: 96.3% proportion of variance explained. 

# h) 
km.out4=kmeans(scale(x.values),3,nstart=20)
km.out4$tot.withinss
table(km.out4$cluster,y.values)
# comment: the k-means cluster seperate the data into 3 clusters, with each cluster contains 20 observations. 
# comment: 49.2% proportion of variance explained. 


