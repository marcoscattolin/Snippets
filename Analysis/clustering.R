#------------HIERARCHICAL CLUSTERING


#generate data
set.seed(1234)
x <- rnorm(12, mean=rep(1:3, each=4), sd =0.2)
y <- rnorm(12, mean=rep(c(1,2,1), each=4), sd =0.2)

#plot them
par(mar = c(0,0,0,0))
plot(x,y, col = "blue", pch=19, cex=2)
text(x,y,pos=3, labels=as.character(1:12))


#set data frame
dataFrame <- data.frame(x=x,y=y)

#calculate distances between points
distxy <- dist(dataFrame, method="euclidean")

#run clustering and plot
hClustering <- hclust(distxy)
plot(hClustering)


#hierarchical on rows and columns and table
dataFrame <- data.frame(x=x,y=y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)



#------------K-MEANS

#generate data
set.seed(1234)
x <- rnorm(12, mean=rep(1:3, each=4), sd =0.2)
y <- rnorm(12, mean=rep(c(1,2,1), each=4), sd =0.2)

#plot them
par(mar = c(0,0,0,0))
plot(x,y, col = "blue", pch=19, cex=2)
text(x,y,pos=3, labels=as.character(1:12))

#set data frame
dataFrame <- data.frame(x=x,y=y)

#calculate clustering with 3 centers
kmeansObj <- kmeans(dataFrame, centers=3)
kmeansObj

#plot clustering and centroids
plot(x,y, col = kmeansObj$cluster, pch=19, cex=2)
points(kmeansObj$centers, col=1:3, pch=3, cex=3,lwd=3)


