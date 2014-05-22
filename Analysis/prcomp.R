# Author: Steve Pittard - wsp@emory.edu, ticopittard@gmail.com 
# This video is in support of the following YouTube video:

# Using prcomp and varimax for PCA in R www.youtube.com/watch?v=PSuvMBtvJcA 
library(lattice)
library(gclus)

my.wines <- read.csv("./wines.csv", header=TRUE)
row.names(my.wines) <- my.wines[,1]
my.wines <- my.wines[,-1]

# Look at the correlations
my.abs     <- abs(cor(my.wines))
my.colors  <- dmat.color(my.abs)
my.ordered <- order.single(cor(my.wines))
cpairs(my.wines, my.ordered, panel.colors=my.colors, gap=0.5)

# Do the PCA 
my.prc <- prcomp(my.wines, center=TRUE, scale=TRUE)
summary(my.prc)


#retain only components having eigenvalues > 1
my.prc$sdev ^ 2   #equivalent to eigenvalues
screeplot(my.prc, main="Scree Plot", xlab="Components")
screeplot(my.prc, main="Scree Plot", type="line" )


# DotPlot PC1
load    <- my.prc$rotation
sorted.loadings <- load[order(load[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

# DotPlot PC2
sorted.loadings <- load[order(load[, 2]), 2]
myTitle <- "Loadings Plot for PC2"
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

# Now draw the BiPlot -- cosine of the angle between arrays is the correlation between them
biplot(my.prc, cex=c(1, 0.7))

# Apply the Varimax Rotation
my.var <- varimax(my.prc$rotation)



