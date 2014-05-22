attach(mtcars)

# Scatterplot Matrices from the car Package
library(car)
scatterplot(mpg ~ wt | cyl, data=mtcars, 
            xlab="Weight of Car", ylab="Miles Per Gallon", 
            main="Enhanced Scatter Plot", 
            labels=row.names(mtcars))

# Scatterplot Matrices from the car Package
scatterplot.matrix(~mpg+disp+drat+wt|cyl, data=mtcars, main="Three Cylinder Options")



# High Density Scatterplot with Color Transparency 
x <- rnorm(1000)
y <- rnorm(1000) 
plot(x,y, main="Scatterplot Example", col=rgb(0,100,0,50,maxColorValue=255), pch=16)



# Spinning 3d Scatterplot
library(rgl)
plot3d(wt, disp, mpg, col=gear, size=10)


# Another Spinning 3d Scatterplot
library(Rcmdr)
scatter3d(wt, disp, mpg)


# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane 
library(scatterplot3d) 
s3d <-scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
fit <- lm(mpg ~ wt+disp) 
s3d$plane3d(fit)
