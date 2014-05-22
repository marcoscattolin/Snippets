# Interacting with a scatterplot 
attach(mydata)
x <- rnorm(100)
y <- rnorm(100)
plot(x, y) # scatterplot
identify(x, y, labels=row.names(mydata)) # identify points 
coords <- locator(type="l") # add lines
coords # display list


