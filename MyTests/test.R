library(XML)

cleanHeader <- function(tmp){
        tmp[,3] <- as.character(tmp[,3])
        first <- colnames(tmp)
        tmp <- rbind(first,tmp)
        colnames(tmp) <- c("US","UK","Title")
        tmp      
}

#-------A-K Catalogue
url <- "http://netflixukvsusa.blogspot.it/2014/05/alphabetical-list-k-wed-may-21-2014.html"
tables <- readHTMLTable(url)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))

#cleaning
tmp <- cleanHeader(tables[[1]])
data <- rbind(tmp,cleanHeader(tables[[2]]))

#get flags
html <- htmlTreeParse(url, useInternalNodes=T)
tmp <- xpathApply(html, "//td")
tmp <- tmp[1:(length(tmp)-2)]

data$US <- as.logical(data$US)
data$UK <- as.logical(data$UK)

for(i in 0:(length(data[,1])-1)){
        us <- xmlChildren(tmp[[(3*i)+1]])$img
        uk <- xmlChildren(tmp[[(3*i)+2]])$img
        data[i,1] <- (length(us) > 0)
        data[i,2] <- (length(uk) > 0)
        
}