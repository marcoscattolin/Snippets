library(stringr)
library(XML)

loadHTML <- function(url){
        html <- htmlTreeParse(url, useInternalNodes=T)
}

arrangeData <- function(html){
        
        #get titles and metadata
        titles <- xpathSApply(html, "//td/b/a", xmlValue)
        years <- xpathSApply(html, "//td/b", xmlValue)
        meta <- xpathSApply(html, "//td/i", xmlValue)
        imdbLinks <- xpathSApply(html, "//td/i/a", xmlGetAttr,"href")
        data <- data.frame(cbind(titles,years,meta,imdbLinks))
        
        
        #get flag values
        data$us <- NA
        data$uk <- NA
        html <- htmlTreeParse(url, useInternalNodes=T)
        tmp <- xpathApply(html, "//td")
        tmp <- tmp[1:(length(tmp)-2)]
        
        for(i in 1:length(data[,1])){
                us <- xmlChildren(tmp[[(3*(i-1))+1]])$img
                uk <- xmlChildren(tmp[[(3*(i-1))+2]])$img
                data$us[i] <- (length(us) > 0)
                data$uk[i] <- (length(uk) > 0)
                
        }
        
        #clean values for years
        titlesLength <- nchar(as.character(data$titles))+2
        yearsLength <- nchar(as.character(data$years))
        data$years <- str_sub(as.character(data$years),titlesLength,yearsLength)
        data$years <- gsub("[^0-9\\-]","",data$years)
        
        #clean values for rankings and time
        tmp <- strsplit(as.character(data$meta),",")
        tmp <- data.frame(tmp)
        tmp <- t(tmp)
        data$rankings <- str_trim(tmp[,1])
        data$rankings <- gsub("[^0-9\\.]","",data$rankings)
        data$time <- str_trim(tmp[,2])
        data$time <- str_sub(data$time,1,nchar(as.character(data$time))-6)
        
        
        #assign class to columns
        data$titles <- factor(data$titles)
        data$years <- factor(data$years)
        data$time <- factor(data$time)
        data$rankings <- as.numeric(data$rankings)
        data$startingYear <- factor(substr(data$years,1,4))
        
        data
        
}


#-------A-K Catalogue
url <- "http://netflixukvsusa.blogspot.it/2014/05/alphabetical-list-k-wed-may-21-2014.html"
data <- arrangeData(loadHTML(url))


#-------K-z Catalogue
url <- "http://netflixukvsusa.blogspot.it/2014/05/alphabetical-list-k-z-wed-may-21-2014.html"
data2 <- arrangeData(loadHTML(url))

data <- rbind(data,data2)


#checkValues
print(length(data[,1]))
print(sum(data$us))
print(sum(data$uk))



#Further clean times
library(lubridate)
data$castTime <- gsub(" ","",data$time)
data$castTime <- gsub("minutes","m",data$castTime)
data$castTime <- gsub("min","m",data$castTime)
regex <- "^[0-9]*m$"
data$castTime[grep(regex,data$castTime)] <- paste0("0hr",data$castTime[grep(regex,data$castTime)])
regex <- "hr$"
data$castTime[grep(regex,data$castTime)] <- paste0(data$castTime[grep(regex,data$castTime)],"0m")
data$castTime <- gsub("hrs","hr",data$castTime)
data$castTime <- hm(data$castTime)


#check time cleaning and alocate Series Flag
unique(data$time[is.na(data$castTime)])
sum(is.na(data$castTime))
data$contentType <- is.na(data$castTime)
data$contentType <- ifelse(data$contentType,"Tv Series","Movie")

#define country variable
data$country <- ifelse(data$us & data$uk,"Both",ifelse(data$us,"US only","UK only"))



#interesting plots
table(data$us,data$uk)
hist(data$rankings)
barplot(table(data$startingYear))

library(ggplot2)
qplot(data = data, startingYear,, facets=country~contentType)





qplot(startingYear, data=data, fill=contentType, alpha=I(.5), 
      main="Distribution of Gas Milage", xlab="Miles Per Gallon", 
      ylab="Density")


