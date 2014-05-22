#set working directory
setwd("C:\\course-R\\Getting and Cleaning Data")


#create directory
if(!file.exists("data")){
        dir.create("data")
}

#---------CSV FILE
#download file (use methd = "curl" when on Mac and downloading from https)
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
destfile <- ".\\data\\camera.csv"
download.file(fileUrl, destfile=destfile)
dateDownloaded <- date()

#load csv data
cameraData <- read.csv(destfile)
head(cameraData)


#---------XLSX FILE
#download file (use methd = "curl" when on Mac and downloading from https)
library(xlsx)
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
destfile <- ".\\data\\camera.xlsx"
download.file(fileUrl, destfile=destfile)
dateDownloaded <- date()

#partially load xlsx data
colIndex <- 1:3
rowIndex <- 1:4
cameraData <- read.xlsx(destfile, sheetIndex=1, header=TRUE, colIndex=colIndex, rowIndex=rowIndex)
head(cameraData)

#---------XML FILE
library(XML)
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl, useInternalNodes=TRUE)

#decode html file
scores <- xpathSApply(doc,"//li[@class='score']",xmlValue)
teams <- xpathSApply(doc,"//li[@class='team-name']",xmlValue)





