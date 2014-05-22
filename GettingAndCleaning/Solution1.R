#QUESTION1&2
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url, destfile = "american.csv", method = "curl")
data <- read.csv("american.csv")
a <- data[data$VAL==24,2]

#QUESTION3
library(xlsx)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(url, destfile = "ngap.xlsx", method = "curl")
dat <- read.xlsx("ngap.xlsx", sheetIndex=1, colIndex=7:15, rowIndex=18:23)
sum(dat$Zip*dat$Ext,na.rm=T)

#QUESTION4
library(XML)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(url, destfile = "rest.xml", method = "curl")
doc <- xmlTreeParse("rest.xml",useInternalNodes=TRUE)
rootNode <- xmlRoot(doc)
rootNode[[1]]
zips <- xpathSApply(rootNode,"//zipcode", xmlValue)
table(zips)

#QUESTION5
library(data.table)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(url, destfile = "american.csv", method = "curl")
DT <- fread("american.csv")
system.time(DT[,mean(pwgtp15),by=SEX])
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
