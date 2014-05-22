#Q1
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
data <- read.csv(url)
a <- strsplit(names(data),"wgtp")

#Q1
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
data <- read.csv(url)
data <- data[5:194,]
GDP <- data$X.3
GDP <- str_trim(GDP)
GDP <- gsub(",","",GDP)
mean(as.numeric(GDP))

#Q3
countryNames <- data$X.2
grep("^United",countryNames)



#Q4
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
data1 <- read.csv(url)
data1 <- data1[5:194,1:5]
colnames(data1) <-c("CountryCode","GDPRank","x","Country","GDPValue")
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv "
data2 <- read.csv(url2)
merged <- join_all(list(data1,data2),type="inner") #fast join on one column
patterns <- merged$Special.Notes
patterns <- patterns[grep("Fiscal year end:",patterns)]
patterns[grep("[Jj]une",patterns)]




#Q5
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

years <- format(sampleTimes,"%Y")
sum(years=="2012")

days <- format(sampleTimes[(years=="2012")],"%a")
sum(days=="lun")
