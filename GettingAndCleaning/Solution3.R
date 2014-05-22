url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
data <- read.csv(url)


data$agricultureLogical <- (data$ACR == 3 & data$AGS == 6)

which(data$agricultureLogical)

install.packages("jpeg")
library(jpeg)
file <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(file, destfile="t.jpg")
data <- readJPEG("jeff.jpg",native=TRUE)
quantile(data,probs=c(0.3,0.8))


url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv "
download.file(url1,destfile="./GDP.csv")
download.file(url2,destfile="./STATS_Country.csv")

data1 <- read.csv("./GDP_v1.csv", sep = ";", header=FALSE)
colnames(data1) <-c("CountryCode","GDPRank","Country","GDPValue")
merged <- join_all(list(data1,data2),type="inner") #fast join on one column
merged <- arrange(merged,desc(GDPRank))
tapply(merged$GDPRank,merged$Income.Group,mean)
merged$QuantileGroup <- cut2(merged$GDPRank, g=5)
table(merged$QuantileGroup,merged$Income.Group)

