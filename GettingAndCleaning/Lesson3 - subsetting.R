#----------------subset and sort
set.seed(13435)
X <- data.frame("var1"=sample(1:5),"var2" = sample(6:10), "var3"=sample(6:10))
X <- X[sample(1:5),]
X$var2[c(1,3)] = NA

X[(X$var1 > 2 | X$var3 < 10),]

#use which when dealing with NA values
X[(X$var2 > 2),]
X[which(X$var2 > 2),]

#sort pushig Nas at the end
sort(X$var2,na.last=T)

#order data frame by column 1
X[order(X$var1),]

#order data frame by column 1 and then 3
X[order(X$var1,X$var3),]

#order with plyr
library(plyr)
arrange(X,desc(var1))

#add variable to dataset
X$var4 <- rnorm(5)



#----------------summarize
url <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"

resData <- read.csv(url, header=T)
summary(restData)
quantile(restData$councilDistrict, probs=c(0.5,0.9,1))

#add to summary table info about missing values
table(restData$zipCode, useNA="ifany")

sum(is.na(restData$councilDistrict)) #how many are NAs
any(is.na(restData$councilDistrict)) #is there any NA

all(restData$zipCode > 0)       #are all > 0


colSums(is.na(restData))                #how many NAs in each col
all(colSums(is.na(restData))==0)        #are all values != NAs


table(restData$zipCode %in% c("21212","21213"))

restData[restData$zipCode %in% c("21212","21213"),]


data(UCBAdmissions)
DF <- as.data.frame(UCBAdmissions)
summary(DF)
xt <- xtabs(Freq ~ Gender + Admit, data=DF) #breakdown Freq col over table gender and admit

#add col to warpbreaks dataset
warpbreaks$replicate <- rep(1:9, len=54)

xt <- xtabs(breaks ~.,data=warpbreaks) #break breaks col over all possible combinations
ftable(xt) #flat previous table to make it readable


fakeData <- rnorm(1e5)
print(object.size(fakeData), units="MB")   #print MB size of an object


#----------------create new vars
url <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
restData <- read.csv(url, header=T)

s1 <- seq(1,10,by=2)
s1 <- seq(1,10,length=3)
x <- c(1,3,8,)

restData$nearMe <- restData$neighborhood %in% c("Roland Park","Homeland") #create new var with logical value
table(restData$nearMe)


restData$zipWrong <- ifelse(restData$zipCode < 0, TRUE, FALSE) #return first term when logical cond is true
table(restData$zipWrong,restData$zipCode <0)

#define a factor variable (min,max] and break in 4 quantiles the data and def
restData$zipGroups <- cut(restData$zipCode, breaks=quantile(restData$zipCode))
table(restData$zipGroups,restData$zipCode)

library(Hmisc)
restData$zipGroups <- cut2(restData$zipCode, g=4) #as above

#create factor variable
restData$zcf <- factor(restData$zipCode)


yesno <- sample(c("yes","no"), size=10, replace=TRUE)
yesnofac = factor(yesno,levels=c("yes","no"))

as.numeric(yesnofac)
yesnofac <- relevel(yesnofac, ref="yes")  #set base level
as.numeric(yesnofac)


library(Hmisc)
library(plyr)

restData2 <- mutate(restData, zipGroups=cut2(zipCode,g=4))



#----------------reshaping
library(reshape2)
mtcars$carname <- rownames(mtcars)
carmelt <- melt(mtcars, id=c("carname","gear","cyl"),measure.vars=c("mpg","hp")) #melt data defining id variables and measure variables
head(carmelt)
tail(carmelt)


cylData <- dcast(carmelt, cyl ~ variable) #recast data defining 
cylData <- dcast(carmelt, cyl ~ variable,mean) #recast data using mean


tapply(InsectSprays$count,InsectSprays$spray,sum)

#same as above but using list
spIns <- split(InsectSprays$count,InsectSprays$spray)
sprCount <- lapply(spIns,sum)
unlist(sprCount) #reshape to vector

sapply(spIns,sum) #same as above but directly to vector


library(plyr)
ddply(InsectSprays,.(spray),summarise,sum=sum(count))
ddply(InsectSprays,.(spray),summarise,sum=ave(count,FUN=sum)) #create column for dataset with sum for different spray values


#merging data
fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
reviews <- read.csv(fileUrl1)
solutions <- read.csv(fileUrl2)



mergedData <- merge(reviews,solutions, by.x="solution_id",by.y="id",all=TRUE) #all = TRUE means outer join
head(mergedData)

intersect(names(reviews),names(solutions))
mergedData <- merge(reviews,solutions, all=TRUE)

library(plyr)
df1 <- data.frame(id=sample(1:9),x=rnorm(9))
df2 <- data.frame(id=sample(1:10),y=rnorm(10))
df3 <- data.frame(id=sample(1:10),z=rnorm(10))
dfList <- list(df1,df2,df3)
join_all(dfList,type="full") #fast join on one column

