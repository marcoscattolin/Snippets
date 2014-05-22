#-------MYSQL
library(RMySQL)

#connect to server and get databases
ucscDb <- dbConnect(MySQL(), user="genome", host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb, "show databases;")
dbDisconnect(ucscDb)

#connect to database and get tables
hg19 <- dbConnect(MySQL(), user="genome", db = "hg19",host="genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)

#get columns
dbListFields(hg19,"affyU133Plus2")

#query table
dbGetQuery(hg19,"select count(*) from affyU133Plus2")

#download table
affyData <- dbReadTable(hg19,"affyU133Plus2")
head(affyData)

#async query
query <- dbSendQuery(hg19,"select * from affyU133Plus2 where misMatches between 1 and 3")
affyMis <- fetch(query, n=10)   #fecth only 10 rows
dbClearResult(query)            #clear query from remote server
quantile(affyMis$misMatches)

dbDisconnect(hg19)


#-------HDF5
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)

#create file, group and list
created = h5createFile("example.h5")
created = h5createGroup("example.h5","foo")
created = h5createGroup("example.h5","foo/baa")
h5ls("example.h5")
A = 3
h5write(A,"example.h5","foo/baa")


#-------WEB
con <- url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode = readLines(con)
close(con)
nchar(htmlCode[100])

#based on xml library
library(XML)
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes=T)
xpathSApply(html, "//td[@id='col-citedby']", xmlValue)

#based on httr library, to be used for websites requesting auth
install.packages("httr")
library(httr)
url <- "http://httpbin.org/basic-auth/user/passwd"
html2 <- GET(url, authenticate("user","passwd"))
names(html2)
content2 <- content(html2,as="text")
parsedHtml <- htmlParse(content2, asText=T)
xpathSApply(parsedHtml, "//td[@id='col-citedby']", xmlValue)

google <- handle("http://google.com")
pg1 <- GET(handle=google,path="/")
pg2 <- GET(handle=google,path="search")


#-------API
install.packages("RJSONIO")
install.packages("jsonlite")
install.packages("httpuv")
library(httr)
library(RJSONIO)
library(jsonlitemd)
library(httpuv)
myapp <- oauth_app("twitter", key = "JUzVM7W7icFd7oE5ffOviO66U", secret="LCDTiHYiNy2lk5GeXDZWaF7GtirkiWcFcG9Ohkhqv03ZSquzl5")
sig <- sign_oauth1.0(myapp, token="myaccestokenscatto80", token_secret="myaccestokenscatto80secret")
homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json", sig)
json1 = content(homeTL)
json2 = jsonlite::fromJSON(toJSON(json1))
json2[1,1:4]



#github
oauth_endpoints("github")
myapp <- oauth_app("Testapp", key = "fa498509dfd801fb0d5a",secret="10c25e105539bbc7e1f07631007c945bcb74d945")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
req <- GET("https://api.github.com/users/jtleek/repos", config(token = github_token))
json1 = content(req)
json2 = jsonlite::fromJSON(toJSON(json1))
json2[1,1:4]


#fixed width
file <- "getdata-wksst8110.for"
widths <- c(10,5,4,4,5,4,4,5,4,4,5,4,4)
a <- read.fwf(file,widths,skip=4)
sum(a$V6) #fourth column, sum only SST data = 32426.7
