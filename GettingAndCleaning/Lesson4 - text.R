#--------------------------Editign text
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
destfile <- ".\\camera.csv"
download.file(fileUrl, destfile=destfile)


#read csv data
cameraData <- read.csv(destfile)
head(cameraData)

names(cameraData) <- tolower(names(cameraData))
splitnames <- strsplit(names(cameraData),"\\.")


sub("\\.","",names(cameraData)) #sostituisci il primo "."
gsub("\\.","",names(cameraData)) #sostituisci tutti i "."



grep("Alameda",cameraData$intersection)  #find rows including string pattern
grep("Alameda",cameraData$intersection, value=TRUE)  #find strings including string pattern

grepl("Alameda",cameraData$intersection)  #logical vector defining if string pattern exists


library(stringr)
nchar("ciao")
substr("ciao",2,10)
paste("ciao","ciao")
paste0("ciao","ciao")

str_trim("ciao     ")



#--------------------------Regular Expressions
^i think  #beginnig with
morning$  #ending with
[Bb][Uu][Ss][Hh]  #match bush regardless of case
^[Ii] am  #I am OR i am
^[0-9][a-zA-Z]   #one number and then any letter
[^?.]$ # strings not ending with ? or .
9.11 #any character bewtween 9 and 11
flood|fire  #logical or
^[Gg]ood | [Bb]ad #beginning with good or Goog or including Bad or bad
^([Gg]ood | [Bb]ad) #beginning with good or Goog or beginning Bad or bad
[Gg]eorge([Ww]\.)? [Bb]ush #including george bush with potentially(ie. ?) w. or W.


[0-9]+(.*)[0-9]+  #at least one number followed by anny number of characters followed by at least on number
[Bb]ush( +[^ ]+ +){1,5} debate #search for bush space anyword space debate (with mid section repeated up to 5 times)
 +([A-Za-z]+) +\1 +  #\1 indicates repetitions of the same pattern

        
        
        
#--------------------------Dates
date()        
d2 <- Sys.Date()        
        
format(d2,"%a %d %b, %A %B %Y %y")


z <- c("1gen1996")

a <- as.Date(z, "%d%b%Y", )
weekdays(a)
months(a)
julian(a)

install.packages("lubridate")
library(lubridate)

ymd("20040305")
dmy("03-05-2005")

ymd_hms("2010-06-05 10:10:10", tz="CET")





