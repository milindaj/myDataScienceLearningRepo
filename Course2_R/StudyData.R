https://gist.github.com/labeneator/6833376#file-rmysql_works-sh


fgrep MYSQL_HOME /Library/Frameworks/R.framework/Resources/etc/Renviron
MYSQL_HOME="/usr/local/mysql/"


ln -s /usr/local/mysql/lib/libmysqlclient.18.dylib  /Library/Frameworks/R.framework/Resources/lib

export PKG_LIBS="-L/usr/local/mysql/lib -lmysqlclient"
export PKG_CPPFLAGS="-I/usr/local/mysql/include"

R CMD INSTALL  /Users/milindaj/Downloads/RMySQL_0.9-3.tgz 
or
install.packages("RMySQL", type = "source")

library(RMySQL)

##Read my SQL

ucscDB <- dbConnect(MySQL(), user="genome", host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDB, "show databases;"); 
dbDisconnect(ucscDB);

hg19 <- dbConnect(MySQL(), user="genome", db="hg19", host="genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
head(allTables)

dbListFields(hg19, "acemblyPep")
dbListFields(hg19, "affyU133Plus2")

dbGetQuery(hg19, "select count(*) from acemblyPep")

##someData <- dbReadTable(hg19, "acemblyPep")

query <- dbSendQuery(hg19, "select * from affyU133Plus2 where misMatches between 1 and 3")
affMis <- fetch(query)
affMisSmall <- fetch(query, n = 10)
dbClearResult(query)
dbDisconnect(ucscDB);


## read web
library(XML)
fileURL <- "http://mmb.moneycontrol.com/"
fileURL <- "http://mmb.moneycontrol.com/stock-message-forum/icici-bank/comments/6422"
doc <- htmlTreeParse(fileURL, useInternalNodes=TRUE)
saveXML(doc, file="ex.txt")
xpathSApply(doc, "//title", xmlValue)
xpathSApply(doc, "//div/a", xmlValue)
xpathSApply(doc, "//a[@class='bl_14']/strong", xmlValue)
xpathSApply(doc, "//div[@class='info']", xmlValue)


## read web - httr package
library(httr)
html2 <- GET(fileURL)
content2 <- content(html2, as="text")
parsedHTML <- htmlParse(content2, asText=TRUE)
xpathSApply(parsedHTML, "//title", xmlValue)
xpathSApply(parsedHTML, "//div[@class='info']", xmlValue)

divs2 <- xpathSApply(doc, "//div")

txt <- getURL(url=fileURL)

doc2 <- as(doc, "character")
parsedDoc <- xmlParse(doc2)
xpathSApply(doc, "//title", xmlValue)
xpathSApply(doc, "//div/a", xmlValue)
xpathSApply(doc, "//a[@class='bl_14']/strong", xmlValue)
xpathSApply(doc, "//div[@class='info']", xmlValue)

f = system.file("examples", "index.html", package = "XML")
htmlTreeParse(readLines(f), asText = TRUE)
htmlTreeParse(readLines(f))


'<div class="offer-name">
  <a href="http://www.somesite.com" itemprop="name">Fancy Product</a>
  </div>' -> xData
library(XML)
parsedHTML <- xmlParse(xData)
Products <- xpathSApply(parsedHTML, "//div[@class='offer-name']", xmlValue) 
hrefs <- xpathSApply(parsedHTML, "//div/a", xmlGetAttr, 'href')


## Download file chapter
if(!file.exists("data")) {
    dir.create("data")
}


fileURL <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileURL, destfile="./data/cameras.csv", method="curl")
cameradata <- read.table("./data/cameras.csv", sep=",", head = TRUE)

fileXLUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileXLUrl, destfile="./data/cameras.xlsx", method="curl")
install.packages("xlsx")
library(xlsx)

camData <- read.xlsx("./data/cameras.xlsx", sheetIndex=1, header=TRUE)

colIndex <- 2:3
rowIndex <- 1:4

camData <- read.xlsx("./data/cameras.xlsx", sheetIndex=1, colIndex=colIndex, rowIndex=rowIndex)


## reading XML
library(XML)
fileXMLUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileXMLUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
rootNode[[1]]
xmlSApply(rootNode, xmlValue)

## xpath
xpathSApply(rootNode, "//name", xmlValue)
xpathSApply(rootNode, "//price", xmlValue)


## json
library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/milindaj/repos")
names(jsonData)
names(jsonData$owner)

## data.table
install.packages("data.table")
library(data.table)
DT <- data.table(x=rnorm(9), y=rep(c("a", "b", "c"), each=3), z=rnorm(9))
DT[,list(mean(x),sum(z))]
DT[,w:=z^2]


## Quiz 1
qFileUrl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv "
download.file(qFileUrl, destfile="./data/acs.csv", method="curl")
ACS <- read.table("./data/acs.csv", sep=",", head = TRUE)
head(ACS)

#Q1
ACSAll <- ACS$VAL[!is.na(ACS$VAL)]
SUM(ACSAll == 24)

#Q3

fileXLUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileXLUrl, destfile="./data/ngap.xlsx", method="curl")

colIndex <- 7:15
rowIndex <- 18:23

dat <- read.xlsx("./data/ngap.xlsx", sheetIndex=1, colIndex=colIndex, rowIndex=rowIndex)
sum(dat$Zip*dat$Ext,na.rm=T) 

#Q4

fileXMLUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileXMLUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)


##Quiz 2, Q2 
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", "acs.csv", method="curl")
acs <- read.csv("acs.csv")

install.packages("sqldf")

#Q4

file <- getURL("http://biostat.jhsph.edu/~jleek/contact.html")
library(XML)
lns <- readLines(file)

#Q5
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for", "q2q4.xyz", method="curl")
x2 <- read.fwf(
    file="q2q4.xyz",
    skip=4,
    widths=c(12, 7,4, 9,4, 9,4, 9,4))


## quiz 3

setwd("/Users/milindaj/web/r/repos/myDataScienceLearningRepo/Course3_Data")
# q1

qFileUrl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(qFileUrl, destfile="./data/acsIdaho.csv", method="curl")
ACSIdaho <- read.table("./data/acsIdaho.csv", sep=",", head = TRUE)

ACSIdaho_df <- tbl_df(ACSIdaho)

#householde > 10 acre and $10000 products
filter(ACSIdaho_df, ACR == 3, AGS == 6)

agricultureLogical <- ACSIdaho[ACSIdaho$ACR == 3 & ACSIdaho$AGS == 6,]

ACSIdaho$condtion <- ifelse(ACSIdaho$ACR == 3 & ACSIdaho$AGS == 6, TRUE, FALSE)
which(ACSIdaho$condtion) 

#Q2
install.packages("jpeg")
library(jpeg)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", destfile="./data/jpegFile.jpg", method="curl")
jpegData <- readJPEG("./data/jpegFile.jpg", native = TRUE)

quantile(jpegData, probs=c(0.3, 0.8))


#q3
qFileUrl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(qFileUrl, destfile="./data/gdpData.csv", method="curl")
gdpData <- read.csv("./data/gdpData.csv", sep=",", head = TRUE, skip = 4, nrows=190)
colnames(gdpData) <- c("shortcode", "num", "x", "conutry", "A", "B", "C", "D", "E" )

qFileUrl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(qFileUrl2, destfile="./data/eduData.csv", method="curl")
eduData <- read.csv("./data/eduData.csv", sep=",", head = TRUE)

mData <- merge(gdpData, eduData, by.x = "shortcode", by.y = "CountryCode")
mData2 <- merge(eduData, gdpData, by.y = "shortcode", by.x = "CountryCode")
m2Ord <- mData2[order(mData2$num, decreasing = TRUE), ]
m2Ord[13, ]

#q4
OECD <- mData[mData$Income.Group == "High income: OECD", ]
mean(OECD$num)

nonOECD <- mData[mData$Income.Group == "High income: nonOECD", ]
mean(nonOECD$num)

#q5
install.packages("Hmisc")
library(Hmisc)
mData$GDPGroup <- cut2(mData$num, g = 5)
table(mData$GDPGroup, mData$Income.Group)


## xpath
zips <- xpathSApply(rootNode, "//zipcode", xmlValue)
xpathSApply(rootNode, "//price", xmlValue)

system.time(fread("household_power_consumption.txt"))
system.time(fread("household_power_consumption.txt", colClasses=c("charecter", "charecter", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings="?"))
system.time(fread("household_power_consumption.txt", sep=";", colClasses=c("charecter", "charecter", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings="?"))

hpcDT <- fread("household_power_consumption.txt", sep=";", colClasses=c("charecter", "charecter", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings="?")

hpcDF2 <- read.table("household_power_consumption.txt", sep=";", colClasses=c("charecter", "charecter", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings="?")

hpcDTTemp <- fread("household_power_consumption.txt", select = 1, sep=";", colClasses=c("charecter", "charecter", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings="?")


hpcDT <- fread("household_power_consumption.txt", sep=";", colClasses=c("charecter", "charecter", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings="?")
hpcDT <- hpcDT[Date == "1/1/2007" | Date == "2/1/2007"]
hpcDF <- as.data.frame(hpcDT)
hpcDF$Date <- as.Date(hpcDF$Date, "%d/%m/%Y")

hpcDF$Global_reactive_power <- as.numeric(hpcDF$Global_reactive_power)
hpcDF$Global_active_power <- as.numeric(hpcDF$Global_active_power)

hist(hpcDF$Global_active_power, col="red")


## HDF5

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")


# Demo speedup
n=1e6
DT = data.table( a=sample(1:1000,n,replace=TRUE),
                 b=sample(1:1000,n,replace=TRUE),
                 c=rnorm(n),
                 d=sample(c("foo","bar","baz","qux","quux"),n,replace=TRUE),
                 e=rnorm(n),
                 f=sample(1:1000,n,replace=TRUE) )
DT[2,b:=NA_integer_]
DT[4,c:=NA_real_]
DT[3,d:=NA_character_]
DT[5,d:=""]
DT[2,e:=+Inf]
DT[3,e:=-Inf]

write.table(DT,"test.csv",sep=",",row.names=FALSE,quote=FALSE)

cat("File size (MB):", round(file.info("test.csv")$size/1024^2),"\n")


## tydy data
library(reshape2)

mtcars$carname <- rownames(mtcars)

carMelt <- melt(mtcars, id=c("carname", "gear", "cyl"), measure.vars=c("mpg", "hp"))


## course project

fileLoc <- "D:/work/r/Class_Data/data/UCI HAR Dataset/train/X_train.txt"

colClasses <- c(rep("numeric", 561))

xTrain <- read.table(fileLoc, sep = " ", colClasses = colClasses, nrow = 1)



fileLoc2 <- "D:/work/r/Class_Data/data/UCI HAR Dataset/train/X_train2.txt"

widths <- c(rep(16, 531))
xTrain2 <- read.fwf(fileLoc, widths, n = 1000)


