https://gist.github.com/labeneator/6833376#file-rmysql_works-sh


fgrep MYSQL_HOME /Library/Frameworks/R.framework/Resources/etc/Renviron
MYSQL_HOME="/usr/local/mysql/"


ln -s /usr/local/mysql/lib/libmysqlclient.18.dylib  /Library/Frameworks/R.framework/Resources/lib

export PKG_LIBS="-L/usr/local/mysql/lib -lmysqlclient"
export PKG_CPPFLAGS="-I/usr/local/mysql/include"

R CMD INSTALL  /Users/milindaj/Downloads/RMySQL_0.9-3.tgz

##Read my SQL

ucscDB <- dbConnect(MySQL(), user="genome", host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDB, "show databases;"); 
dbDisconnect(ucscDB);

## read web
fileURL <- "http://mmb.moneycontrol.com/"
fileURL <- "http://mmb.moneycontrol.com/stock-message-forum/icici-bank/comments/6422"
doc <- htmlTreeParse(fileURL, useInternalNodes=TRUE)
saveXML(doc, file="ex.txt")
xpathSApply(doc, "//title", xmlValue)
xpathSApply(doc, "//div/a", xmlValue)
xpathSApply(doc, "//a[@class='bl_14']/strong", xmlValue)
xpathSApply(doc, "//div[@class='info']", xmlValue)


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

## xpath
zips <- xpathSApply(rootNode, "//zipcode", xmlValue)
xpathSApply(rootNode, "//price", xmlValue)

#Q5

q5FileUrl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(qFileUrl, destfile="./data/acs.csv", method="curl")
ACS <- read.table("./data/acs.csv", sep=",", head = TRUE)
head(ACS)

system.time( {
  
  for(i in 1:10000) {
    DT[,mean(pwgtp15),by=SEX]
  }
  
})
