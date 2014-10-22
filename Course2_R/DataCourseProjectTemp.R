## This is the script for extarcting data from UCI HAR dataset provided with the assignment
## and tyding it as per the assignments requirements

##         *** required packages  ***
## In addition to base packages the following additional packages are used in this 
## script. reshape2


## check if reshape library is loaded
if (!require("reshape2")) { 
  install.packages("reshape2") 
} 

library(reshape2)



## For the purpose of this assignment it is assumed that the zip file provided with the assignment
## is downloaded and extracted under the data folder. The expected folder structure is as below

## ....../              <- base folder under which run_analysis.R recides. This is also current working directory
## ....../UCI HAR Dataset/         <- UCI HAR dataset directory. This directory has files such as activity_labels.txt, features.txt etc
## ....../UCI HAR Dataset/test     <- UCI HAR dataset test files
## ....../UCI HAR Dataset/train    <- UCI HAR dataset training files


## define directory variables
datadir="./UCI HAR Dataset/" 
testdir=paste(datadir,"test/",sep="") 
traindir=paste(datadir,"train/",sep="")

## step 0 - read activity labels and features

# read activity lables
actLables <- read.table(paste(datadir, "activity_labels.txt", sep = ""), 
                        sep = " ", strip.white = TRUE)

# read features
features <- read.table(paste(datadir, "features.txt", sep = ""), 
                        sep = " ", strip.white = TRUE)

## step 1 - read training and test datasets and merge into single dataset

## read training datasets - assign col names as we read data
trainSubject <- read.table(paste(traindir, "subject_train.txt", sep = ""))
trainLabel <- read.table(paste(traindir, "y_train.txt", sep = ""))
trainSet <- read.table(paste(traindir, "X_train.txt", sep = ""))

## merge training datasets - subject and activity label with training set
trainAll <- cbind(trainLabel, trainSubject, trainSet)

## release temp datasets
rm(list = c("trainSet", "trainLabel", "trainSubject"))

## read test datasets
testSubject <- read.table(paste(testdir, "subject_test.txt", sep = ""))
testLabel <- read.table(paste(testdir, "y_test.txt", sep = ""))
testSet <- read.table(paste(testdir, "X_test.txt", sep = ""))

## merge test datasets - subject and activity label with training set
testAll <- cbind(testLabel, testSubject, testSet)


## release temp datasets
rm(list = c("testSet", "testLabel", "testSubject"))

## merge training and test datasets to create a single dataset
dataAll <- rbind(trainAll, testAll)

rm(list = c("testAll", "trainAll"))

# assign column names to dataset
colnames(dataAll) <- c("Activity", "Subject" ,as.character(features[, 2]))

## step 2 - extract measurements on mean and SD
requiredVars <- grep("Subject|Activity|mean\\(\\)|std\\(\\)",colnames(dataAll), value = TRUE)

# create tidy dataset
tidyData <- dataAll[requiredVars]

rm(list = c("dataAll", "requiredVars"))

## step 3 - name activities in the dataset

actDescLabel <- actLables[tidyData$Activity, 2]

tidyData <- cbind(ActivityLabel = actDescLabel, tidyData)

## step 4 - label variable names => completed above

## step 5 - label variable names => completed above

tMelt <- melt(tidyData, id=c("ActivityLabel", "Subject"), na.rm = TRUE)

newTidyData <- dcast(tMelt, ActivityLabel + Subject ~ variable, mean)








