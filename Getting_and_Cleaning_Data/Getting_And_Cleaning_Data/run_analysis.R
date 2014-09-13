library(RCurl)
library(plyr)
library(reshape2)

## Download data from the internet if not in data directory
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- paste0("https://d396qusza40orc.cloudfront.net/",
          "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip") 
destFile <- "./data/dataset.zip"
if(!file.exists(destFile)){
     download.file(fileUrl, destfile=destFile, method="curl")
}

## Unzip the dataset.zip file in the data directory
unzip("./data/dataset.zip", exdir="./data")

## Set the root directory of the unzipped data set
DataRoot <- "./data/UCI HAR Dataset/"

## Read in the test data
.TestDir <- paste0(DataRoot,"test/")
.xTestFile <- paste0(.TestDir,"X_test.txt")
.yTestFile <- paste0(.TestDir,"y_test.txt")
.SubjectTestFile <- paste0(.TestDir,"subject_test.txt")
SubjectTestData <- read.table(.SubjectTestFile, col.names="SubjectID")
xTestData <- read.table(.xTestFile)
yTestData <- read.table(.yTestFile, col.names="Activity")

## Read in the train data
.TrainDir <- paste0(DataRoot,"train/")
.xTrainFile <- paste0(.TrainDir,"X_train.txt")
.yTrainFile <- paste0(.TrainDir,"y_train.txt")
.SubjectTrainFile <- paste0(.TrainDir,"subject_train.txt")
SubjectTrainData <- read.table(.SubjectTrainFile, col.names="SubjectID")
xTrainData <- read.table(.xTrainFile)
yTrainData <- read.table(.yTrainFile, col.names="Activity")

## Read in activity labels and feature names
.ActivityFile <- paste0(DataRoot,"activity_labels.txt")
ActivityLabels <- read.table(.ActivityFile,
                             col.names=c("ActivityLevel", "ActivityLabel"))
.FeatureNameFile <- paste0(DataRoot,"features.txt")
FeatureNames <- read.table(.FeatureNameFile,
                           col.names=c("FeatureID","FeatureName"))

## Replace the column names in the x data with the feature names
## from the FeatureNameFile
colnames(xTestData) <- FeatureNames$FeatureName
colnames(xTrainData) <- FeatureNames$FeatureName

## Merge the y data with the activity labels
yTrainData <- merge(yTrainData,
                    ActivityLabels,
                    by.x="Activity", by.y="ActivityLevel",all=TRUE)
yTestData <- merge(yTestData,
                    ActivityLabels,
                    by.x="Activity", by.y="ActivityLevel",all=TRUE)

## Column bind the test and train data into combined data sets
TrainData <- cbind(SubjectTrainData, yTrainData, xTrainData)
TestData <- cbind(SubjectTestData, yTestData, xTestData)

## Remove the Activity column and rename ActivityLabel to Activity
TrainData <- subset(TrainData, select =-Activity)
TestData <- subset(TestData, select =-Activity)
names(TrainData)[names(TrainData)=="ActivityLabel"]<-"Activity"
names(TestData)[names(TestData)=="ActivityLabel"]<-"Activity"

## Merge the train and test data
# CompleteData<- merge(TrainData,TestData,
#                      by.x="SubjectID",by.y="SubjectID",all=TRUE)
CompleteData<- merge(TrainData,TestData,all=TRUE)
CompleteData <- CompleteData[order(CompleteData$SubjectID,
                                   CompleteData$Activity),]

## Convert the subject ID to factor
CompleteData$SubjectID <- factor(CompleteData$SubjectID)

## First remove hyphens and parentheses.  Then,find the column numbers of the 
## variables that have "mean" and "std" in their name
ColumnNames <- gsub("\\(\\)","",names(CompleteData))
ColumnNames <- gsub("-","",ColumnNames)
ColumnNames <- gsub("mean","Mean",ColumnNames)
ColumnNames <- gsub("std","Std",ColumnNames)
meanHeadings <- grep("Mean",ColumnNames)
stdHeadings <- grep("Std",ColumnNames)
desiredColumns <- sort(c(1,2,meanHeadings,stdHeadings))
colnames(CompleteData) <- ColumnNames

## Create subset of CompleteData with only the desired columns
FirstDataSet <- CompleteData[,desiredColumns]
## Remove columns with angle in title
AngleHeadings <- grep("angle",names(FirstDataSet))
FirstDataSet <- subset(FirstDataSet, select =-AngleHeadings)

## Write FirstDataSet as CSV file to disk into project directory
write.csv(FirstDataSet, file = "./firstdataset.txt")

## Get column names for ID and varables to melt the FirstDataSet
IDCols = names(FirstDataSet)[1:2]
numColsData = ncol(FirstDataSet)
VarCols = names(FirstDataSet)[3:numColsData]

## Melt the FirstDataSet
meltData <- melt(FirstDataSet,id=IDCols,measure.vars=VarCols)

## Generate table with summary of mean values per SubjectId and Activity
## for each variable
SummaryData <- ddply(meltData, .(SubjectID,Activity,variable),
                     summarize, mean=mean(value))

## Write the mean summarized data set as CSV to disk in project directory
write.csv(SummaryData, file = "./MeanSummaryDataSet.txt")

