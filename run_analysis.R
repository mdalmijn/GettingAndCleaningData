## Script to produce a tidy dataset from the UCI HAR Dataset.
## The script requires the reshape and dplyr libraries to be installed.
## The working directory needs to be adjusted before running the script at line 12.

library(reshape)
library(dplyr)

## 1. Merge the training and the test sets to create one data set. 

## Set working directory to location of UCI HAR Dataset.

setwd("C:/Users/Dev/Documents/fit")

## Read in all necessary test and training data 
features <- read.table("features.txt")
activityType <- read.table("activity_labels.txt")
trainSubjects <- read.table("subject_train.txt")
testSubjects <- read.table("subject_test.txt")
xTraining <- read.table("X_train.txt")
xTest <- read.table("X_test.txt")
yTraining <- read.table("y_train.txt")
yTest <- read.table("y_test.txt")

## Label columns 
colnames(activityType) <- c("activityId", "activityType")
colnames(trainSubjects) <- "subject"
colnames(testSubjects) <- "subject"
colnames(xTraining) <- features[,2]
colnames(xTest) <- features[,2]
colnames(yTraining) <-  "activityId"
colnames(yTest) <- "activityId"

## Creating the test and training data sets and merging them.
trainingData <- cbind(trainSubjects, yTraining, xTraining)
testData <- cbind(testSubjects, yTest, xTest)
mergedData <- rbind(testData, trainingData)

## 2. Extract only the measurements on the mean and standard deviation for each measurement.

## Save all column names in a vector.
cNames <- colnames(mergedData)

## Create a logical vector that only returns TRUE for subjectId, activityId, mean() and std() and false for other columns.
logicalVector <- (grepl("subject", cNames) | grepl("activityId", cNames) | grepl("mean[(][)]", cNames) | grepl("std[(][)]", cNames))

## Select only columns from merged dataset that return TRUE in the logical vector.
subsetData <- mergedData[, logicalVector == TRUE]

## 3. Use descriptive activity names to name the acitivities in the data set.

## Merge subsetData with acitivityType to add descriptive activity names.
subsetData <- merge(subsetData,activityType,by='activityId');

## Removing unnecessary activityId and rearranging the data.
subsetData <- select(subsetData, -(activityId))
subsetData <- subsetData[,c(1,68,2:67)]

## 4. Appropriately label the data set with descriptive variable names. 

## Cleaning up variables names.
cNames <- colnames(subsetData)

for(i in 1:length(cNames)) {
       cNames[i] <- gsub("[-]|[(][)]","", cNames[i])
       cNames[i] <- gsub("std","Std", cNames[i])
       cNames[i] <- gsub("mean","Mean", cNames[i])
}

## Renaming columns of subsetData
colnames(subsetData) <- cNames

## 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
moltenData <- melt(subsetData, id=c("subject","activityType"))
finalData <- cast(moltenData, subject + activityType ~ variable, mean)

## Write final table to working directory
write.table(finalData,"UCIHAR.txt", sep=",")






