library(data.table)
library(dplyr)

###First, get everything we need set up 

#Set the working directory
setwd("https://github.com/SarahEthakoti/Getting-Cleaning-Data-Course-Project/getdata_projectfiles_UCI HAR Dataset")

#set training data sets
sTrain <- read.table("train/subject_train.txt", header = FALSE)
yTrain <- read.table("train/y_train.txt", header = FALSE)
xTrain <- read.table("train/X_train.txt", header = FALSE)

#set test data sets
sTest <- read.table("test/subject_test.txt", header = FALSE)
yTest <- read.table("test/y_test.txt", header = FALSE)
xTest <- read.table("test/x_test.txt", header = FALSE)

#Listing of features
featuresList <- read.table("features.txt")

## Part 1. Merge the training and test sets to create one data set

#Now merge the test and training sets
sCombine <- rbind(sTrain, sTest)
yCombine <- rbind(yTrain, yTest)
xCombine <- rbind(xTrain, xTest)

#Let's name the columns now
colnames(yCombine) <- "Activity"
colnames(sCombine) <- "Subject"
colnames(xCombine) <- t(featuresList[2])

AllData <- as.data.frame(cbind(xCombine, yCombine, sCombine))

## Part 2. Extract only the measurements on the mean and standard deviation for each measurement

#Now find the columns which have the standard deviation or the mean 
ColumnsWeNeed <- as.data.frame(grep(".*Mean.*|.*Std.*", names(AllData), ignore.case = TRUE))
reqColumns <- list(ColumnsWeNeed, 562,563)
filteredData <- AllData[,unlist(reqColumns)]

#How many rows and columns do we have here?
dim(filteredData)

## Part 3. Use descriptive activity names to name the activities in the data set

#Let's change the filtered columns to character type
filteredData$Activity <- as.character(filteredData$Activity)
for (i in 1:6){filteredData$Activity[filteredData$Activity == i] <- as.character(read.table("activity_labels.txt")[i,2])}
filteredData$Activity <- as.factor(filteredData$Activity)

## Part 4. Appropriately label the data set with descriptive variable names

#replace "t" with "Time"
names(filteredData) <- gsub("^t", "Time", names(filteredData))

#replace "f" with "Frequency"
names(filteredData) <- gsub("^f", "Frequency", names(filteredData))

## Part 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject

Tidy_Dataset <- aggregate(.~Activity+Subject, filteredData, mean)
Tidy_Dataset_Final <- Tidy_Dataset[order(Tidy_Dataset$Subject,Tidy_Dataset$Activity),]

write.table(Tidy_Dataset_Final, file = "TidyData.txt", row.names = FALSE)


