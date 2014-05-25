## import data
getwd()
setwd("./UCI HAR Dataset/")
X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")
activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

## 1.merges the training and the test sets to create one data set.
TrainSet <- cbind(X_train, y_train, subject_train)
TestSet <- cbind(X_test, y_test, subject_test)
names(TrainSet)
names(TestSet)
names(TrainSet)[562] <- "y"; names(TestSet)[562] <- "y"
names(TrainSet)[563] <- "subject"; names(TestSet)[563] <- "subject"

MergeData <- merge(TrainSet, TestSet, all=TRUE)


## 2.extracts only the measurements on the mean and standard 
## deviation for each measurement. 
head(features)
names(MergeData)
meanIndex <- which(grepl("mean", features$V2, perl=TRUE) == TRUE)
meanIndex
# exclude "meanFreq()"
Index_exclude <- which(grepl("Freq", features$V2, perl=TRUE) == TRUE)
Index_exclude
meanIndex <- meanIndex[-which(meanIndex %in% Index_exclude)]
meanIndex
stdIndex <- which(grepl("std", features$V2, perl=TRUE) == TRUE)
stdIndex


ExtractData <- MergeData[, c(meanIndex, stdIndex, 562:563)]
head(ExtractData)
dim(ExtractData)


## 3.uses descriptive activity names to name the activities in
## the data set
head(features)
# refine feature names: remove punctuation and lower case
features$V2 <- gsub("[[:punct:]]", "", features$V2) 
features$V2 <- tolower(features$V2)

names(ExtractData)
features_index <- as.numeric(gsub("V", "", names(ExtractData[,1:66])))
features_index
write.table(features_index, "features_index.txt")
names(ExtractData)[1:66] <- features$V2[features_index]
names(ExtractData)[67] <- "activity"
head(ExtractData)


## 4.appropriately labels the data set with descriptive activity
## names.
# a function to map a label into its activity name 
activity_name_func <- function(label){
    activity <- as.character(activity_labels$V2)
    name <- ifelse(label == 1, activity[1],
                   ifelse(label == 2, activity[2],
                          ifelse(label == 3, activity[3],
                                 ifelse(label == 4, activity[4],
                                        ifelse(label == 5, activity[5], activity[6])))))
    return(name)
  }

ExtractData$activity <- sapply(ExtractData$activity, activity_name_func)
ExtractData$activity <- sapply(ExtractData$activity, as.factor)
names(ExtractData)
head(ExtractData)
dim(ExtractData)

setwd("/Users/wei/Desktop/SUMMER/cleaningdata")
write.table(ExtractData, "ExtractData.txt")

## 5.creates a second, independent tidy data set with the average of 
## each variable for each activity and each subject. 
library(reshape2)
names(ExtractData)
MeltData <- melt(ExtractData, id=c("subject", "activity"),
                 measure.vars = names(ExtractData)[1:66]) 
head(ExtractData)
tail(ExtractData)

TidyData <- dcast(MeltData, subject + activity ~ variable, mean)
head(TidyData)
dim(TidyData)

setwd("/Users/wei/Desktop/SUMMER/cleaningdata")
write.table(TidyData, "TidyData.txt")










