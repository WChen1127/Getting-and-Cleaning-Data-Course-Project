##Purpose
The purpose of this project is to demonstrate my ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 


##Object
I create one R script called run_analysis.R that does the following. 
* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement. 
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive activity names. 
* Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


##Data
The Data are available here: 
            
      https://class.coursera.org/exdata-002/human_grading/view/courses/972082/assessments/4/submissions
      
The Data were gained from the experiments carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 


##Process

### import data
Download the data file from the course website to the local computer 
and read ".txt" data using     read.table()
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

### 1.merges the training and the test sets to create one data set.

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
