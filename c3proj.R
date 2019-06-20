#load dplyr
library(dplyr)

#GET THE DATA

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}
dataPath <- "UCI HAR Dataset"
if (!file.exists("UCI HAR Dataset")) {
  unzip(zipFile)
}

#READ THE DATA

trainSubject <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainActLabel <- read.table(file.path(dataPath, "train", "y_train.txt"))

testSubject <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActLabel <- read.table(file.path(dataPath, "test", "y_test.txt"))

features <- read.table(file.path(dataPath, "features.txt"),as.is = TRUE)
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

#1 Merge the training and the test sets to create one data set.
mergedData <- rbind(
  cbind(trainSubject, trainValues, trainActLabel),
  cbind(testSubject, testValues, testActLabel)
)
rm(trainSubject,trainValues,trainActLabel,testSubject,testValues,testActLabel)

colnames(mergedData) <- c("subject", features[, 2], "activity")

#2 Extract only the measurements on the mean and standard deviation for each measurement.
colKeep=grepl("subject|activity|mean|std", colnames(mergedData))
mergedData <- mergedData[, colKeep]

#3 Assign descriptive activity names to name the activities in the data set
mergedData$activity <- factor(mergedData$activity, levels = activities[, 1], labels = activities[, 2])

#4 Appropriately label the data set with descriptive variable names
names(mergedData) <- gsub("[\\(\\)-]", "", names(mergedData))

names(mergedData)<-gsub("Acc", "Accelerometer", names(mergedData))
names(mergedData)<-gsub("Gyro", "Gyroscope", names(mergedData))
names(mergedData)<-gsub("Mag", "Magnitude", names(mergedData))
names(mergedData)<-gsub("^t", "Time", names(mergedData))
names(mergedData)<-gsub("^f", "Frequency", names(mergedData))
names(mergedData)<-gsub("tBody", "TimeBody", names(mergedData))
names(mergedData)<-gsub("-mean()", "Mean", names(mergedData), ignore.case = TRUE)
names(mergedData)<-gsub("-std()", "STD", names(mergedData), ignore.case = TRUE)
names(mergedData)<-gsub("-freq()", "Frequency", names(mergedData), ignore.case = TRUE)
names(mergedData)<-gsub("angle", "Angle", names(mergedData))
names(mergedData)<-gsub("gravity", "Gravity", names(mergedData))

#5 Create a second, independent tidy data set with the average of each variable for each activity and each subject.
mergedDataMeans <- mergedData %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

write.table(mergedDataMeans, "tidy_data.txt", row.names = FALSE )

