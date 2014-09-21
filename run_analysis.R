## This file does the following
##
##	1 - Merges the training and the test sets
##	2 - Extracts only the measurements on the mean and standard deviation
##	3 - Uses descriptive activity names to name the activities in the data set
##	4 - Appropriately labels the data set with descriptive variable names
##	5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
##
##


## 1 - Merges the training and the test sets

training = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
training[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
training[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

testing = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
testing[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
testing[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

MergedData = rbind(training, testing)



## 2 - Extracts only the measurements on the mean and standard deviation

Columns <- grep(".*Mean.*|.*Std.*", features[,2])
features <- features[Columns,]
Columns <- c(Columns, 562, 563)
FilteredData <- MergedData[,Columns]

## 3 - Uses descriptive activity names to name the activities in the data set

colnames(FilteredData) <- c(features$V2, "Activity", "Subject")
colnames(FilteredData) <- tolower(colnames(FilteredData))

## 4 - Appropriately labels the data set with descriptive variable names

currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
	FilteredData$activity <- gsub(currentActivity, currentActivityLabel, FilteredData$activity)
	currentActivity <- currentActivity + 1
}

FilteredData$activity <- as.factor(FilteredData$activity)
FilteredData$subject <- as.factor(FilteredData$subject)

## 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

tidy = aggregate(FilteredData, by=list(activity = FilteredData$activity, subject=FilteredData$subject), mean)
tidy[,90] = NULL
tidy[,89] = NULL
write.table(tidy, "tidy.txt", sep="\t")
