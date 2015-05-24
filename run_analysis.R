##Read the training and the test sets to create one data set.
##I've added the feature column names before merging the 2 sets into "totalset"

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("id", "name"))
testset <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$name)
trainset <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$name)

totalset <- rbind(testset, trainset)

##Extracts only the measurements on the mean and standard deviation for each measurement. 
library(plyr)
library(dplyr)
meanstdset <- select(totalset, contains("mean"), contains("std"))

##Uses descriptive activity names to name the activities in the data set
##Appropriately labels the data set with descriptive variable names.

activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("id", "name"))

subjectest <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "Subject")
subjectrain <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "Subject")
subjectotal <- rbind(subjectest,subjectrain) ##combine the test/train subjects

ytest <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "Activity")
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "Activity")
ytotal <- rbind(ytest, ytrain)  ##combine the test/train activities

ytotal <- sapply(ytotal[,1], as.numeric) ##change the "Activity" column as numerical before applying the change

library(qdap)   #install.packages("qdap") to use the "mgsub" function to allow multiple gsubs

ytotal <- as.data.frame(mgsub(activitylabels$id, activitylabels$name, ytotal))
colnames(ytotal) <- "Activity"

meanstdset <- cbind(c(subjectotal, ytotal), meanstdset) ##add the subject and activity columns

##Modify the column names for a more descriptive format
##I tried "mgsub" as done earlier but did not get the needed format

names(meanstdset) <- gsub("^t", "time", names(meanstdset))
names(meanstdset) <- gsub("^f", "frequency", names(meanstdset))
names(meanstdset) <- gsub("Acc", "Accelerometer", names(meanstdset))
names(meanstdset) <- gsub("Gyro", "Gyroscope", names(meanstdset))
names(meanstdset) <- gsub("Mag", "Magnitude", names(meanstdset))
names(meanstdset) <- gsub("BodyBody", "Body", names(meanstdset))

##From the data set in step 4, creates a second, independent tidy data set with the 
##average of each variable for each activity and each subject.
##The use of the "aggregate" function will help to split the data into subsets, 
##compute summary statistics for each, and return the result in a convenient form.
##Order the tidyset and write the table into "tidydata.txt"

tidyset <- aggregate(. ~Subject + Activity, meanstdset, mean)
tidyset <- tidyset[order(tidyset$Subject, tidyset$Activity), ]
write.table(tidyset, file = "tidydata.txt", row.name=FALSE)
