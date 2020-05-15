# JHU Getting and Cleaning Data Course Project
# Akshay Bapte
# May 9, 2020

# runAnalysis.r will:

# Input UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Listing all the files in the folder
setwd("~/Coursera files R/Cleaning data/Week4/getdata_projectfiles_UCI HAR Dataset");
list.files("./");

pathdata <- file.path("./", "UCI HAR Dataset");
files <- list.files(pathdata, recursive = T);
files;

#Import train data from the files and merge them
xtrain <- read.table(file.path(pathdata, "train", "X_train.txt"), header = FALSE);
ytrain <- read.table(file.path(pathdata, "train", "Y_train.txt"), header = FALSE);
subject_train <- read.table(file.path(pathdata, "train", "subject_train.txt"), header = FALSE);

#Import test data from the files and merge them
xtest <- read.table(file.path(pathdata, "test", "X_test.txt"), header = FALSE);
ytest <- read.table(file.path(pathdata, "test", "Y_test.txt"), header = FALSE);
subject_test <- read.table(file.path(pathdata, "test", "subject_test.txt"), header = FALSE);

#Import activity_labels and features file
activity <- read.table(file.path(pathdata, "activity_labels.txt"), header = FALSE);
features <- read.table(file.path(pathdata, "features.txt" ), header = FALSE);

#Name the columns
colnames(xtrain) <- features[,2];
colnames(ytrain) <- "activityId";
colnames(subject_train) <- "subjectId";
colnames(xtest) <- features[,2];
colnames(ytest) <- "activityId";
colnames(subject_test) <- "subjectId";
colnames(activity) <- c("activityId", "activityType");

#Merge all test and train data
merge_train <- cbind(ytrain, subject_train, xtrain);
merge_test <- cbind(ytest, subject_test, xtest);
allinone <- rbind(merge_train, merge_test);

# Create a vector that indentifies the ID, mean & stddev columns as TRUE
colNames <- colnames(allinone);
mean_std <- (grepl("activityId", colNames) | grepl("subjectId", colNames) | grepl("mean..", colNames) | grepl("std..", colNames));

#Update allinone dataset based on previously identified columns
set_mean_std <- allinone[, mean_std == TRUE];

# Add in descriptive activity names to set_mean_std & update columns vector
merged_dataset <- merge(set_mean_std, activity, by = "activityId");
merged_dataset$activityId <- activity[,2][match(merged_dataset$activityId, activity[,1])] ;
columns <- colnames(merged_dataset);

# Tidy column names
for (i in 1:length(columns)) 
   {
         columns[i] <- gsub("\\()","",columns[i])
         columns[i] <- gsub("-std$","StdDev",columns[i])
         columns[i] <- gsub("-mean","Mean",columns[i])
         columns[i] <- gsub("^(t)","time",columns[i])
         columns[i] <- gsub("^(f)","freq",columns[i])
         columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
         columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
         columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
         columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
         columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
         columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
         columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
     };
 
 
 # Update MergedDataSet with new descriptive column names   
colnames(merged_dataset) <- columns;

# Remove activityType column
merged_dataset <- merged_dataset[,names(merged_dataset) != 'activityType'];

# Averaging each activity and each subject as Tidy Data
tidyData <- aggregate(merged_dataset[,names(merged_dataset) != c('activityId', 'subjectId')],
                      by = list(activityId=merged_dataset[,1],
                                subjectId=merged_dataset[,2]), mean);
# Export tidyData set 
write.table(tidyData, './FinalTidyData.txt',row.names=FALSE, sep='\t');

#View tidyData
View(tidyData)
