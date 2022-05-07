
# Install R Package 
# install.packages("dplyr")

# Loading required package
library(dplyr)

# Set Working Directry 
cur_dir  <-  "C:/DataAnalysis/Data_Cleaning_R"
setwd(cur_dir)

# Download the Dataset
file_name <- "Getting_and_Cleaning_Data.zip"

# Checking if zip file already exists 
if(!file.exists(file_name)){
	file_url ="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	download.file(file_url, file_name, method ="curl")
}

# Checking if folder already exists 
if(!file.exists("UCI HAR Dataset")){
	unzip(file_name)
}

# Assiging of all Dataframes
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("functions-number","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activity_code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity_code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity_code")


# Step 1: Merges the training and the test sets to create one data set. 
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data_Set <- cbind(Subject, Y, X)

# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
# Select columns of the dataframe based upon certains conditions
Extracted_Data_Set <- select(Merged_Data_Set, subject, activity_code, contains("mean"), contains("std"))

# Step 3: Uses descriptive activity names to name the activities in the data set.
Extracted_Data_Set$activity_code <- activities[Extracted_Data_Set$activity_code,2]


# Step 4: Appropriately labels the data set with descriptive variable names.
names(Extracted_Data_Set)[2] = "activity"
names(Extracted_Data_Set)<-gsub("Acc", "Accelerometer", names(Extracted_Data_Set))
names(Extracted_Data_Set)<-gsub("Gyro", "Gyroscope", names(Extracted_Data_Set))
names(Extracted_Data_Set)<-gsub("BodyBody", "Body", names(Extracted_Data_Set))
names(Extracted_Data_Set)<-gsub("Mag", "Magnitude", names(Extracted_Data_Set))
names(Extracted_Data_Set)<-gsub("^t", "Time", names(Extracted_Data_Set))
names(Extracted_Data_Set)<-gsub("^f", "Frequency", names(Extracted_Data_Set))
names(Extracted_Data_Set)<-gsub("tBody", "TimeBody", names(Extracted_Data_Set))
names(Extracted_Data_Set)<-gsub("-mean()", "Mean", names(Extracted_Data_Set), ignore.case = TRUE)
names(Extracted_Data_Set)<-gsub("-std()", "STD", names(Extracted_Data_Set), ignore.case = TRUE)
names(Extracted_Data_Set)<-gsub("-freq()", "Frequency", names(Extracted_Data_Set), ignore.case = TRUE)
names(Extracted_Data_Set)<-gsub("angle", "Angle", names(Extracted_Data_Set))
names(Extracted_Data_Set)<-gsub("gravity", "Gravity", names(Extracted_Data_Set))


# Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
independent_tidy_data_set <- Extracted_Data_Set %>% 
					group_by(subject, activity) %>%
					summarise_all(funs(mean))

write.table(independent_tidy_data_set, "Independent_tidy_data_set.txt", row.name=FALSE)
