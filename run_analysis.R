## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average 
##    of each variable for each activity and each subject.

if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}

require("data.table")
require("reshape2")

#The process involves the following steps to create "tidy" TEST & TRAIN sets, and then merge them
#In this implementation, to improve memory processing effeciency, the tidying-up is done independently 
#on the TEST and TRAIN sets before merging them. 
#The process of "tidying" involves (IN NO PARTICULAR ORDER):
#a. Reading the "X_????.txt" file, and adding the propper headers from "features.txt" file
#b. Then based on the legible headers just added, we filter it to only contain "mean" & "std" headers
#c. Now that we have smaller, required data set of X_????, we now read y_????.txt file, and replace the 
#   numeric flags with readable activity names from the "activity_labels.txt" file. This will forms the 
#   new column to be appended to x_???? data.frame
#d. Finally read the "subject_test.txt" file that contains the numeric codes corresponding to human 
#   individuals who participated in the tests. This data fill form the 3rd cilumn of the x_???? data.frame
#e. At this pint, we have tidied up both ehe TEST & TRAIN data, and can now be merged by rows, to form one large dataset
#f. We can now easily apply the labels (Question#4) & creat averages (Question#5)

###################
#  READ ALL DATA  #
###################

# READ features.txt for column names on X_????.txt
features <- read.table("./UCI HAR Dataset/features.txt")[,2]

# READ activity_labels.txt
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

# READ subject codes corresponding to human individuals
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# READ X_test & y_test data.
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

# READ X_train & y_train data.
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

########################################
#  START PROCESSING TEST & TRAIN SETS  #
########################################

# Assign legible headers to subject_???? data column
names(subject_test) = "subject"
names(subject_train) = "subject"

# Assign legible headers to X_???? data columns 
names(X_test) = features
names(X_train) = features

# Extract only the measurements on the mean and standard deviation for each measurement.
# Using grepl, we get a logical vector of required column headings
required_features <- grepl("mean|std", features)

# Extract only the measurements on the "mean" and "standard deviation" for each measurement.
X_test = X_test[,required_features]
X_train = X_train[,required_features]

# mutate y_???? to append legible activity labels from activity_labels.txt, corresponding to numeric Activity_ID
# TEST SET #
y_test[,2] = activity_labels[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity_Label")
# TRAIN SET #
y_train[,2] = activity_labels[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity_Label")

# Bind data
test_data <- cbind(subject_test, y_test, X_test)
train_data <- cbind(subject_train, y_train, X_train)

###############################
#  COMBINE TEST & TRAIN SETS  #
###############################

# Merge test and train data
data = rbind(test_data, train_data)

##############################################
#  CREATE TIDY DATASET & CALCULATE AVERAGES  #
##############################################

id_labels   = c("subject", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(data), id_labels)
melt_data      = melt(data, id = id_labels, measure.vars = data_labels)

# Apply mean function to dataset using dcast function
tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)

write.table(tidy_data, file = "./tidy_data.txt", row.name=FALSE)
