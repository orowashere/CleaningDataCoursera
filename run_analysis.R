# Data Courtesy of: 

# Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and 
# Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones 
# using a Multiclass Hardware-Friendly Support Vector Machine. 
# International Workshop of Ambient Assisted Living (IWAAL 2012). 
# Vitoria-Gasteiz, Spain. Dec 2012

# Goals ----
# 1. Extracts only the measurements on the mean and standard deviation for each measurement.
# 2. Uses descriptive activity names to name the activities in the data set
# 3. Appropriately labels the data set with descriptive variable names.
# 4. From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.

# libs --- 

library(dplyr)
library(memisc)

# Merge Train and Test ----

## Prepare Test Data ----

test <- readLines("UCI HAR Dataset/test/X_test.txt")
test <- lapply(
  test, 
  function(x) unlist(strsplit(x, " "))
)
test <- lapply(test, function(x) as.numeric(x[x != ""]))
test <- do.call(rbind, test)
test <- as.data.frame(test)
nms <- read.delim("UCI HAR Dataset/features.txt", sep = " ", header = FALSE)$V2
names(test) <- nms

test$Type <- "Test"

## Prepare Train Data ---

train <- readLines("UCI HAR Dataset/train/X_train.txt")
train <- lapply(
  train, 
  function(x) unlist(strsplit(x, " "))
)
train <- lapply(train, function(x) as.numeric(x[x != ""]))
train <- do.call(rbind, train)
train <- as.data.frame(train)
names(train) <- nms

train$Type <- "Train"

## Combine ----

tidy <- rbind(test, train)
tidy <- tidy[grep("std[(]|mean[(]|Type", names(tidy))]

## Add activities ----

tidy$activities <- c(
  readLines("UCI HAR Dataset/test/y_test.txt"),
  readLines("UCI HAR Dataset/train/y_train.txt")
)

### Label activities ----

activity_labs <- read.delim("UCI HAR Dataset/activity_labels.txt", sep = " ", header = FALSE)
tidy$activities <- factor(tidy$activities, labels = as.character(activity_labs$V2))

## Add subjects ----
tidy$subject <- as.numeric(c(
  readLines("UCI HAR Dataset/test/subject_test.txt"),
  readLines("UCI HAR Dataset/train/subject_train.txt")
))

# Summary Data by Subject / Activity

summary_df <- select(tidy, -Type) %>% 
  group_by(activities, subject) %>% 
  summarise_all(.funs = function(x) mean(x))
