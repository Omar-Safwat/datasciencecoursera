library(dplyr)
## Reading and assembling the "test" data.

##First read each of the data independently:
#------------------------------------------

##1)Reading the activity labels
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
##2)Reading the 561 features of test data
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
x_test <- lapply(x_test, function(x){as.numeric(x)})
##3)Reading the "Subject" column
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
##4)Reading the names of the feature, i.e.,(The variable names)
features <- read.table("UCI HAR Dataset/features.txt",colClasses = c("NULL", "character"))

##Second assemble the data:
#-------------------------

testData <- tibble(subject_test, y_test, as_tibble(x_test), .name_repair = "minimal")

#assign names to variables of features:
testData <- setNames(testData, c("subject", "activity", features[[1]]))

## Reading and assembling the "training" data.

##First read each of the data independently:
#------------------------------------------

##1)Reading the activity labels
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
##2)Reading the 561 features of test data
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
x_train <- lapply(x_train, function(x){as.numeric(x)})
##3)Reading the "Subject" column
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
##4)Reading the names of  features, i.e.,(variable names)
features <- read.table("UCI HAR Dataset/features.txt",colClasses = c("NULL", "character"))

##Second assemble the data:
#-------------------------

trainData <- tibble(subject_train, y_train, as_tibble(x_train), .name_repair = "minimal")

#assign names to variables of features:
trainData <- setNames(trainData, c("subject", "activity", features[[1]]))

#Merge the training and test dataset
merged <- rbind(trainData, testData)

#Extract variables with mean or std.
extracted <- grep(".[Mm]ean|.[Ss]td", names(merged) ,value = TRUE)
merged <- select(merged, subject:activity, all_of(extracted))

#Fill in descriptive names under variable "activity"
my_labels <- read.table("UCI HAR Dataset/activity_labels.txt", colClasses = c("NULL", "character"))
for(i in seq_along(my_labels$V2))
{
  merged$activity <- sub(as.character(i), my_labels$V2[i], merged$activity)
}

#Finding the average for each subject and each activity
avg_data <- tibble() 
for(i in seq_along(1:30))
{
  for(j in seq_along(my_labels$V2))
  {
    sub_data <- filter(merged, subject == i & activity == my_labels$V2[j])
    avg_tibble <- select(sub_data, -(subject:activity)) %>% lapply(mean) %>% as_tibble()
    avg_data <- sub_data[1,c("subject", "activity")] %>% tibble(avg_tibble)%>% 
    rbind(avg_data)
  }
}
#Tibble containing average values
avg_data <- arrange(avg_data, activity, subject)