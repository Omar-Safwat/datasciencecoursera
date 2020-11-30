---
title: "README"
author: "Omar Safwat"
date: "29/11/2020"
output: md_file
urlcolor: blue
---
# Introduction

The purpose of this project is to demonstrate one's ability in retrieving *"messy"*, raw data from a source, and use R in tidying the data up as well as performing some basic mathematical operations.

The completion of this project is one of the requirements for finishing a data science course on Coursera, part of the Data Science Specialization courses offered by the John Hopkins University.

# Study design
The data represent readings collected from the accelerometers of a Samsung Galaxy S smartphone, and was directly collected in the form of a *.zip* file from:

[https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip). 

A full description of the data can also be found here:

[http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).

To start of, the first assignment was to merge the two data sets **train** and **test**. However, both data sets were scattered and in an unreadable state. 

The readings obtained from the accelerometers and Gyro were stored in the text files ***"X_test.txt"*** and ***"X_train.txt"***, both contatin **561** columns (Each column represents a variable). Variable names or column headers are stored in ***"features.txt"***, each row in the file holds the name of one variable, therefore, summing up to a total of **561** rows.

Data from ***"subject_test.txt"***, and ***"y_test.txt"*** are to be column bound with ***"X_test.txt"***. The same process is then similarly repeated for the **train** data set, thus forming a complete tidy data table for each of the two data sets.

I chose to go with ***Wide*** data frames instead of ***Narrow*** to avoid disecting variable names, as this was reported by Forum Mentors to cause unnecessary confusion.

## Note

Both ***Wide*** and ***Narrow*** data tables are both an **acceptable form** of *tidy* data, as per [Hadley Wickam's paper](https://vita.had.co.nz/papers/tidy-data.pdf).

# R script *"run_analysis.R"*

The script uses *tibbles* and other functions from the *dplyr* package.
```r
library(dplyr)
```

First, each of the scattered "test" data was read independently in the following code section:
```r
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
```
and then assembled here using the *tibble()* function for its special printing properties. The list object "feature" containing column names is matched with the generated *tibble()*, to assign the header names.
```r
##Second assemble the data:
#-------------------------

testData <- tibble(subject_test, y_test, as_tibble(x_test), .name_repair = "minimal")

#assign names to variables of features:
testData <- setNames(testData, c("subject", "activity", features[[1]]))
```
The same process is repeated here for the "train" data.
```r
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
```
Finally, both tables ("testData"" and "trainData") are merged together as was stated in the assignment, using the ***rbind()*** function in R, since both have the same column names and width.
```r
#Merge the training and test dataset
tidy_merged <- rbind(trainData, testData)
```
Next, the *mean* and *standard deviation* variables were to be extracted from this merged data set. Since this is an "open" project, the choice is left to the student, which mean and std variables should be extracted, consequently, I chose to extract all variables with the words *"[Mm]ean"* or *"[Ss]td"*, regardless of their position in the variable's name. This is achieved here using the ***grep()*** function and by utilizing the Regular expressions learned in the 4th week of this course. Furthermore, all values under the variable *"activity"* were to be replaced with more descriptive values, i.e., "WALKING" instead of "1".
```r
#Extract variables with mean or std.
extracted <- grep(".[Mm]ean|.[Ss]td", names(merged) ,value = TRUE)
tidy2 <- select(tidy_merged, subject:activity, all_of(extracted))

#Fill in descriptive names under variable "activity"
my_labels <- read.table("UCI HAR Dataset/activity_labels.txt", colClasses = c("NULL", "character"))
for(i in seq_along(my_labels$V2))
{
  tidy2$activity <- sub(as.character(i), my_labels$V2[i], tidy2$activity)
}
```

Finally, a new tidy data set was created to store the average of each variable, with respect to each subject for every activity type.
```r
#Finding the average for each subject and each activity
tidy_avg <- tibble() 
for(i in seq_along(1:30))
{
  for(j in seq_along(my_labels$V2))
  {
    sub_data <- filter(tidy2, subject == i & activity == my_labels$V2[j])
    avg_tibble <- select(sub_data, -(subject:activity)) %>% lapply(mean) %>% as_tibble()
    tidy_avg <- sub_data[1,c("subject", "activity")] %>% tibble(avg_tibble)%>% 
    rbind(tidy_avg)
  }
}
#Tibble containing average values
tidy_avg <- arrange(avg_data, activity, subject)
```
**Please resort to the code book available on this repository for the discussion over the structure of the tidy set and its variables "features".**
