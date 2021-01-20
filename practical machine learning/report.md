---
title: "Machine Learning project"
author: "Omar Safwat"
date: "1/20/2021"
output: html_document
---
```{r echo = FALSE, message=FALSE}
library(caret)
library(parallel)
library(doParallel)
```

# Executive Summary

A classification prediction model is built on the data from the ["Qualitative Activity Recognition of Weight Lifting Exercises"](http://groupware.les.inf.puc-rio.br/har) study, to predict exercise quality for 20 unknown observations.

* A 100 of the original 160 features were unusable, due to the high rate of missing values.
* 52 features of the remaining 60 were used to predict the value of the class response ```classe```.
* A random forest model with 99.2% test accuracy at ```mtry = 27``` was finalized, thus correctly classifying 20 out of 20 observations. 

# Exploratory Analysis/Features Selection

The ratio of number of observations to number of features in the loaded dataset is approximately ```122```, a ratio this large will aid in the process of model selection. 

```{r echo=FALSE}
#Loading the data
training = read.csv("pml-training.csv", na.strings = c("NA", " ", ""))
print("dimensions of the loaded dataset")
dim(training)
```

Almost 98% of the observations are missing in some of the features as shown below, which makes them unusable, and will, therefore, be removed, leaving only 60 features remaining.

```{r echo = FALSE}
#detect NAs. Columns with NAs
NA.col = as.matrix(apply(training, 2, function(x){sum(1*is.na(x))}))
training = training[,names(NA.col[NA.col[,1] == 0,])]
c("Count of columns with missing values" = length(which(NA.col[,1] != 0)), "NAs per column" = median(NA.col[,1]))
```

The first and second column contain the index number of the dataset and the name of the test participant, respectively. It can be seen from the barplot below, that each participant performed each of the 5 classed activities. The ```classe``` variable is therefore, not user name specific. Therefore, both features will be removed, as they don't contribute to the generality of the model. 

```{r echo=FALSE}
training$classe = as.factor(training$classe)
#Create Barplot
counts = table(training$classe, training$user_name)
barplot(counts, xlab = "User name", ylab = "Frequency", main = "Distibution of Classes over users", col = c("grey", "skyblue3", "yellow3", "green3", "purple2"), beside = TRUE, legend = paste("Class" ,rownames(counts)))
#Remove time stamps and unused columns
training = training[,-c(1:7)]
```

Similarly, the date and time columns, i.e., columns 3 through 7, are also removed, as they don't affect the measurements obtained from the sensors.
The remaining 52 variables have no missing values and require no imputations.

# Model building

The loaded data is split; 70% to train the model and the remaining 30% for validation.
Given the large number of variables still remaining, an approach adopting decision trees would be more robust. The Random Forest algorithm will automatically select a useful subset of the features available, while minimizing the variance of the model.
In order to obtain results within manageable time, a 5-fold cross validation is used in model building, in lieu of boosting, along with parallel computing.

```{r echo=FALSE, cache=TRUE}
#Split training to build and validation sets
set.seed(1)
inBuild = createDataPartition(training$classe, p = 0.7, list = FALSE)
build = training[inBuild,]
validation = training[-inBuild,]
#Configuring Parallel processing
cluster = makeCluster(detectCores() - 1)
registerDoParallel(cluster)
# Fit models using rf and boosting Will use decision trees because it automatically picks the most suitable features
intervalStart = Sys.time()
modControl = trainControl(method = "cv", number = 5, allowParallel = TRUE, classProbs = TRUE)
rf.mdl = train(classe ~ ., data = build, method = "rf", trControl = modControl)
#De-register parallel processing
stopCluster(cluster)
registerDoSEQ()
intervalEnd = Sys.time()
paste("Model building took: ",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))
print("Accruacy on training set")
rf.mdl$finalModel
```

The Out of sample error is an estimate of the true test error. The final model yielded an 0.76% out of sample error, thus giving a probability of 85.8% that the model will accurately identify 20 out of the 20 unknown observations.

The model achieved highest accuracy using 27 features as illustrated below.

```{r echo=FALSE}
plot(rf.mdl)
```

The classification error of the final model remained steady right around a 100 bagged trees as shown below, after which, no considerable decrease in error is seen.

```{r}
plot(rf.mdl$finalModel, main = "Error by Fold")
```

The relative importance of each variable for each variable is illustrated by the next plot, showing that the most important 7 sensor measurements for the classification model are ```roll_belt```, ```pitch_forearm```, ```yaw_belt```, ```pitch_belt```, ```magent_dumbell_z```, ```magnet_dumbell_y```, ```roll_forearm```.

```{r}
plot(varImp(rf.mdl), main = "Variables Importance Plot", top = 10)
```

# Results

```{r}
#Accuracy on validation set
print("Accruacy on validation set")
confusionMatrix(validation$classe, predict(rf.mdl, newdata = validation[,-53]))
```

The model was able to accurately classify 99.2% of the observations in the validation set.
Since this model will later be used to predict 20 unknown observations, it is estimated that the model will accurately identify the 20 cases with a $0.992^{20}$%, 85% probability.

# Appendix

```{r eval=FALSE}
library(caret)
library(parallel)
library(doParallel)
#Loading the data
set.seed(1)
training = read.csv("pml-training.csv", na.strings = c("NA", " ", ""))
testing = read.csv("pml-testing.csv", na.strings = c("NA", " ", ""))
```

```{r eval=FALSE}
#Remove time stamps and unused columns
training = training[,-c(1:7)]
training$classe = as.factor(training$classe)
#detect NAs. Columns with NAs, some have 19216 NA values which is 98% of the data, get rid of them
NA.col = as.matrix(apply(training, 2, function(x){sum(1*is.na(x))}))
training = training[,names(NA.col[NA.col[,1] == 0,])]
#Split training to build and validation sets
inBuild = createDataPartition(training$classe, p = 0.7, list = FALSE)
build = training[inBuild,]
validation = training[-inBuild,]
```

```{r eval=FALSE}
#Configuring Parallel processing
cluster = makeCluster(detectCores() - 1)
registerDoParallel(cluster)
# Fit models using rf and boosting Will use decision trees because it automatically picks the most suitable features
modControl = trainControl(method = "cv", number = 5, allowParallel = TRUE, classProbs = TRUE)
rf.mdl = train(classe ~ ., data = build, method = "rf", trControl = modControl)
#De-register parallel processing
stopCluster(cluster)
registerDoSEQ()
#Accuracy on validation set
confusionMatrix(validation$classe, predict(rf.mdl, newdata = validation[,-53]))
```

```{r}
sessionInfo()
```
