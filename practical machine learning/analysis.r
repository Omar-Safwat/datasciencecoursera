library(caret)
library(parallel)
library(doParallel)
#Loading the data
set.seed(1)
training = read.csv("pml-training.csv", na.strings = c("NA", " ", ""))
testing = read.csv("pml-testing.csv", na.strings = c("NA", " ", ""))

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