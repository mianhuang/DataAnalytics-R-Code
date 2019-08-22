# clear workspace
rm(list = ls())

# load Libraries: p_load can install,load, and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
} 

pacman::p_load(readr, caret, ggplot2, tidyverse, tidyr, dplyr, lubridate, plotly, C50, tibbletime, doParallel)

# find how many cores are on your machine
detectCores() # Result = Typically 4 to 6

# create Cluster with desired number of cores. Don't use them all! Your computer is running other processes. 
cl <- makeCluster(4)

# register Cluster
registerDoParallel(cl)

# confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2 

# import training & validation datasets
trainingData_orig <- read_csv("trainingData.csv")
validData_orig <- read_csv("validationData.csv")

# make working copy of datasets 
trainingData <- trainingData_orig
validData <- validData_orig

# remove repeated rows in datasets
trainingData <-distinct(trainingData)
validData <- distinct(validData)

# removing variables that are not needed: USERID, PHONEID, TIMESTAMP
trainingData[527:529] <- NULL
validData[527:529]<-NULL

# transform some variables to factor/numeric/datetime
trainingData[,523:526] <- lapply(trainingData[,523:526], as.factor)
trainingData[,521:522] <- lapply(trainingData[,521:522], as.numeric)
#trainingData$TIMESTAMP <- as_datetime(trainingData$TIMESTAMP, origin = "1970-01-01", tz="UTC")

# change value of WAPS= 100 (out of range value) to WAPS=-110
trainingData[,1:520] <- sapply(trainingData[,1:520],function(x) ifelse(x==100,-110,x))
summary(trainingData[1:10])

# identify and removing WAPS with zero variance (remove WAPS that has no detection)
nzv_train<-nearZeroVar(trainingData[1:520], saveMetrics=TRUE)
trainingData<-trainingData[-which(nzv_train$zeroVar==TRUE )]

# remove rows with all out of range WAP value
trainingData <- trainingData %>% 
  filter(apply(trainingData[1:312], 1, function(x)length(unique(x)))>1)

trainingData$LATITUDE<-NULL
trainingData$LONGITUDE<-NULL

# initial examination of the data.
barplot(table(trainingData$FLOOR[trainingData$BUILDINGID==2]))
barplot(table(trainingData$BUILDINGID))
hist(trainingData$LONGITUDE)
hist(trainingData$LATITUDE)

# subsetting data by building
bldg0 <- subset(trainingData,trainingData$BUILDINGID==0)
bldg1 <- subset(trainingData,trainingData$BUILDINGID==1)
bldg2 <- subset(trainingData,trainingData$BUILDINGID==2)

# add LOCATION column by merging FLOOR, BUILDINGID, SPACEID, & RELATIVEPOSITION
bldg0_loc<- unite(bldg0, "LOCATION", c(FLOOR, BUILDINGID, SPACEID, RELATIVEPOSITION))
bldg0_loc$LOCATION <- as.factor(bldg0_loc$LOCATION)
bldg1_loc<- unite(bldg1, "LOCATION", c(FLOOR, BUILDINGID, SPACEID, RELATIVEPOSITION))
bldg1_loc$LOCATION <- as.factor(bldg1_loc$LOCATION)
bldg2_loc<- unite(bldg2, "LOCATION", c(FLOOR, BUILDINGID, SPACEID, RELATIVEPOSITION))
bldg2_loc$LOCATION <- as.factor(bldg2_loc$LOCATION)

# set seed
set.seed(1)

# set up 10 fold cross validation    
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

# create training & test set for each building
inTraining0 <- createDataPartition(bldg0_loc$LOCATION, p = .75, list = FALSE)
training0 <- bldg0_loc[inTraining0,]
testing0 <- bldg0_loc[-inTraining0,]

inTraining1 <- createDataPartition(bldg1_loc$LOCATION, p = .75, list = FALSE)
training1 <- bldg1_loc[inTraining1,]
testing1 <- bldg1_loc[-inTraining1,]

inTraining2 <- createDataPartition(bldg2_loc$LOCATION, p = .75, list = FALSE)
training2 <- bldg2_loc[inTraining2,]
testing2 <- bldg2_loc[-inTraining2,]

# training models
# KNN
system.time(knnFit0 <- train(LOCATION~., data = training0, method = "knn", trControl=fitControl))
system.time(knnFit1 <- train(LOCATION~., data = training1, method = "knn", trControl=fitControl))
system.time(knnFit2 <- train(LOCATION~., data = training2, method = "knn", trControl=fitControl))

b0predict_knn<-predict(knnFit0, testing0)
b0_ConfusionMatrix_knn<-confusionMatrix(b0predict_knn, testing0$LOCATION)
b0_ConfusionMatrix_knn
b1predict_knn<-predict(knnFit1, testing1)
b1_ConfusionMatrix_knn<-confusionMatrix(b1predict_knn, testing1$LOCATION)
b1_ConfusionMatrix_knn
b2predict_knn<-predict(knnFit2, testing2)
b2_ConfusionMatrix_knn<-confusionMatrix(b2predict_knn, testing2$LOCATION)
b2_ConfusionMatrix_knn

# C5.0
system.time(C50Fit0 <- train(LOCATION~., data = training0, method = "C5.0", trControl=fitControl))
system.time(C50Fit1 <- train(LOCATION~., data = training1, method = "C5.0", trControl=fitControl))
system.time(C50Fit2 <- train(LOCATION~., data = training2, method = "C5.0", trControl=fitControl))

b0predict_C50<-predict(C50Fit0, testing0)
b0_ConfusionMatrix_C50<-confusionMatrix(b0predict_C50, testing0$LOCATION)
b0_ConfusionMatrix_C50
b1predict_C50<-predict(C50Fit1, testing1)
b1_ConfusionMatrix_C50<-confusionMatrix(b1predict_C50, testing1$LOCATION)
b1_ConfusionMatrix_C50
b2predict_C50<-predict(C50Fit2, testing2)
b2_ConfusionMatrix_C50<-confusionMatrix(b2predict_C50, testing2$LOCATION)
b2_ConfusionMatrix_C50

# Decision Tree
system.time(rpartFit0 <- train(LOCATION~., data = training0, method = "rpart", tuneLength = 200, trControl=fitControl))
system.time(rpartFit1 <- train(LOCATION~., data = training1, method = "rpart", tuneLength = 200, trControl=fitControl))
system.time(rpartFit2 <- train(LOCATION~., data = training2, method = "rpart", tuneLength = 200, trControl=fitControl))

b0predict_rpart<-predict(rpartFit0, testing0)
b0_ConfusionMatrix_rpart<-confusionMatrix(b0predict_rpart, testing0$LOCATION)
b0_ConfusionMatrix_rpart
b1predict_rpart<-predict(rpartFit1, testing1)
b1_ConfusionMatrix_rpart<-confusionMatrix(b1predict_rpart, testing1$LOCATION)
b1_ConfusionMatrix_rpart
b2predict_rpart<-predict(rpartFit2, testing2)
b2_ConfusionMatrix_rpart<-confusionMatrix(b2predict_rpart, testing2$LOCATION)
b2_ConfusionMatrix_rpart

# Random Forest
system.time(RFfit0 <- train(LOCATION~., data=training0, method="rf", trcontrol=fitControl, tuneLength=5))
system.time(RFfit1 <- train(LOCATION~., data=training1, method="rf", trcontrol=fitControl, tuneLength=5))
system.time(RFfit2 <- train(LOCATION~., data=training2, method="rf", trcontrol=fitControl, tuneLength=5))

b0predict_RF<-predict(RFfit0, testing0)
b0_ConfusionMatrix_RF<-confusionMatrix(b0predict_RF, testing0$LOCATION)
b0_ConfusionMatrix_RF
b1predict_RF<-predict(RFfit1, testing1)
b1_ConfusionMatrix_RF<-confusionMatrix(b1predict_RF, testing1$LOCATION)
b1_ConfusionMatrix_RF
b2predict_RF<-predict(RFfit2, testing2)
b2_ConfusionMatrix_RF<-confusionMatrix(b2predict_RF, testing2$LOCATION)
b2_ConfusionMatrix_RF

# SVM
system.time(SVMfit0 <- train(LOCATION~., data = training0, method = "svmLinear", trControl=fitControl))
system.time(SVMfit1 <- train(LOCATION~., data = training1, method = "svmLinear", trControl=fitControl))
system.time(SVMfit2 <- train(LOCATION~., data = training2, method = "svmLinear", trControl=fitControl))

b0predict_SVM<-predict(SVMfit0, testing0)
b0_ConfusionMatrix_SVM<-confusionMatrix(b0predict_SVM, testing0$LOCATION)
b0_ConfusionMatrix_SVM
b1predict_SVM<-predict(SVMfit1, testing1)
b1_ConfusionMatrix_SVM<-confusionMatrix(b1predict_SVM, testing1$LOCATION)
b1_ConfusionMatrix_SVM
b2predict_SVM<-predict(SVMfit2, testing2)
b2_ConfusionMatrix_SVM<-confusionMatrix(b2predict_SVM, testing2$LOCATION)
b2_ConfusionMatrix_SVM

# compare models
b0Data <- resamples(list(C50=C50Fit0, KNN = knnFit0, rpart = rpartFit0))
summary(b0Data)
bwplot(b0Data)
dotplot(b0Data)
b1Data <- resamples(list(C50=C50Fit1, KNN = knnFit1, rpart = rpartFit1))
summary(b1Data)
bwplot(b1Data)
dotplot(b1Data)
b2Data <- resamples(list(C50=C50Fit2, KNN = knnFit2, rpart = rpartFit2))
summary(b2Data)
bwplot(b2Data)
dotplot(b2Data)


# Stop Cluster. After performing your tasks, stop your cluster. 
stopCluster(cl)