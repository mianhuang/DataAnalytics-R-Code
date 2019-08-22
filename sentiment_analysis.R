# clear workspace
rm(list = ls())

# Load Libraries: p_load can install,load, and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
} 

pacman::p_load(readr, caret, ggplot2, tidyverse, tidyr, dplyr, lubridate, plotly, C50, 
               tibbletime, doParallel, corrplot, kknn, e1071)

# import dataset
iphone_orig <- read_csv("iphone_smallmatrix_labeled_8d.csv")
galaxy_orig <- read_csv("galaxy_smallmatrix_labeled_9d.csv")

# make working copy
iphoneDF <- iphone_orig
galaxyDF <- galaxy_orig

# initial examination of datasets
str(iphoneDF)
summary(iphoneDF)
plot_ly(iphoneDF, x= ~iphonesentiment, type='histogram')

str(galaxyDF)
summary(galaxyDF)
plot_ly(galaxyDF, x= ~galaxysentiment, type='histogram')

# pre-processing datasets
## remove highly correlated features
options(max.print=1000000)
iphone_Cor <- cor(iphoneDF)
print(iphone_Cor)
corrplot(iphone_Cor)
highlyCorrelated_i <- findCorrelation(iphone_Cor, cutoff=0.9)
print(highlyCorrelated_i)
iphoneCOR <- iphoneDF[,-highlyCorrelated_i]

galaxy_Cor <- cor(galaxyDF)
print(galaxy_Cor)
corrplot(galaxy_Cor)
highlyCorrelated_g <- findCorrelation(galaxy_Cor, cutoff=0.9)
print(highlyCorrelated_g)
galaxyCOR <- galaxyDF[,-highlyCorrelated_g]

## create a new data set and remove near zero variance features
nzvMetrics <- nearZeroVar(iphoneDF, saveMetrics = TRUE)
nzvMetrics
iphone_nzv <- nearZeroVar(iphoneDF, saveMetrics = FALSE) 
iphone_nzv
iphoneNZV <- iphoneDF[,-iphone_nzv]
str(iphoneNZV)

galaxy_nzv <- nearZeroVar(galaxyDF, saveMetrics = FALSE) 
galaxy_nzv
galaxyNZV <- galaxyDF[,-galaxy_nzv]
str(galaxyNZV)

## recursive feature elimination (rfe)
set.seed(123)
iphoneSample <- iphoneDF[sample(1:nrow(iphoneDF), 1000, replace=FALSE),]
galaxySample <- galaxyDF[sample(1:nrow(galaxyDF), 1000, replace=FALSE),]

### Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
### Use rfe and omit the response variable (attribute 59 iphonesentiment) 
iphone_rfe <- rfe(iphoneSample[,1:58], 
                  iphoneSample$iphonesentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrl)
galaxy_rfe <- rfe(galaxySample[,1:58], 
                  galaxySample$galaxysentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrl)
### Get results
iphone_rfe
galaxy_rfe
### Plot results
plot(iphone_rfe, type=c("g", "o"))
plot(galaxy_rfe, type=c("g", "o"))
### create new data set with rfe recommended features
iphoneRFE <- iphoneDF[,predictors(iphone_rfe)]
galaxyRFE <- galaxyDF[,predictors(galaxy_rfe)]
### add the dependent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphoneDF$iphonesentiment
galaxyRFE$galaxysentiment <- galaxyDF$galaxysentiment
### review outcome
str(iphoneRFE)
str(galaxyRFE)

## update attributes for iphonesentiment & galaxysentiment to factor
iphoneDF$iphonesentiment <- as.factor(iphoneDF$iphonesentiment)
iphoneCOR$iphonesentiment <- as.factor(iphoneCOR$iphonesentiment)
iphoneNZV$iphonesentiment <- as.factor(iphoneNZV$iphonesentiment)
iphoneRFE$iphonesentiment <- as.factor(iphoneRFE$iphonesentiment)

galaxyDF$galaxysentiment <- as.factor(galaxyDF$galaxysentiment)
galaxyCOR$galaxysentiment <- as.factor(galaxyCOR$galaxysentiment)
galaxyNZV$galaxysentiment <- as.factor(galaxyNZV$galaxysentiment)
galaxyRFE$galaxysentiment <- as.factor(galaxyRFE$galaxysentiment)

# Model Develoment
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

## "Out of Box" Models
iphoneDF_inTraining <- createDataPartition(iphoneDF$iphonesentiment, p = .7, list = FALSE)
iphoneDF_training <- iphoneDF[iphoneDF_inTraining,]
iphoneDF_testing <- iphoneDF[-iphoneDF_inTraining,]

galaxyDF_inTraining <- createDataPartition(galaxyDF$galaxysentiment, p = .7, list = FALSE)
galaxyDF_training <- galaxyDF[galaxyDF_inTraining,]
galaxyDF_testing <- galaxyDF[-galaxyDF_inTraining,]

### kknn (from the kknn package)
system.time(iphoneDF_kknnFit0 <- kknn(iphonesentiment~., iphoneDF_training, iphoneDF_testing, 
                                      distance=1, kernel="triangular"))
iphoneDF_kknn_CM<-confusionMatrix(iphoneDF_kknnFit0$fitted.values, iphoneDF_testing$iphonesentiment)
iphoneDF_kknn_CM
postResample(iphoneDF_testing$iphonesentiment,iphoneDF_kknnFit0$fitted.values)

system.time(galaxyDF_kknnFit0 <- kknn(galaxysentiment~., galaxyDF_training, galaxyDF_testing, 
                                      distance=1, kernel="triangular"))
galaxyDF_kknn_CM<-confusionMatrix(galaxyDF_kknnFit0$fitted.values, galaxyDF_testing$galaxysentiment)
galaxyDF_kknn_CM
postResample(galaxyDF_testing$galaxysentiment,galaxyDF_kknnFit0$fitted.values)

###	C5.0
system.time(iphoneDF_C50Fit0 <- train(iphonesentiment~., data = iphoneDF_training, method = "C5.0", trControl=fitControl))
iphoneDF_C50pred<-predict(iphoneDF_C50Fit0, iphoneDF_testing)
iphoneDF_C50_CM<-confusionMatrix(iphoneDF_C50pred, iphoneDF_testing$iphonesentiment)
iphoneDF_C50_CM
postResample(iphoneDF_testing$iphonesentiment,iphoneDF_C50pred)

system.time(galaxyDF_C50Fit0 <- train(galaxysentiment~., data = galaxyDF_training, method = "C5.0", trControl=fitControl))
galaxyDF_C50pred<-predict(galaxyDF_C50Fit0, galaxyDF_testing)
galaxyDF_C50_CM<-confusionMatrix(galaxyDF_C50pred, galaxyDF_testing$galaxysentiment)
galaxyDF_C50_CM
postResample(galaxyDF_testing$galaxysentiment,galaxyDF_C50pred)

###	Random Forest
system.time(iphoneDF_RFFit0 <- train(iphonesentiment~., data = iphoneDF_training, method = "rf", trControl=fitControl))
iphoneDF_RFpred<-predict(iphoneDF_RFFit0, iphoneDF_testing)
iphoneDF_RF_CM<-confusionMatrix(iphoneDF_RFpred, iphoneDF_testing$iphonesentiment)
iphoneDF_RF_CM
postResample(iphoneDF_testing$iphonesentiment,iphoneDF_RFpred)

system.time(galaxyDF_RFFit0 <- train(galaxysentiment~., data = galaxyDF_training, method = "rf", trControl=fitControl))
galaxyDF_RFpred<-predict(galaxyDF_RFFit0, galaxyDF_testing)
galaxyDF_RF_CM<-confusionMatrix(galaxyDF_RFpred, galaxyDF_testing$galaxysentiment)
galaxyDF_RF_CM
postResample(galaxyDF_testing$galaxysentiment,galaxyDF_RFpred)

###	SVM (from the e1071 package) 
system.time(iphoneDF_SVMFit0 <- svm(iphonesentiment~ ., data = iphoneDF))
iphoneDF_SVMpred<-predict(iphoneDF_SVMFit0, iphoneDF_testing)
iphoneDF_SVM_CM<-confusionMatrix(iphoneDF_SVMpred, iphoneDF_testing$iphonesentiment)
iphoneDF_SVM_CM
postResample(iphoneDF_testing$iphonesentiment,iphoneDF_SVMpred)

system.time(galaxyDF_SVMFit0 <- svm(galaxysentiment~ ., data = galaxyDF))
galaxyDF_SVMpred<-predict(galaxyDF_SVMFit0, galaxyDF_testing)
galaxyDF_SVM_CM<-confusionMatrix(galaxyDF_SVMpred, galaxyDF_testing$galaxysentiment)
galaxyDF_SVM_CM
postResample(galaxyDF_testing$galaxysentiment,galaxyDF_SVMpred)

## Random Forest Model on Feature Selection Datasets
### Dataset with highly correlated variables removed
iphoneCOR_inTraining <- createDataPartition(iphoneCOR$iphonesentiment, p = .7, list = FALSE)
iphoneCOR_training <- iphoneCOR[iphoneCOR_inTraining,]
iphoneCOR_testing <- iphoneCOR[-iphoneCOR_inTraining,]

galaxyCOR_inTraining <- createDataPartition(galaxyCOR$galaxysentiment, p = .7, list = FALSE)
galaxyCOR_training <- galaxyCOR[galaxyCOR_inTraining,]
galaxyCOR_testing <- galaxyCOR[-galaxyCOR_inTraining,]

system.time(iphoneCOR_RFFit0 <- train(iphonesentiment~., data = iphoneCOR_training, method = "rf", trControl=fitControl))
iphoneCOR_RFpred<-predict(iphoneCOR_RFFit0, iphoneCOR_testing)
iphoneCOR_RF_CM<-confusionMatrix(iphoneCOR_RFpred, iphoneCOR_testing$iphonesentiment)
iphoneCOR_RF_CM
postResample(iphoneCOR_testing$iphonesentiment,iphoneCOR_RFpred)

system.time(galaxyCOR_RFFit0 <- train(galaxysentiment~., data = galaxyCOR_training, method = "rf", trControl=fitControl))
galaxyCOR_RFpred<-predict(galaxyCOR_RFFit0, galaxyCOR_testing)
galaxyCOR_RF_CM<-confusionMatrix(galaxyCOR_RFpred, galaxyCOR_testing$galaxysentiment)
galaxyCOR_RF_CM
postResample(galaxyCOR_testing$galaxysentiment,galaxyCOR_RFpred)

### Dataset with near-zero variance variables removed
iphoneNZV_inTraining <- createDataPartition(iphoneNZV$iphonesentiment, p = .7, list = FALSE)
iphoneNZV_training <- iphoneNZV[iphoneNZV_inTraining,]
iphoneNZV_testing <- iphoneNZV[-iphoneNZV_inTraining,]

galaxyNZV_inTraining <- createDataPartition(galaxyNZV$galaxysentiment, p = .7, list = FALSE)
galaxyNZV_training <- galaxyNZV[galaxyNZV_inTraining,]
galaxyNZV_testing <- galaxyNZV[-galaxyNZV_inTraining,]

system.time(iphoneNZV_RFFit0 <- train(iphonesentiment~., data = iphoneNZV_training, method = "rf", trControl=fitControl))
iphoneNZV_RFpred<-predict(iphoneNZV_RFFit0, iphoneNZV_testing)
iphoneNZV_RF_CM<-confusionMatrix(iphoneNZV_RFpred, iphoneNZV_testing$iphonesentiment)
iphoneNZV_RF_CM
postResample(iphoneNZV_testing$iphonesentiment,iphoneNZV_RFpred)

system.time(galaxyNZV_RFFit0 <- train(galaxysentiment~., data = galaxyNZV_training, method = "rf", trControl=fitControl))
galaxyNZV_RFpred<-predict(galaxyNZV_RFFit0, galaxyNZV_testing)
galaxyNZV_RF_CM<-confusionMatrix(galaxyNZV_RFpred, galaxyNZV_testing$galaxysentiment)
galaxyNZV_RF_CM
postResample(galaxyNZV_testing$galaxysentiment,galaxyNZV_RFpred)

### Dataset with RFE
iphoneRFE_inTraining <- createDataPartition(iphoneRFE$iphonesentiment, p = .7, list = FALSE)
iphoneRFE_training <- iphoneRFE[iphoneRFE_inTraining,]
iphoneRFE_testing <- iphoneRFE[-iphoneRFE_inTraining,]

galaxyRFE_inTraining <- createDataPartition(galaxyRFE$galaxysentiment, p = .7, list = FALSE)
galaxyRFE_training <- galaxyRFE[galaxyRFE_inTraining,]
galaxyRFE_testing <- galaxyRFE[-galaxyRFE_inTraining,]

system.time(iphoneRFE_RFFit0 <- train(iphonesentiment~., data = iphoneRFE_training, method = "rf", trControl=fitControl))
iphoneRFE_RFpred<-predict(iphoneRFE_RFFit0, iphoneRFE_testing)
iphoneRFE_RF_CM<-confusionMatrix(iphoneRFE_RFpred, iphoneRFE_testing$iphonesentiment)
iphoneRFE_RF_CM
postResample(iphoneRFE_testing$iphonesentiment,iphoneRFE_RFpred)

system.time(galaxyRFE_RFFit0 <- train(galaxysentiment~., data = galaxyRFE_training, method = "rf", trControl=fitControl))
galaxyRFE_RFpred<-predict(galaxyRFE_RFFit0, galaxyRFE_testing)
galaxyRFE_RF_CM<-confusionMatrix(galaxyRFE_RFpred, galaxyRFE_testing$galaxysentiment)
galaxyRFE_RF_CM
postResample(galaxyRFE_testing$galaxysentiment,galaxyRFE_RFpred)

# create a new dataset & recode sentiment to combine factor levels 0 & 1 and 4 & 5
iphoneRC <- iphoneDF
iphoneRC$iphonesentiment <- recode(iphoneRC$iphonesentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
summary(iphoneRC)
str(iphoneRC)
iphoneRC$iphonesentiment <- as.factor(iphoneRC$iphonesentiment)

galaxyRC <- galaxyDF
galaxyRC$galaxysentiment <- recode(galaxyRC$galaxysentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
summary(galaxyRC)
str(galaxyRC)
galaxyRC$galaxysentiment <- as.factor(galaxyRC$galaxysentiment)

## Feature Selection on recoded dataset
iphoneRCCOR <- iphoneRC[,-highlyCorrelated_i]
galaxyRCCOR <- galaxyRC[,-highlyCorrelated_g]

iphoneRCNZV <- iphoneRC[,-iphone_nzv]
galaxyRCNZV <- galaxyRC[,-galaxy_nzv]

iphoneRCRFE <- iphoneRC[,predictors(iphone_rfe)]
galaxyRCRFE <- galaxyRC[,predictors(galaxy_rfe)]
iphoneRCRFE$iphonesentiment <- iphoneRC$iphonesentiment
galaxyRCRFE$galaxysentiment <- galaxyRC$galaxysentiment

iphoneRCCOR$iphonesentiment <- as.factor(iphoneRCCOR$iphonesentiment)
iphoneRCNZV$iphonesentiment <- as.factor(iphoneRCNZV$iphonesentiment)
iphoneRCRFE$iphonesentiment <- as.factor(iphoneRCRFE$iphonesentiment)

galaxyRCCOR$galaxysentiment <- as.factor(galaxyRCCOR$galaxysentiment)
galaxyRCNZV$galaxysentiment <- as.factor(galaxyRCNZV$galaxysentiment)
galaxyRCRFE$galaxysentiment <- as.factor(galaxyRCRFE$galaxysentiment)

## Random Forest Model on recoded dataset
iphoneRC_inTraining <- createDataPartition(iphoneRC$iphonesentiment, p = .7, list = FALSE)
iphoneRC_training <- iphoneRC[iphoneRC_inTraining,]
iphoneRC_testing <- iphoneRC[-iphoneRC_inTraining,]

galaxyRC_inTraining <- createDataPartition(galaxyRC$galaxysentiment, p = .7, list = FALSE)
galaxyRC_training <- galaxyRC[galaxyRC_inTraining,]
galaxyRC_testing <- galaxyRC[-galaxyRC_inTraining,]

system.time(iphoneRC_RFFit0 <- train(iphonesentiment~., data = iphoneRC_training, method = "rf", trControl=fitControl))
iphoneRC_RFpred<-predict(iphoneRC_RFFit0, iphoneRC_testing)
iphoneRC_RF_CM<-confusionMatrix(iphoneRC_RFpred, iphoneRC_testing$iphonesentiment)
iphoneRC_RF_CM
postResample(iphoneRC_testing$iphonesentiment,iphoneRC_RFpred)

system.time(galaxyRC_RFFit0 <- train(galaxysentiment~., data = galaxyRC_training, method = "rf", trControl=fitControl))
galaxyRC_RFpred<-predict(galaxyRC_RFFit0, galaxyRC_testing)
galaxyRC_RF_CM<-confusionMatrix(galaxyRC_RFpred, galaxyRC_testing$galaxysentiment)
galaxyRC_RF_CM
postResample(galaxyRC_testing$galaxysentiment,galaxyRC_RFpred)

### Dataset with highly correlated variables removed
iphoneRCCOR_inTraining <- createDataPartition(iphoneRCCOR$iphonesentiment, p = .7, list = FALSE)
iphoneRCCOR_training <- iphoneRCCOR[iphoneRCCOR_inTraining,]
iphoneRCCOR_testing <- iphoneRCCOR[-iphoneRCCOR_inTraining,]

galaxyRCCOR_inTraining <- createDataPartition(galaxyRCCOR$galaxysentiment, p = .7, list = FALSE)
galaxyRCCOR_training <- galaxyRCCOR[galaxyRCCOR_inTraining,]
galaxyRCCOR_testing <- galaxyRCCOR[-galaxyRCCOR_inTraining,]

system.time(iphoneRCCOR_RFFit0 <- train(iphonesentiment~., data = iphoneRCCOR_training, method = "rf", trControl=fitControl))
iphoneRCCOR_RFpred<-predict(iphoneRCCOR_RFFit0, iphoneRCCOR_testing)
iphoneRCCOR_RF_CM<-confusionMatrix(iphoneRCCOR_RFpred, iphoneRCCOR_testing$iphonesentiment)
iphoneRCCOR_RF_CM
postResample(iphoneRCCOR_testing$iphonesentiment,iphoneRCCOR_RFpred)

system.time(galaxyRCCOR_RFFit0 <- train(galaxysentiment~., data = galaxyRCCOR_training, method = "rf", trControl=fitControl))
galaxyRCCOR_RFpred<-predict(galaxyRCCOR_RFFit0, galaxyRCCOR_testing)
galaxyRCCOR_RF_CM<-confusionMatrix(galaxyRCCOR_RFpred, galaxyRCCOR_testing$galaxysentiment)
galaxyRCCOR_RF_CM
postResample(galaxyRCCOR_testing$galaxysentiment,galaxyRCCOR_RFpred)

### Dataset with near-zero variance variables removed
iphoneRCNZV_inTraining <- createDataPartition(iphoneRCNZV$iphonesentiment, p = .7, list = FALSE)
iphoneRCNZV_training <- iphoneRCNZV[iphoneRCNZV_inTraining,]
iphoneRCNZV_testing <- iphoneRCNZV[-iphoneRCNZV_inTraining,]

galaxyRCNZV_inTraining <- createDataPartition(galaxyRCNZV$galaxysentiment, p = .7, list = FALSE)
galaxyRCNZV_training <- galaxyRCNZV[galaxyRCNZV_inTraining,]
galaxyRCNZV_testing <- galaxyRCNZV[-galaxyRCNZV_inTraining,]

system.time(iphoneRCNZV_RFFit0 <- train(iphonesentiment~., data = iphoneRCNZV_training, method = "rf", trControl=fitControl))
iphoneRCNZV_RFpred<-predict(iphoneRCNZV_RFFit0, iphoneRCNZV_testing)
iphoneRCNZV_RF_CM<-confusionMatrix(iphoneRCNZV_RFpred, iphoneRCNZV_testing$iphonesentiment)
iphoneRCNZV_RF_CM
postResample(iphoneRCNZV_testing$iphonesentiment,iphoneRCNZV_RFpred)

system.time(galaxyRCNZV_RFFit0 <- train(galaxysentiment~., data = galaxyRCNZV_training, method = "rf", trControl=fitControl))
galaxyRCNZV_RFpred<-predict(galaxyRCNZV_RFFit0, galaxyRCNZV_testing)
galaxyRCNZV_RF_CM<-confusionMatrix(galaxyRCNZV_RFpred, galaxyRCNZV_testing$galaxysentiment)
galaxyRCNZV_RF_CM
postResample(galaxyRCNZV_testing$galaxysentiment,galaxyRCNZV_RFpred)

### Dataset with RFE
iphoneRCRFE_inTraining <- createDataPartition(iphoneRCRFE$iphonesentiment, p = .7, list = FALSE)
iphoneRCRFE_training <- iphoneRCRFE[iphoneRCRFE_inTraining,]
iphoneRCRFE_testing <- iphoneRCRFE[-iphoneRCRFE_inTraining,]

galaxyRCRFE_inTraining <- createDataPartition(galaxyRCRFE$galaxysentiment, p = .7, list = FALSE)
galaxyRCRFE_training <- galaxyRCRFE[galaxyRCRFE_inTraining,]
galaxyRCRFE_testing <- galaxyRCRFE[-galaxyRCRFE_inTraining,]

system.time(iphoneRCRFE_RFFit0 <- train(iphonesentiment~., data = iphoneRCRFE_training, method = "rf", trControl=fitControl))
iphoneRCRFE_RFpred<-predict(iphoneRCRFE_RFFit0, iphoneRCRFE_testing)
iphoneRCRFE_RF_CM<-confusionMatrix(iphoneRCRFE_RFpred, iphoneRCRFE_testing$iphonesentiment)
iphoneRCRFE_RF_CM
postResample(iphoneRCRFE_testing$iphonesentiment,iphoneRCRFE_RFpred)

system.time(galaxyRCRFE_RFFit0 <- train(galaxysentiment~., data = galaxyRCRFE_training, method = "rf", trControl=fitControl))
galaxyRCRFE_RFpred<-predict(galaxyRCRFE_RFFit0, galaxyRCRFE_testing)
galaxyRCRFE_RF_CM<-confusionMatrix(galaxyRCRFE_RFpred, galaxyRCRFE_testing$galaxysentiment)
galaxyRCRFE_RF_CM
postResample(galaxyRCRFE_testing$galaxysentiment,galaxyRCRFE_RFpred)

# Principal Component Analysis
## create object containing centered, scaled PCA components from training set
## excluded the dependent variable and set threshold to .95
i_preprocessParams <- preProcess(iphoneDF_training[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(i_preprocessParams)
g_preprocessParams <- preProcess(galaxyDF_training[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(g_preprocessParams)

## use predict to apply pca parameters, create training & testing dataset, exclude dependant
iphone_train.pca <- predict(i_preprocessParams, iphoneDF_training[,-59])
iphone_test.pca <- predict(i_preprocessParams, iphoneDF_testing[,-59])

galaxy_train.pca <- predict(g_preprocessParams, galaxyDF_training[,-59])
galaxy_test.pca <- predict(g_preprocessParams, galaxyDF_testing[,-59])

iphone_train.pca$iphonesentiment <- iphoneDF_training$iphonesentiment
iphone_test.pca$iphonesentiment <- iphoneDF_testing$iphonesentiment

galaxy_train.pca$galaxysentiment <- galaxyDF_training$galaxysentiment
galaxy_test.pca$galaxysentiment <- galaxyDF_testing$galaxysentiment


str(iphone_train.pca)
str(iphone_test.pca)
str(galaxy_train.pca)
str(galaxy_test.pca)

## Random Forest on PCA dataset
system.time(iphonePCA_RFFit0 <- train(iphonesentiment~., data = iphone_train.pca, method = "rf", trControl=fitControl))
iphonePCA_RFpred<-predict(iphonePCA_RFFit0, iphone_test.pca)
iphonePCA_RF_CM<-confusionMatrix(iphonePCA_RFpred, iphone_test.pca$iphonesentiment)
iphonePCA_RF_CM
postResample(iphone_test.pca$iphonesentiment,iphonePCA_RFpred)

system.time(galaxyPCA_RFFit0 <- train(galaxysentiment~., data = galaxy_train.pca, method = "rf", trControl=fitControl))
galaxyPCA_RFpred<-predict(galaxyPCA_RFFit0, galaxy_test.pca)
galaxyPCA_RF_CM<-confusionMatrix(galaxyPCA_RFpred, galaxy_test.pca$galaxysentiment)
galaxyPCA_RF_CM
postResample(galaxy_test.pca$galaxysentiment,galaxyPCA_RFpred)

# Apply Model to LargeMatrix
iphone_large <- read_csv("iphoneLargeMatrix.csv")
galaxy_large <- read_csv("galaxyLargeMatrix.csv")

iphone_large$id <- NULL
galaxy_large$id <- NULL

iphoneLG_pred<-predict(iphoneRC_RFFit0, iphone_large)
galaxyLG_pred<-predict(galaxyRC_RFFit0, galaxy_large)
summary(iphoneLG_pred)
summary(galaxyLG_pred)

# set up parallel processing
## Find how many cores are on your machine
detectCores() # Result = Typically 4 to 6
# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes. 
cl <- makeCluster(detectCores()-2)
## Register Cluster
registerDoParallel(cl)
## Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2 

# Stop Cluster. After performing your tasks, stop your cluster. 
stopCluster(cl)
