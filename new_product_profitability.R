# Install library packages
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("readr")
install.packages("corrplot")

# Calling library
library(readr)
library(caret)
library(corrplot)


# Import Dataset
existing_products <- read_csv("existingproductattributes2017.csv")

# Dummify the data
newDataFrame <- dummyVars(" ~ .", data = existing_products)
readyData <- data.frame(predict(newDataFrame, newdata = existing_products))

# Pre-Processing Dataset
View(readyData)
str(readyData)
summary(readyData)
# Remove attributes with NA
readyData$BestSellersRank <- NULL
# Remove ProductNum as it is a unique identifier and does not contribute to modeling.
readyData$ProductNum <- NULL

corrData <- cor(readyData)
corrData
corrplot(corrData)

# Remove 5-star review since it has a correlation of 1 with Volume
readyData$x5StarReviews <- NULL
#Remove 3-star review and 1-star review since they have >0.9 correlation to 4-star review and 2-star review.
readyData$x3StarReviews <- NULL
#readyData$x1StarReviews <- NULL


# Set seed
set.seed(998)

# Create training & testing partition
inTraining <- createDataPartition(readyData$Volume, p = .75, list = FALSE)
training <- readyData[inTraining,]
testing <- readyData[-inTraining,]

# 10-fold cross-validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

# SVM
svmFit1 <- train(Volume~., data = training, 
                 method = "svmLinear",
                 trControl=fitControl,
                 preProc = c("center", "scale"))
svmFit1
svmPred<-predict(svmFit1,testing)
postResample(testing$Volume,svmPred)

# Gradient Boosting
gbmFit1 <- train(Volume~., data=training, method = 'gbm', 
                trControl=fitControl)
gbmFit1
gbmPred<-predict(gbmFit1,testing)
postResample(testing$Volume,gbmPred)

# Random Forest
rfFit1 <- train(Volume~., data = training, method = "rf", trControl=fitControl)
rfFit1
rfPred<-predict(rfFit1,testing)
postResample(testing$Volume,rfPred)


# import new products dataset
new_products <- read_csv("newproductattributes2017.csv")
View(new_products)

# Dummify the data
newproductsDataFrame <- dummyVars(" ~ .", data = new_products)
readyNewData <- data.frame(predict(newproductsDataFrame, newdata = new_products))

# make prediction on new products using Random Forest Model
rfNPPred<-predict(rfFit1,readyNewData)
rfNPPred

# Combine prediction with new products dataset and write to csv file
output <- new_products 
output$predictionsrf <- rfNPPred
output$Profit <- output$ProfitMargin * output$predictionsrf
profit <- output[order(-output$Profit),] 
write.csv(profit, file="newproducts_profit.csv", row.names = TRUE)


