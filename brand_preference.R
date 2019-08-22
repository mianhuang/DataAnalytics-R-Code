# install library packages
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("ggplot2")
install.packages("generics")
install.packages("gower")
install.packages("ModelMetrics")
install.packages("klaR")
install.packages("promises")
install.packages("mime")
install.packages("readr")
install.packages("C50")
install.packages("inum")


# calling library
library(readr)
library(caret)
library(C50)
library(mlbench)

# import & pre-process dataset
CompleteResponses <- read_csv("CompleteResponses.csv")
View(CompleteResponses)
str(CompleteResponses)
CompleteResponses$brand<-as.factor(CompleteResponses$brand)
CompleteResponses$elevel<-as.factor(CompleteResponses$elevel)
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
str(CompleteResponses)
summary(CompleteResponses)
is.na(CompleteResponses)

set.seed(998)

# create training & testing partition
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]

# C5.0 Decision Tree model
dtreectrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, summaryFunction = defaultSummary)
dtreeFit1 <- train(brand~., data = training, method = "C5.0", trControl=dtreectrl)
dtreeFit1

# make predictions with C5.0 Decision Tree model
dtreeBrand <- predict(dtreeFit1, newdata = testing)
# Check C5.0 model performance
confusionMatrix(data = dtreeBrand, testing$brand)
varImp(dtreeFit1)
postResample(testing$brand,dtreeBrand)

# Random Forest Manual 10-fold cross validation and manually tune 5 different mtry value
rfctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))
system.time(rfFitm1 <- train(brand~., data = training, method = "rf", trControl=rfctrl, tuneGrid=rfGrid))
rfFitm1

# make predictions with Random Forest model
rfBrand <- predict(rfFitm1, newdata = testing)
#Check C5.0 model performance
confusionMatrix(data = rfBrand, testing$brand)
varImp(rfFitm1)
postResample(testing$brand,rfBrand)


#import & pre-process surveyIncomplete
SurveyIncomplete <- read_csv("SurveyIncomplete.csv")
View(SurveyIncomplete)
SurveyIncomplete$brand<-as.factor(SurveyIncomplete$brand)
SurveyIncomplete$elevel<-as.factor(SurveyIncomplete$elevel)
SurveyIncomplete$car<-as.factor(SurveyIncomplete$car)
SurveyIncomplete$zipcode<-as.factor(SurveyIncomplete$zipcode)
str(SurveyIncomplete)
summary(SurveyIncomplete)
is.na(SurveyIncomplete)

# make predictions for surveyIncomplete with the chosen model (C5.0 Decision Tree)
dtreeIncomplete <- predict(dtreeFit1, newdata = SurveyIncomplete)

# Check C5.0 model performance
confusionMatrix(data = dtreeIncomplete, SurveyIncomplete$brand)
varImp(dtreeFit1)
postResample(SurveyIncomplete$brand,dtreeIncomplete)

# Plot predictions
brand_cnt = summary(dtreeIncomplete)
lbls <- c("Acer", "Sony")
pct <- round(brand_cnt/sum(brand_cnt)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(brand_cnt, labels=lbls, main="Brand Preference Pie Chart")

# Write results to csv file
dtreeResults<-cbind(SurveyIncomplete,dtreeIncomplete)
write.csv(dtreeResults,file="results.csv")

