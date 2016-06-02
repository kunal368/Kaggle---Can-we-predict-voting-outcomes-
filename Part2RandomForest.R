# Solving Kaggle Problem : Show of Hands
#Analytics Edge Course 
#Using Random Forest (Got score of 0.63793 on Kaggle)

#Loading Data Sets
train <- read.csv("train2016.csv")
test <- read.csv("test2016.csv")

str(train)
#Loading Libraries for Imputation
library(mice)
table(train$Party)

gh <- train[,2:108]
k <- test[,2:107]
#Using Imputation for Missing Values on Testing and training dataset
set.seed(328)
imputed1 <- complete(mice(gh))
imputed2 <- complete(mice(k))
summary(imputed1)

#Filling Generated Values in original Datasets
train$YOB <- imputed1$YOB
test$YOB <- imputed2$YOB

#Using Random Forest for creating Model
library(randomForest)

set.seed(697)
fit <- randomForest(Party ~ .-USER_ID, data = train, importance = TRUE, ntrees= 1200)
fit1 <- randomForest(Party ~ .-USER_ID, data = train, importance = TRUE, ntrees=600)

#Looking at Imp Variables
varImpPlot(fit)

#Making Predictions on test dataset
Pollprediction <- predict(fit, newdata = test)
summary(Pollprediction)

#Writing Predictions to the file
submission <- data.frame(USER_ID = test$USER_ID, Predictions = Pollprediction)
write.csv(submission, file = "randomforestsubmission3.csv", row.names = FALSE)

# Build condition inference tree Random Forest
library(party)
set.seed(415)
cifit <- cforest(Party ~ .-USER_ID, data = train, controls=cforest_unbiased(ntree=1000, mtry=3)) 

# Now let's make a prediction and write a submission file
ciPredict <- predict(cifit, test, OOB=TRUE, type = "response")
summary(ciPredict)
submit <- data.frame(PassengerId = test$USER_ID, Predictions = ciPredict)
write.csv(submit, file = "ciforest1.csv", row.names = FALSE)


