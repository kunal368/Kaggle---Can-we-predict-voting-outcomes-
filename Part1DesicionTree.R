#Kunal Agarwal 26/6/2015
# Kaggele:Show of Hands Problem Part1:Desicion Tree

#Loading datasets
train <- read.csv("train2016.csv")
test <- read.csv("test2016.csv")

#Inspecting training dataset
str(train)

prop.table(table(train$Party))

table(train$Gender)
prop.table(table(train$Party, train$Gender),2)

prop.table(table(train$Party, train$Q109244),2)
table(train$YOB)

library(rpart)

#Making Desicion Tree
DesicionModel <- rpart(Party ~ .-USER_ID, data = train, method = "class" )

#Installing Libraries
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#PLotting desicion tree
fancyRpartPlot(DesicionModel)

#making Predictions
PredictDtree <- predict(DesicionModel, newdata = test, type = "class")

#Writing to file
submit <- data.frame(USER_ID = test$USER_ID, Predictions = PredictDtree)
write.csv(submit, file = "desiciontree.csv", row.names =FALSE)
