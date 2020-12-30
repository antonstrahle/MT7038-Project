source("data.R")
library(ipred)
library(adabag)
library(gbm)


#Tree
tree <- rpart(Occupancy~., data = trainingData, method = "class")
rpart.plot(tree)
treepred <- predict(tree, testingData, type = "class")
table(testingData$Occupancy, treepred)

formula <- Occupancy ~ .

#Bagging
bagged <- ipred::bagging(formula, data = trainingData, nbagg = 1000, coob = TRUE)
#Out-of-bag error
print(bagged)

bagpred <- predict(bagged, newdata = testingData)



train.error <- mean(sqrt((as.numeric(as.character(bagpred)) - as.numeric(as.character(testingData$Occupancy)))^2))

#Boosting
#boosting function not found
boosttree <- boosting(formula, data = trainingData, boos = TRUE, mfinal = 1000)

boostpred <- predict.boosting(boosttree, newdata = testingData)

boostpred$error