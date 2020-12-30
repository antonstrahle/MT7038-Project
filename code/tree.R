source("data.R")
library(ipred)
library(adabag)
library(gbm)


#Tree
formula <- Occupancy ~ .

tree <- rpart(formula, data = trainingData, method = "class")
rpart.plot(tree)
treepred <- predict(tree, testingData, type = "class")
table(testingData$Occupancy, treepred)



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