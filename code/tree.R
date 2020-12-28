source("data.R")
library(ipred)


#Tree
tree <- rpart(Occupancy~., data = trainingData, method = "class")
rpart.plot(tree)
treepred <- predict(tree, testingData, type = "class")
table(testingData$Occupancy, treepred)

formula <- Occupancy ~ .

bagged <- bagging(formula, data = trainingData, nbagg = 1000, coob = TRUE)
print(bagged)










