source("data.R")
library(mboost)
library(glmnet)


logit.model<- glm(Occupancy ~ ., data = trainingData, family = binomial(link = "logit"))

pred.logit <- predict(logit.model, testingData, type = "response")
pred.logit <- ifelse(pred.logit > 0.5,1,0)


logit.test.error <- mean(pred.logit != testingData$Occupancy)


#Boost
logit.model.boost <- glmboost(Occupancy ~ ., data = trainingData, family = Binomial(type = "adaboost", link = "logit"))

pred.boost <- predict(logit.model.boost, testingData, type = "class")


boost.test.error <- mean(pred.boost != testingData$Occupancy)


#Regularization datamatrix for glmnet

xTrain <- model.matrix(Occupancy~., trainingData)[,-1]

yTrain <- trainingData$Occupancy
  
xTest <- model.matrix(Occupancy~., testingData)[,-1]

yTest <- testingData$Occupancy

xVal <- model.matrix(Occupancy~., validationData)[,-1]

yVal <- validationData$Occupancy

#Lasso
lasso.model <- glmnet(xTrain, yTrain, family = "binomial", alpha = 1)

pred.lasso <- predict(lasso.model, xTest, type = "class")

lasso.test.error <- mean(pred.lasso != yTest)

for(l in 10^seq(-1, 0, by = 0.01)){
  
  m <- glmnet(xTrain, yTrain, alpha = 1, family = "binomial", type.measure = "class", lambda = l)
  
  probs <- m %>% predict(newx = xVal)
  pred <- probs > 0
  
  valError <- mean(pred != as.numeric(as.character(yVal)))
  
  print(paste("Validation Error", valError, "for lambda =", l))
  
}


#Ridge
ridge.model <- glmnet(xTrain, trainingData$Occupancy, family = "binomial", alpha = 0)

pred.ridge <- predict(ridge.model, xTest, type = "class")

ridge.test.error <- mean(pred.ridge != testingData$Occupancy)
