library(tidyverse)
library(e1071)
library(ggpubr)
library(readxl)

#RICE DATA SET. Data at https://www.muratkoklu.com/datasets/ as the link to the data at 
#https://archive.ics.uci.edu/ml/datasets/Rice+%28Cammeo+and+Osmancik%29 has no download

#"Citation Request :
#CINAR, I. and KOKLU, M., (2019). “Classification of Rice Varieties Using Artificial Intelligence Methods.” International Journal of Intelligent Systems and Applications in Engineering, 7(3), 188-194.
#DOI: https://doi.org/10.18201/ijisae.2019355381"


rice <- read_xlsx("data/Rice_Osmancik_Cammeo_Dataset.xlsx") %>% 
  mutate(CLASS = factor(CLASS))

summary(rice)

rice %>% 
  gather(key = "Variable", value = "Value", -CLASS) %>% 
  ggplot(aes(x = CLASS, y = Value, color = CLASS)) +
    geom_boxplot() +
    facet_wrap(~Variable, scales = "free_y")

#From CrossValidate package (issues with dependencies in the install so I yoinked their source code)
balancedSplit <- function(fac, size){
  trainer <- rep(FALSE, length(fac))
  for(lev in levels(fac)){
    N <- sum(fac==lev)
    wanted <- max(1, trunc(N*size))
    trainer[fac==lev][sample(N, wanted)] <- TRUE
  }
  trainer
}

train <- balancedSplit(rice$CLASS, size = 0.6)

rawTrainingData <- rice[train,]  
remainingData <- rice[!train,]

validation <- balancedSplit(remainingData$CLASS, 0.5)

rawValidationData <- remainingData[validation,]
rawTestingData <- remainingData[!validation,]

standardizeData <- function(data, rawTrain = rawTrainingData){
  
  attr <- rawTrain[,!sapply(rawTrain, is.factor)]
  
  means <- apply(attr, 2, mean)
  sd <- apply(attr, 2, sd)
  
  cbind(data[,sapply(rawTrain, is.factor)], t((t(data[,!sapply(rawTrain, is.factor)]) - means)/sd))
  
}

trainingData <- standardizeData(rawTrainingData)
validationData <- standardizeData(rawValidationData)
testingData <- standardizeData(rawTestingData)

#Looks very nice id say

trainingData %>% 
  gather(key = "Variable", value = "Value", -CLASS) %>% 
  ggplot(aes(x = CLASS, y = Value, color = CLASS)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y")


crossValLinearSVM <- function(sequence, train = trainingData, val = validationData){
  
  bestCost <- 0
  bestValError <- 1
  
  for(C in sequence){
    
    m <- svm(CLASS ~ ., data = train, kernel = "linear", cost = C)
    pred <- predict(m, val)
    
    valError <- mean(pred != val$CLASS)
    
    #print(paste("Training Error:", round(mean(m$fitted != train$CLASS), 4), "for C:", C)) 
    print(paste("Validation Error:", round(valError, 4), "for C:", C)) 
    
    if(valError < bestValError){
      
      bestValError <- valError
      bestCost <- C
      
    }
  
  }
  
  bestCost
  
}


bestC <- crossValLinearSVM(seq(0.01, 1, by = 0.05))

bestM <- svm(CLASS ~ ., data = trainingData, kernel = "linear", cost = bestC)
pred <- predict(bestM, testingData)

testError <- mean(pred != testingData$CLASS)












