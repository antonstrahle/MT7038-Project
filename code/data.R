library(tidyverse)
library(e1071)
library(ggpubr)
library(readxl)
library(scales)
library(rpart)
library(rpart.plot)
library(caret)

#occupancy Dataset

d1 <- read.delim("../data/datatraining.txt", sep = ",")
d2 <- read.delim("../data/datatest.txt", sep = ",")
d3 <- read.delim("../data/datatest2.txt", sep = ",")

occupancyData <- d1 %>% 
  rbind(d2) %>% 
  rbind(d3) %>% 
  select(-date) %>% 
  mutate(Occupancy = factor(Occupancy)) %>% 
  select(-Light)

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

train <- balancedSplit(occupancyData$Occupancy, size = 0.6)

rawTrainingData <- occupancyData[train,]  
remainingData <- occupancyData[!train,]

validation <- balancedSplit(remainingData$Occupancy, 0.5)

rawValidationData <- remainingData[validation,]
rawTestingData <- remainingData[!validation,]

standardizeData <- function(data, rawTrain = rawTrainingData){
  
  attr <- rawTrain[,!sapply(rawTrain, is.factor)]
  
  mean <- apply(attr, 2, mean)
  sd <- apply(attr, 2, sd)
  
  data.frame(Occupancy = data[,sapply(rawTrain, is.factor)]) %>% 
    cbind(t((t(data[,!sapply(rawTrain, is.factor)]) - mean)/sd))
  
}

#Standardized separately, should be the same mean and sd for all data
trainingData <- standardizeData(rawTrainingData)
validationData <- standardizeData(rawValidationData)
testingData <- standardizeData(rawTestingData)

#Looks very nice id say

trainingData %>% 
  gather(key = "Variable", value = "Value", -Occupancy) %>% 
  ggplot(aes(x = Occupancy, y = Value, color = Occupancy)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y")
