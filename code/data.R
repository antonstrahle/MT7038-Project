library(tidyverse)
library(e1071)
library(ggpubr)
library(readxl)
library(scales)
library(rpart)
library(rpart.plot)

#occupancy Dataset

rawTrainingData <- read.delim("../data/datatraining.txt", sep = ",") %>% 
  select(-date)%>% 
  mutate(Occupancy = factor(Occupancy)) %>% 
  select(-Light)

rawValidationData <- read.delim("../data/datatest2.txt", sep = ",") %>% 
  select(-date) %>% 
  mutate(Occupancy = factor(Occupancy)) %>% 
  select(-Light)

rawTestingData <- read.delim("../data/datatest.txt", sep = ",") %>% 
  select(-date) %>% 
  mutate(Occupancy = factor(Occupancy)) %>% 
  select(-Light)

upsampling <- function(data){
  
  ones <- sum(data$Occupancy == 1)
  zeros <- sum(data$Occupancy == 0)
  
  dif <- zeros - ones
  
  if(dif > 0){
    
    toAdd <- data %>% 
      filter(Occupancy == 1) %>% 
      sample_n(dif, replace = TRUE)
    
  }else{
    
    toAdd <- data %>% 
      filter(Occupancy == 0) %>% 
      sample_n(dif, replace = TRUE)
    
  }
  
  rbind(data, toAdd)
  
}

standardizeData <- function(data, rawTrain = upsampledTrainingData){
  
  attr <- rawTrain[,!sapply(rawTrain, is.factor)]
  
  mean <- apply(attr, 2, mean)
  sd <- apply(attr, 2, sd)
  
  data.frame(Occupancy = data[,sapply(rawTrain, is.factor)]) %>% 
    cbind(t((t(data[,!sapply(rawTrain, is.factor)]) - mean)/sd))
  
}

#Standardized separately, should be the same mean and sd for all data
trainingData <- upsampling(standardizeData(rawTrainingData))
validationData <- standardizeData(rawValidationData)
testingData <- standardizeData(rawTestingData)

