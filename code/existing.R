library(tidyverse)
library(e1071)
library(ggpubr)
library(readxl)
library(scales)
library(rpart)
library(rpart.plot)

#occupancy Dataset

d1 <- read.delim("../data/datatraining.txt", sep = ",")
d2 <- read.delim("../data/datatest.txt", sep = ",")
d3 <- read.delim("../data/datatest2.txt", sep = ",")

occupancyData <- d1 %>% 
  rbind(d2) %>% 
  rbind(d3) %>% 
  select(-date) %>% 
  mutate(Occupancy = factor(Occupancy))

occupancyData %>% 
  gather(key = "Variable", value = "Value", -Occupancy) %>% 
  ggplot(aes(x = Occupancy, y = Value, color = Occupancy)) +
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

#Linear SVM

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

#Feature Selection

pca <- prcomp(trainingData[,-1])
summary(pca)

#Lots of redundant features


#Tree
#Default tree results in one decision node
#Setting cp to say 0.001 does not improve classification

#####
#Code from Project 1
cv_fold <- function(data, n_fold){
  # fold_id denotes in which fold the observation
  # belongs to the test set
  data <- mutate(data, fold_id = rep_len(1:n_fold, length.out = n()))
  # Two functions to split data into train and test sets
  cv_train <- function(fold, data){
    filter(data, fold_id != fold) %>% 
      select(- fold_id)
  }
  cv_test <- function(fold, data){
    filter(data, fold_id == fold) %>% 
      select(- fold_id)
  }
  # Folding
  tibble(fold = 1:n_fold) %>% 
    mutate(train = map(fold, ~cv_train(.x, data)),
           test = map(fold, ~cv_test(.x, data)),
           fold = paste0("Fold", fold))
}

n <- 10
#as.double otw can't compute mse
occupancyBinary <- occupancy %>% mutate(CLASS = as.double(ifelse(CLASS == "Cammeo", 0, 1)))
cvData <- cv_fold(occupancyBinary, n_fold = n)

formula <- CLASS ~ .

#cp_seq <- seq(0, 50, length.out = 50)
cp_seq <- seq(0, 0.1, length.out = 100)

modeltree_df <- cvData %>% 
  # One row for each combination of lambda and fold
  crossing(cp = cp_seq) %>% 
  # Fit model to training data in each row
  mutate(model_fit = map2(train, cp, ~rpart(formula, .x, control = rpart.control(cp = .y)), method = class),
         # Compute predicted valuea on test data
         predicted = map2(test, model_fit, ~predict(.y, .x)),
         # Extract actual values from test data
         actual = map(test, ~(model.frame(formula, .x) %>% 
                                model.extract("response"))),
         # Compute mse 
         mse = map2_dbl(predicted, actual, ~mean((.x - .y)^2)))



modeltree_df %>%
  group_by(cp) %>% 
  summarise(mse = mean(mse)) %>% 
  ggplot(aes(x = cp, y = mse)) + 
  geom_point() + 
  geom_line()
######


bestCP <- modeltree_df$cp[which.min(modeltree_df$mse)]

#Seems like the best tree is just determined by MAJORAXIS value. Reasonable but simple (boring tree).
tree <- rpart(CLASS~., data = trainingData, method = "class")
rpart.plot(tree)
treepred <- predict(tree, testingData, type = "class")
table(testingData$CLASS, treepred)

treecp <- rpart(CLASS~., data = trainingData, method = "class", control = rpart.control(cp = bestCP))
rpart.plot(treecp)
treecppred <- predict(treecp, testingData, type = "class")
table(testingData$CLASS, treecppred)



#########################################################
#logit model
log.model <- glm(CLASS ~ ., data = trainingData, family = binomial(link = "logit"))
summary(log.model)

#Pairsplot colored by CLASS
cols <- character(nrow(trainingData))

cols[trainingData$CLASS == "Cammeo"] <- "deepskyblue1"
cols[trainingData$CLASS == "Osmancik"] <- "coral2"
pairs(trainingData[,-1], col= alpha(cols, 0.2))






