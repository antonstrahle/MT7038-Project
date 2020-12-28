source("anton.R")

#SVM

valLinearSVM <- function(sequence, train = trainingData, val = validationData){
  
  bestCost <- 0
  bestValError <- 1
  
  for(C in sequence){
    
    m <- svm(Occupancy ~ ., data = train, kernel = "linear", cost = C)
    pred <- predict(m, val)
    
    valError <- mean(pred != val$Occupancy)
    
    #print(paste("Training Error:", round(mean(m$fitted != train$Occupancy), 4), "for C:", C)) 
    print(paste("Validation Error:", round(valError, 4), "for C:", C)) 
    
    if(valError < bestValError){
      
      bestValError <- valError
      bestCost <- C
      
    }
    
  }
  
  bestCost
  
}

valRadialSVM <- function(sequence, train = trainingData, val = validationData){
  
  bestCost <- 0
  bestValError <- 1
  
  for(C in sequence){
    
    m <- svm(Occupancy ~ ., data = train, kernel = "radial", cost = C)
    pred <- predict(m, val)
    
    valError <- mean(pred != val$Occupancy)
    
    #print(paste("Training Error:", round(mean(m$fitted != train$Occupancy), 4), "for C:", C)) 
    print(paste("Validation Error:", round(valError, 4), "for C:", C)) 
    
    if(valError < bestValError){
      
      bestValError <- valError
      bestCost <- C
      
    }
    
  }
  
  bestCost
  
}

valPolynomialSVM <- function(sequence, deg, coeff, train = trainingData, val = validationData){
  
  bestCost <- 0
  bestValError <- 1
  
  for(C in sequence){
    
    m <- svm(Occupancy ~ ., data = train, kernel = "polynomial", cost = C, degree = deg, coef0 = coeff)
    pred <- predict(m, val)
    
    valError <- mean(pred != val$Occupancy)
    
    #print(paste("Training Error:", round(mean(m$fitted != train$Occupancy), 4), "for C:", C)) 
    print(paste("Validation Error:", round(valError, 4), "for C:", C)) 
    
    if(valError < bestValError){
      
      bestValError <- valError
      bestCost <- C
      
    }
    
  }
  
  bestCost
  
}

#Radial is king, polt deg 5 is fine but times out. 0.06-0.07 with Radial and best C











