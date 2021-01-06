source("data.R")

#SVM

valLinearSVM <- function(sequence, train = trainingData, val = validationData){
  
  l <- rep(list(c(NA,NA)), length(sequence))
  
  i = 1
  
  for(C in sequence){
    
    m <- svm(Occupancy ~ ., data = train, kernel = "linear", cost = C)
    pred <- predict(m, val)
    
    valError <- mean(pred != val$Occupancy)
    
    #print(paste("Training Error:", round(mean(m$fitted != train$Occupancy), 4), "for C:", C)) 
    print(paste("Validation Error:", round(valError, 4), "for C:", C)) 
    
    l[[i]] <- c(C, valError)
    
    i = i + 1
    
  }
  
  data.frame(matrix(unlist(l), ncol = 2, byrow = T)) %>% 
    setNames(c("c", "error"))
  
}

valRadialSVM <- function(sequence, train = trainingData, val = validationData){
  
  l <- rep(list(c(NA,NA)), length(sequence))
  
  i = 1
  
  for(C in sequence){
    
    m <- svm(Occupancy ~ ., data = train, kernel = "radial", cost = C)
    pred <- predict(m, val)
    
    valError <- mean(pred != val$Occupancy)
    
    #print(paste("Training Error:", round(mean(m$fitted != train$Occupancy), 4), "for C:", C)) 
    print(paste("Validation Error:", round(valError, 4), "for C:", C)) 
    
    l[[i]] <- c(C, valError)
    
    i = i + 1
    
  }
  
  data.frame(matrix(unlist(l), ncol = 2, byrow = T)) %>% 
    setNames(c("c", "error"))
  
}

valPolynomialSVM <- function(C, deg, coeff, train = trainingData, val = validationData){
  
  l <- rep(list(c(NA,NA,NA)), length(C)*length(deg))
  
  i = 1
  
  for(d in deg){  
    
    for(c in C){
    
      m <- svm(Occupancy ~ ., data = train, kernel = "polynomial", cost = c, degree = d, coef0 = coeff)
      pred <- predict(m, val)
      
      valError <- mean(pred != val$Occupancy)
      
      #print(paste("Training Error:", round(mean(m$fitted != train$Occupancy), 4), "for C:", c)) 
      print(paste("Validation Error:", round(valError, 4), "for C:", c, "and D:", d)) 
      
      l[[i]] <- c(c, d, valError)
      
      i = i + 1

    }
      
  }
  
  data.frame(matrix(unlist(l), ncol = 3, byrow = T)) %>% 
  setNames(c("c", "d", "error"))

}












