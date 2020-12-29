source("data.R")
library(class)

valKNN <- function(K, train = trainingData, val = validationData){
  
  bestK <- 0
  bestError <- 1
  
  for(k in K){
  
    m <- knn(train[, !sapply(train, is.factor)], val[, !sapply(val, is.factor)], cl = train[,sapply(train, is.factor)], k = k) 
    
    valError <- mean(val[, sapply(val, is.factor)] != m)
    
    print(paste("Validation Error:", round(valError, digits = 3), "for k =", k))
    
    if(valError < bestError){
      
      bestK <- k
      
    }
  
  }
  
  bestK
    
}

