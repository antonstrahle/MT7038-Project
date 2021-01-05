source("data.r")
library(kknn)

valWKNN <- function(K, kernel = "epanechnikov", train = trainingData, val = validationData){
  
  bestK <- 0
  bestError <- 1
  
  for(k in K){
    
    m <- kknn(Occupancy ~ ., train = train, test = val, kernel = kernel, k = k) 
    
    valError <- mean(val[, sapply(val, is.factor)] != m$fitted.values)
    
    print(paste("Validation Error:", round(valError, digits = 3), "for k =", k))
    
    if(valError < bestError){
      
      bestK <- k
      bestError <- valError
      
    }
    
  }
  
  bestK
  
}
