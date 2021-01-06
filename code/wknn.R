source("data.r")
library(kknn)

valWKNN <- function(K, kernel = "gaussian", train = trainingData, val = validationData){

  l <- rep(list(c(NA,NA)), length(K))
  
  i = 1
   
  for(k in K){
    
    m <- kknn(Occupancy ~ ., train = train, test = val, kernel = kernel, k = k) 
    
    valError <- mean(val[, sapply(val, is.factor)] != m$fitted.values)
    
    print(paste("Validation Error:", round(valError, digits = 3), "for k =", k))
    
    l[[i]] <- c(k, valError)
    
    i = i + 1
    
  }
  
  data.frame(matrix(unlist(l), ncol = 2, byrow = T)) %>% 
    setNames(c("k", "error"))
  
}

