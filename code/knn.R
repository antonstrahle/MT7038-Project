source("data.R")
library(class)

valKNN <- function(K, train = trainingData, val = validationData){
  
  l <- rep(list(c(NA,NA)), length(K))
  
  i = 1
  
  for(k in K){
  
    m <- knn(train[, !sapply(train, is.factor)], val[, !sapply(val, is.factor)], cl = train[,sapply(train, is.factor)], k = k) 
    
    valError <- mean(val[, sapply(val, is.factor)] != m)
    
    print(paste("Validation Error:", round(valError, digits = 3), "for k =", k))
    
    l[[i]] <- c(k, valError)
  
    i = i + 1
    
  }
  
  data.frame(matrix(unlist(l), ncol = 2, byrow = T)) %>% 
    setNames(c("k", "error"))
  
}

