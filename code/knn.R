source("data.R")
library(class)

valKNN <- function(k, train = trainingData, val = validationData){
  
  m <- knn(train[, !sapply(train, is.factor)], val[, !sapply(val, is.factor)], cl = train[,sapply(train, is.factor)], k = k) 
  
  table <- table(m, val[,sapply(val, is.factor)])
  
  print(table)
  
  print(paste("Validation Error:", round(1-sum(diag(table))/sum(table), digit = 3), "for k =", k))
  
}

for(k in 1:50){
  
  valKNN(k)
  
}

valKNN(k = 1, val = testingData)

#1-nn seems to be working very well due to lack of noise in the data