source("data.R")
#########################################################
#logit model

log.model <- glm(Occupancy ~ ., data = trainingData, family = binomial(link = "logit"))
summary(log.model)


train.error <- mean(sqrt((log.model$fitted - as.numeric(as.character(trainingData$Occupancy)))^2))


#Pairsplot colored by CLASS
cols <- character(nrow(trainingData))

cols[trainingData$Occupancy == 1] <- "deepskyblue1"
cols[trainingData$Occupancy == 0] <- "coral2"
pairs(trainingData[,-1], col= alpha(cols, 0.1))






