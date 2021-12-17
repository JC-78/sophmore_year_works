######################################################################
####################Loading the datasets##############################

##Loading the credit_data from the "credit_data.csv" file, converting unknown values to 'NA' and loadings the strings as factors
credit_data <- read.csv("credit_data.csv", stringsAsFactors = TRUE)


######################################################################
##############################Part B##################################

#####Question 1 and 2#####
##Installing and Loading the "caret" library. It includes the functions to implement knn algorithm
#install.packages("caret")
library(caret)

##default with '~' indicates it is a predictor,
##data defines the input data, method argument defines the type of classification or regression model used, 
##preprocess argument specifies to center and scale the predictors, tuneGrid defines the possible tuning values , in this cas is the k values between 1 to 50
##trConrtol parameter defines the resampling method, here we are using 10-fold crosss validation, 
##the seed function is used pseudo-random number generation.The seed number you choose is the starting point used in the generation of a sequence of random numbers, 
##which is why (provided you use the same pseudo-random number generator) you'll obtain the same results given the same seed number.
set.seed(100)
knn_credit <- train(default ~ ., data=credit_data, method = "knn",
              preProcess = c("center", "scale"), tuneGrid=expand.grid(k=1:50),
              trControl = trainControl(method = "cv", number=10))

##ploting the accuracies of K nearest neighbor implemented for k=1 to 50
plot(knn_credit)
knn_credit
which.max(knn_credit$results$Accuracy) 

#generating the confusion matrix for the k nearest neighbor implemented with k having the highest accuracy
knnPredict  <- predict(knn_credit,credit_data)
confusionMatrix(knnPredict, credit_data$default)


