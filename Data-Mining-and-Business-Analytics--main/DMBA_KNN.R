#### Background
# A riding mower manufacturer would like to find a way of classifying families in 
# a city into those likely to purchase a riding mower and those not likely to purchase one. 


#### Load file RidingMowers.csv
mower_df <- read.csv("RidingMowers.csv")
mower_df$Ownership = as.factor(mower_df$Ownership)

# New data Income = 60, Lot_Size = 20
new_df <- data.frame(Income = 60, Lot_Size = 20)


## Plot the above data
## Show new data point in the above plot
library(ggplot2)

ggplot(mower_df, aes(x=Income, y=Lot_Size)) +
  geom_point(aes(shape = factor(Ownership), colour = factor(Ownership)), size = 2) + 
  geom_point(data = new_df, 
             mapping = aes(x=Income, y=Lot_Size),
             shape = '?',
             size = 3)
#modify shape . ex.shape=18

## Normalize mower_df and new_df
# Scale new data
new_df$Income <- (new_df$Income-mean(mower_df$Income))/sd(mower_df$Income)
new_df$Lot_Size <- (new_df$Lot_Size-mean(mower_df$Lot_Size))/sd(mower_df$Lot_Size)

# Scale original data
mower_df$Income <- (mower_df$Income-mean(mower_df$Income))/sd(mower_df$Income)
mower_df$Lot_Size <- (mower_df$Lot_Size-mean(mower_df$Lot_Size))/sd(mower_df$Lot_Size)

###------------------------------------------------------------------------##
###     Example 1:  Build knn model using "knn" function of FNN library    ##
###------------------------------------------------------------------------##

## OPTION 1: USING "knn" function
library(FNN)

knn.pred <- knn(train = mower_df[,c(1,2)], test = new_df, cl=mower_df[,3], k=3)
# NOTES: 'cl': factor of true classifications of training set.


## OPTION 2: USING "caret" function
library(caret)

# Build knn model using "train" function of caret package
knn_train <-train(Ownership~., data = mower_df, method = 'knn')

# Predict using the model
predict(knn_train, new_df)

###------------------------------------------------------------------------##
###         [BREAKOUT PRACTICE] Example 2: Choosing k                      ##
###------------------------------------------------------------------------##
## (a). partition the data into train (60%) and validation (40%) sets
## Note that we are skipping the normalization step, since we have done that already.
set.seed(11)
train.index <- sample(row.names(mower_df), 0.6*dim(mower_df)[1])
valid.index <- setdiff(row.names(mower_df), train.index)
train.norm.df = mower_df[train.index,]
valid.norm.df = mower_df[valid.index,]


## (b). Try 3-NN with training set being train.norm.df, testing set being valid.norm.df
knn.pred <- knn(train.norm.df[,c(1,2)], test = valid.norm.df[,c(1,2)], 
                cl = train.norm.df[,3], k=3)

confusionMatrix(knn.pred,valid.norm.df[,3])

confusionMatrix(knn.pred,valid.norm.df[,3])$overall[1]

## (c) Compute knn accuracy for different k (k=1,2,…,12) on validation set
# initialize a data frame with two columns: k and accuracy
accuracy.df <- data.frame(k= seq(1,12,1), accuracy = rep(0,12))

# start comparing accuracies under different k
# tip: inside the loop, the codes are similar to what we did under (b)
for(i in 1:12){
  knn.pred <- knn(train.norm.df[,c(1,2)], test = valid.norm.df[,c(1,2)], 
                  cl = train.norm.df[,3], k=i)
  accuracy.df[i,2] = confusionMatrix(knn.pred,valid.norm.df[,3])$overall[1]
}


## WE CAN CHOOSE K =8
## NOTE: now the validation set is used as part of the training process (to set k),
#       it will not reflect a true holdout set as before.
#       Ideally, we would need a third set (testing set) to compute the unbiased 
#       model performance. 

###------------------------------------------------------------------------##
##  [ OPTIONAL ]  Example 3: DIY Cross-valiation in KNN function           ##
###------------------------------------------------------------------------##
## partition the data into train and validation sets
## Note that we are skipping the normalization step, since we have done that already.
# 
library(splitTools)
 
set.seed(1)

folds <- create_folds(mower_df$Ownership, k = 3)

# initialize a data frame with two columns: k and accuracy
avg_accuracy.df <- data.frame(k= seq(1,12,1), avg_accuracy = rep(0,12))

for(i in 1:12){
  accuracy.df <- data.frame(fold= seq(1,5,1), accuracy = rep(0,5))
  fold_n = 1 
  for (f in folds){
    knn.pred <- knn(mower_df[f,c(1,2)], test = mower_df[-f,c(1,2)], 
                    cl = mower_df[f,3], k=i)
    accuracy.df[fold_n,2] = confusionMatrix(knn.pred,mower_df[-f,3])$overall[1]
    fold_n = fold_n + 1
  }
  avg_accuracy.df[i,2] = mean(accuracy.df$accuracy)
}


###------------------------------------------------------------------------##
###           Example 4: KNN with Cross-validation Using Caret             ##
###------------------------------------------------------------------------##

##Installing and Loading the "caret" library. It includes the functions to implement knn algorithm
#install.packages("caret")
library(caret)

## Restart from the original data
mower_df <- read.csv("RidingMowers.csv")
mower_df$Ownership = as.factor(mower_df$Ownership)

# New data Income = 60, Lot_Size = 20
new_df <- data.frame(Income = 60, Lot_Size = 20)

#The seed function is used pseudo-random number generation.The seed number you 
#    choose is the starting point used in the generation of a sequence of random numbers, 
#    which is why (provided you use the same pseudo-random number generator) 
#    you'll obtain the same results given the same seed number.
set.seed(100)

knn_mower <- train(Ownership ~ ., data=mower_df, 
                   method = "knn",
                   preProcess = c("center", "scale"), 
                   tuneGrid=expand.grid(k=1:12),
                   trControl = trainControl(method = "cv", number=5))

##NOTES:
#  1. data defines the input data, 
#  2. 'method' defines the type of classification or regression model used, 
#  3. 'preprocess'  specifies to center and scale the predictors, 
#  4. 'tuneGrid' defines the possible tuning values , in this case is the k values between 1 to 12
#  5. 'trConrtol' defines the resampling method, here we are using 5-fold crosss validation, 

##ploting the accuracies of K nearest neighbor implemented for k=1 to 12
plot(knn_mower)
knn_mower

#generating the confusion matrix for the k nearest neighbor implemented with k having the highest accuracy
knnPredict <- predict(knn_mower,new_df)


