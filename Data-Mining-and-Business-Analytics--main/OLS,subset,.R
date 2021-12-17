## Goal: predict the price of the car using other features of the car

# Read in data
car.df <- read.csv("ToyotaCorolla.csv")

## ---------------- Step 0: data exploration ---------------#

# select the following variables for regression: 3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18
selected_var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
car.df <-car.df[, selected_var]


## ---------------- Step 1: data partition: training (60%), testing (40%)-------#

set.seed(1)

train.index <- sample(row.names(car.df), dim(car.df)[1]*0.6)
train.df <- car.df[train.index,]

test.index <- setdiff(row.names(car.df), train.index)
test.df <- car.df[test.index,]

#------------------------------------------------#
## ----------------   Step 2A: OLS -------------#
#------------------------------------------------#
## explore relationship between x and y
library(ggplot2)

ggplot(data=train.df, aes(x=Price,y=KM))+
  geom_point()

plot(train.df)
## linear regression
# use lm() to run a linear regression of Price on all predictors in the
# training set. 
car_lm_1 <-lm(Price~KM ,data = train.df)
summary(car_lm_1)

car_lm_2 <-lm(Price~KM+ HP,data = train.df)
summary(car_lm_2)

# Use . after ~ to include all the remaining columns in train.df as predictors.
car_lm <-lm(Price~.,data = train.df)
summary(car_lm)


## Prediction in testing set and training set
# in testing set
car_pred_test <- predict(car_lm,test.df)
# in training set
car_pred_train <- predict(car_lm,train.df)
# car_pred_train2 <- car_lm$fitted.values

## Assess the performance in training/testing set
library(forecast)

# in testing set
accuracy(car_pred_test,test.df$Price)
rmse_ols_test <- sqrt(mean((test.df$Price-car_pred_test)^2)) #manually calculate rmse

# in training set
accuracy(car_lm$fitted.values, train.df$Price)
accuracy(car_pred_train,train.df$Price)
rmse_ols_train <- sqrt(mean((train.df$Price-car_pred_train)^2)) #manually calculate rmse

#-----------------------------------------------------------------------------#
## ---------------------  Step 2B: Subset selection --------------------------#
#-----------------------------------------------------------------------------#
# use regsubsets() in package leaps to run an exhaustive search. 

library(leaps)

exh_search <- regsubsets(Price~., data = train.df, nvmax = 11, method = "exhaustive")
## NOTES:
# 1. nvmax: maximum size of subsets to examine
# 2. method: "exhaustive","backward", "forward", "seqrep".  
#           Use exhaustive search, forward selection, backward selection 
#           or sequential replacement to search.


## summary results
sumry_exh <-summary(exh_search)

# show models
sumry_exh$which 
# The r-squared for each model
sumry_exh$rsq
# Adjusted r-squared
sumry_exh$adjr2
# BIC
sumry_exh$bic
# A version of the which component that is formatted for printing
sumry_exh$outmat
# show metrics
data.frame(rsq = sumry_exh$rsq, adjr2 = sumry_exh$adjr2, BIC = sumry_exh$bic, sumry_exh$outmat)

## alternative searching sequence.
bwd_search <- regsubsets(Price~., data = train.df, method = "backward")
fwd_search <- regsubsets(Price~., data = train.df, method = "forward")
both_search <- regsubsets(Price~., data = train.df, method = "seqrep")

##---------[advanced contents (not required for midterm)] ------------------#
##---------         Step 3: Assess Model Performance                  -----##
##--------------------------------------------------------------------------#
## prediction using the regsubset models
# Unfortunately, regsubset is not compatible with the popular predict() function
# you could try using: predict(exh_search,test.df), which will report errors
x_test <- model.matrix(Price ~ ., data = test.df )
# NOTES: model.matrix creates a design (or model) matrix, e.g., by expanding  
# factors to a set of dummy variables (depending on the contrasts) and 
# expanding interactions similarly.

## prediction based on one model 
coef <- coef(exh_search, id = 8) #This will extract coef from model number x
yhat_subset <- x_test[,names(coef)] %*% coef # %*% is matrix multiplication. 
accuracy(yhat_subset[,1],test.df$Price)
rmse_subset_test <- sqrt(mean((test.df$Price-yhat_subset)^2)) #manually calculate rmse

## prediction based on all models in exh_search
rmse_subsets_test <- rep(NA,11) #set up a container
for (i in 1:11){
  coef <- coef(exh_search, id = i) 
  yhat_subset <- x_test[,names(coef)] %*% coef 
  rmse_subsets_test[i] <- sqrt(mean((test.df$Price-yhat_subset)^2))
}

## Visualize 
library(ggplot2)
visualize <- data.frame(rmse_subsets_test, model = (1:11))

ggplot(visualize, aes(x = model, y = rmse_subsets_test)) +
  geom_line() +
  geom_point(color = "red", size = 3) +
  ylab("RMSE") +
  xlab("Model Number") +
  ggtitle("RMSE on Testing Set")

