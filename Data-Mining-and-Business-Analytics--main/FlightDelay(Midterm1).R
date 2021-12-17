## Import packages

library(rpart.plot)
library(caret)
library(gmodels) # for CrossTable
library(ggplot2)

##############################################################
####################### BACKGROUND ##########################
#############################################################

## Goal: Predicting Delayed Flights. 

##The file FlightDelays.csv contains information on all commercial flights 
##departing the Washington, DC area and arriving at New York during January 2004.
##For each flight, there is information on the departure and arrival airports,
##the distance of the route, the scheduled time and date of the flight, and so 
##on. The variable that we are trying to predict is whether or not a flight is
##delayed. A delay is defined as an arrival that is at least 15 minutes later 
##than scheduled.


## Q1: Reading data 

data<-read.csv("/Users/joonghochoi/Desktop/FlightDelays.csv")

## Q2: Create a dataframe (name it as delays.df) with selective columns:
##   'CARRIER','DEST','DISTANCE','ORIGIN', 'Weather','DAY_WEEK','Flight.Status'

delays.df<-data.frame(CARRIER=data$CARRIER,DEST=data$DEST,DISTANCE=data$DISTANCE,
                      ORIGIN=data$ORIGIN,Weather=data$Weather,DAY_WEEK=data$DAY_WEEK,
                      Flight.Status=data$Flight.Status)

#############################################################
###        Data preprocessing and exploration           #####
#############################################################

## Q3: Force all the variables (except for “DISTANCE”) to be factors
## Transform some variables into categorical variables.

delays.df$CARRIER<-as.factor(delays.df$CARRIER)
delays.df$DEST<-as.factor(delays.df$DEST)
delays.df$ORIGIN<-as.factor(delays.df$ORIGIN)
delays.df$Weather<-as.factor(delays.df$Weather)
delays.df$DAY_WEEK<-as.factor(delays.df$DAY_WEEK)
delays.df$Flight.Status<-as.factor(delays.df$Flight.Status)


## Q4a: Create a side-by-side boxplot to describe the DISTANCE’s distribution 
#       with different CARRIERs. 
# boxplot: 
library(ggplot2)
ggplot(delays.df,aes(y=delays.df$DISTANCE,x=delays.df$CARRIER))+geom_boxplot()

## Q4b: Use CrossTable to learn how Flight.Status is related with different Weather
# CrossTable
library("gmodels")
CrossTable(delays.df$Weather,delays.df$Flight.Status,chisq=T)



## Q5: Partition the data into training (60%) and testing sets (40%).

train_rows<-sample(row.names(delays.df),dim(delays.df)[1]*0.6)
test_rows<-setdiff(row.names(delays.df),train_rows)

train_data<-delays.df[train_rows,]
test_data<-delays.df[test_rows,]




##################################################################
#######      Fit a classification tree          ################## 
##################################################################

## Q6: Fit a classification tree to the flight delay variable ('Flight.Status') using all the predictors. Note:
# •	Set seed as 1 using set.seed(1)
# •	Set method as “rpart”
# •	Use 5 fold cross-validation
# •	Use tuneLength = 30
library("caret")
library("rpart.plot")
library("ggplot2")
library("entropy")
set.seed(1)
tree.fit1 <- train(Flight.Status ~ ., data=delays.df, 
                   method = 'rpart', 
                   trControl=trainControl(method = 'cv', number=5),
                   tuneLength = 30,
                   parms=list(split='information'),
)




## Q7a:  plots the accuracies: what is the best cp?
plot(tree.fit1)
#Any value between 0.01 and 0.06 seems to be equally good cp 

## Q7b:  plot the final model using rpart


rpart.plot(tree.fit1$finalModel)

## Q8:  Use this tree to predict the Y labels in the testing set.
# And evaluate the model performance on the testing set using confusion matrix. 

pred_classes <- predict(tree.fit1, newdata = test_data)
test_data$Flight.Status = as.factor(test_data$Flight.Status)
confusionMatrix(data = pred_classes, test_data$Flight.Status)



