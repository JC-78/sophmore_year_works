
######################################################################### 
############  EXAMPLE (1) with UniversalBank.csv (classification problem) 
######################################################################### 

#### The task: Universal bank is a small but growing bank. The majority of customers 
# are deposit only customers, and the bank is interested in converting more of them 
# into borrowing, customers. The bank has had past success with marketing compaigns 
# to convert the liability customers to personal loan customers. With that success, 
# the bank is interested in being more targeted with its efforts to ensure marketing 
# dollars are spent on those most likely to convert. The bank would like a model to 
# help it predict which new customers are likely to become personal loan customers 
# to aid in this targeting.

#### Business Question
# Can we predict which new customers are most likely to say yes to a personal loan?


#### Load file UniversalBank.csv
bank<-read.csv("UniversalBank.csv")

# Drop ID and zip code columns.
bank$ID<-NULL
bank$ZIP.Code<-NULL

# Convert education level 1,2,3 to "Undergrad", "Graduate", "Advanced" respectively.
bank$Education<-factor(bank$Education, levels = c(1,2,3), labels = c("Undergrad", "Graduate", "Advanced"))

# use set.seed() to get the same partitions when re-running the R code
set.seed(1)

####------------- partition data in training (60%) and test data (40%)
train_rows<-sample(row.names(bank), dim(bank)[1]*0.6)
test_rows<-setdiff(row.names(bank),train_rows)
train_data<-bank[train_rows,] 
test_data <-bank[test_rows,]

# run logistic regression to predict "Personal.Loan"
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logistic_loan <- glm(Personal.Loan~.,data = train_data, family = "binomial")
summary(logistic_loan)


#### Predict using logistic regression

# Predict and obtain probability 
# options(scipen = 999) # To turn off the scientific notation
pred <-predict(logistic_loan,test_data, type = "response")


# create a dataframe to compare the prediction score and actual loan outcome
df_compare = data.frame(pred, test_data$Personal.Loan)

colnames(df_compare) = c("pred_score","actual_outcome")

df_compare$pred_outcome_09 = as.factor(
  ifelse(df_compare$pred_score>0.9,1,0)
)


###[after-class practice ]------------ confusion matrix ------------------------###
#### load the following packages: caret
library(caret)
library(e1071)

# turn actual_outcome into a factor with two levels: accept (level=1), reject (level=0)
df_compare$actual_outcome = factor(df_compare$actual_outcome, levels = c(1,0), labels = c("accept","reject"))

# Use the pred_score and cutoffs of 0.9, 0.98, 0.5, 0.3 to predict accept (Prob>cutoff is accept)
owner_pred_098 <-as.factor(ifelse(df_compare$pred_score>0.98,"accept","reject"))
owner_pred_09 <-as.factor(ifelse(df_compare$pred_score>0.9,"accept","reject"))
owner_pred_05 <-as.factor(ifelse(df_compare$pred_score>0.5,"accept","reject"))
owner_pred_03 <-as.factor(ifelse(df_compare$pred_score>0.3,"accept","reject"))

# Based on the predicted outcome, compute the confusion matrix based on different cutoffs. 
# Note: Argument order: (predicted, actual)
confusionMatrix(owner_pred_098, df_compare$actual_outcome, positive = "accept")
confusionMatrix(owner_pred_09, df_compare$actual_outcome, positive = "accept")
confusionMatrix(owner_pred_05, df_compare$actual_outcome, positive = "accept")
confusionMatrix(owner_pred_03, df_compare$actual_outcome, positive = "accept")

###[after-class practice ]----------- ROC Curves ------------------------###
# Load this package when sometimes roc is not found 
library(pROC) 

# compute roc based on the actual loan outcome and predicted scores
roc_val <- roc(response = df_compare$actual_outcome, predictor = df_compare$pred_score,
               levels = c("reject",'accept'), direction = "<")
plot.roc(roc_val)

## Notes: 
#Setting levels: control = 0, case = 1
# Setting direction: controls < cases
#   “>”: if the predictor values for the control group are higher than the values of the case group (controls > t >= cases). 
#   “<”: if the predictor values for the control group are lower or equal than the values of the case group (controls < t <= cases). 

# compute the AUC
auc(roc_val)

######################################################################### 
############  EXAMPLE (2) with ownerExample.csv (classification problem) 
######################################################################### 

#### load the following packages: caret
library(caret)
library(e1071)

# Load data: ownerExample.csv
owner_df <-read.csv("ownerExample.csv")
owner_df$Class <- as.factor(owner_df$Class)

# [in-class practice] Use the owner data and a cutoff of 0.75 to predict owner (Prob>0.75 is owner)
owner_pred_075 <-as.factor(ifelse(owner_df$Probability>0.75,"owner","nonowner"))
owner_pred_025 <-as.factor(ifelse(owner_df$Probability>0.25,"owner","nonowner"))
owner_pred_05 <-as.factor(ifelse(owner_df$Probability>0.5,"owner","nonowner"))

###---------------------- confusion matrix ------------------------###

# convert "Class" into a factor
owner_df$Class = as.factor(owner_df$Class)

# [in-class practice] Get consusion matrix for cutoffs: 0.25, 0.5, 0.75
# Note: Argument order: (predicted, actual)
confusionMatrix(owner_pred_075, owner_df$Class, positive = "owner")
confusionMatrix(owner_pred_05, owner_df$Class, positive = "owner")
confusionMatrix(owner_pred_025, owner_df$Class, positive = "owner")


###---------------------- ROC Curves ------------------------###
library(pROC) # Load this package when sometimes roc is not found 

# [in-class practice] Get roc values. (actual_class, probability)
roc_val <- roc(response = owner_df$Class, predictor = owner_df$Probability,
               levels = c("nonowner",'owner'), direction = "<")

## Notes:
# 1. Setting levels: control = 0, case = 1
# 2. Setting direction: controls < cases
#    “>”: if the predictor values for the control group are higher than 
#         the values of the case group (controls > t >= cases). 
#    “<”: if the predictor values for the control group are lower or equal than 
#         the values of the case group (controls < t <= cases). 

# [in-class practice] Plot roc curve
plot.roc(roc_val)

# [in-class practice] Get AUC
auc(roc_val)

#The x-axis for ROC plot is 1-Specificity, which is why it is ranging from 1-0. 
#e. But the AUC number should report the exact size of "the area under" the ROC curve.
