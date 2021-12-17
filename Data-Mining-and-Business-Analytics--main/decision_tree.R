######################################################################### 
############  EXAMPLE (1) with titanic.csv (classification problem) 
######################################################################### 

##--------- install packages and library call----------------#
# install.packages("caret")
# install.packages("rpart.plot")
# install.packages("entropy")
# install.packages("gmodels")

library("caret")
library("rpart.plot")
library("ggplot2")
library("entropy")
library("gmodels") # for CrossTable

##------------------------ Import data -----------------------#
# Pclass: Passenger Class (1 = 1st; 2 = 2nd; 3 = 3rd)
# Survived: Survival (0 = No; 1 = Yes)
# Name: Name
# Sex
# Age
# SibSp: Number of Siblings/Spouses Aboard
# Parch: Number of Parents/Children Aboard
# Ticket: Ticket Number
# Fare: Passenger Fare (British pound)
# Cabin: Cabin
# Embarked: Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton)

titanic <- read.csv("titanic.csv")

#select variables to focus on
titanic <- titanic[,c("Pclass","Age","Sex","Survived")]

# see the structure of the data
str(titanic)

# summary statistics
summary(titanic)

#change the survived variable to factor since we are after classification. 
#       if you give numeric as the Y-var, the algorithm will try to do regression.
titanic$Survived <- as.factor(ifelse(titanic$Survived==0,"Died","Survived"))

# also change some other variables to factors
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)

#omit rows with missing values
titanic <- na.omit(titanic)


##--------[in-class practice] Explore data patterns -----------------------#
##  what might be related to Survived ? 

# barplot
ggplot(data=titanic, aes(x=Survived))+
  geom_bar(stat="count")

ggplot(data=titanic, aes(x=Pclass))+
  geom_bar(stat="count")

# boxplot: 
ggplot(data=titanic, aes(x=Survived, y = Age))+
  geom_boxplot()

# CrossTable
CrossTable(titanic$Survive, titanic$Sex)

#Cross Table output: similar to S-Plus crosstabs() 
#and SAS Proc Freq (or SPSS format) with Chi-square, Fisher and McNemar tests of the independence of all table factors.

##--------------  Partition data        -----------------------#
#for reproducibility 
set.seed(100)

tr_rows = sample(row.names(titanic),500)
ts_rows = setdiff(row.names(titanic),tr_rows)
titanic_tr = titanic[tr_rows,]
titanic_ts = titanic[ts_rows,]

##---------[in-class practice] Calculate entropy levels ---------------------#
## calculate the entropy for titanic$Sex
frequency_pct = table(titanic$Sex)/length(titanic$Sex)
frequency_pct
entropy.empirical(frequency_pct, unit="log2")
#0.3655462*log(0.3655462,base=2)+0.6344538*log(0.6344538, base=2)

## step1: define a function that calculates the entropy
shannonEntropy <-function(x){
          Col_ent<-entropy.empirical(table(x)/length(x), unit="log2")
          return(Col_ent)
}

## step2: apply function lets us perform column-wise or row-wise operation 
##   we pass the data set "titanic" and value "2" for column-wise operation 
##   followed by the function "shannonEntropy" 
all_entropy <- apply(titanic[,c("Pclass","Sex","Survived")],2,shannonEntropy)
all_entropy

##--------------  build trees using "rpart" ---------------------#
## R implementation of the CART algorithm is called RPART 
#   (Recursive Partitioning And Regression Trees)

#for reproducibility 
set.seed(555)

####  Train the model
tree.fit1 <- train(Survived ~ ., data=titanic_tr, 
                   method = 'rpart', 
                   trControl=trainControl(method = 'cv', number=7),
                   tuneLength = 20,
                   parms=list(split='information'),  # "gini"
                   #tuneGrid = data.frame(cp = c(0.01, 0.02,0.03,0.04,0.05)),
)
#should not do tunelength and tunegrid together. Tunelength

#(1) To change the candidate values of the tuning parameter, either of the tuneLength or tuneGrid arguments can be used. 
#      (1.a) tuneLength: The train function can generate a candidate set of parameter values and 
#                the tuneLength argument controls how many are evaluated.
#      (1.b) tuneGrid argument is used when specific values are desired
#(2) The splitting index can be gini or information. The default priors are proportional
#       to the data counts, the losses default to 1, and the split defaults to gini.

#### [in-class practice] plots the accuracies
plot(tree.fit1)

#### [in-class practice] draw the tree plot
plot(tree.fit1$finalModel)
text(tree.fit1$finalModel)

#### [in-class practice] more beautiful plots
rpart.plot(tree.fit1$finalModel)
#rpart.plot(tree.fit1$finalModel,fallen.leaves = FALSE)

#### [in-class practice] plot the importance of variables
importance=varImp(tree.fit1)$importance
featureImportance <- data.frame(Feature=row.names(importance), Importance=importance[,1])

# better looking plot
ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  xlab("variables") +
  ylab("Importance") + 
  ggtitle("Feature Importance\n")


##----------  assess the out of sample prediction accuracy -----------------#
## (i.e., performance on the testing set)
# [in-class practice] calculate confusion Matrix
pred_classes <- predict(tree.fit1, newdata = titanic_ts)
titanic_ts$Survived = as.factor(titanic_ts$Survived)
confusionMatrix(data = pred_classes, titanic_ts$Survived)

# plotting the roc curve
pred_probs <- predict(tree.fit1, titanic_ts, type = "prob")
library(pROC)
rpartROC <- roc(titanic_ts$Survived, pred_probs[, "Survived"],
                levels = c('Died','Survived'),
                direction = '<')
plot(rpartROC)
auc(rpartROC)

##---------Advanced:  build trees using "ctree2" or other options--------------#
# READ MORE HERE: https://cran.r-project.org/web/packages/caret/caret.pdf

#for reproducibility 
set.seed(100)

#  Conditional inference trees use a significance test which is a permutation (significant) test 
#      that selects covariate to split and recurse the variable. 
#      The p-value is calculated in this test. 
#      The significance test is executed at each start of the algorithm. 

tree.fit2 <- train(Survived ~ ., data=titanic_tr, 
                   method = 'ctree2', 
                   trControl=trainControl(method = 'cv', number=5),
                   tuneGrid=expand.grid(maxdepth=1:20,mincriterion=0.05)
)

# method = 'ctree2': Conditional Inference Tree
# Tuning parameters:
#   maxdepth (Max Tree Depth)
#   mincriterion (1 - P-Value Threshold)

# [in-class practice] plot the final model
plot(tree.fit2$finalModel)

# [in-class practice] plot the accuracy w/ tree depth
plot(tree.fit2)

######################################################################### 
############  EXAMPLE (2) with UniversalBank.csv (classification problem) 
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

# Drop ID and zip code columns
bank$ID<-NULL
bank$ZIP.Code<-NULL

str(bank)
bank$Education = as.factor(bank$Education)
bank$Personal.Loan = as.factor(bank$Personal.Loan)

#for reproducibility 
set.seed(1)
train_index = sample(row.names(bank), dim(bank)[1]*0.8)
test_index = setdiff(row.names(bank),train_index)  
bank_train = bank[train_index,]
bank_test = bank[test_index,]

#for reproducibility 
set.seed(55)

####  [in-class practice] Train the model
tree.fit3 <- train(Personal.Loan ~ ., data=bank_train, 
                   method = 'rpart', 
                   trControl=trainControl(method = 'cv', number=5),
                   tuneLength = 30,
                   #tuneGrid = data.frame(cp = c(0.01, 0.02,0.03,0.04,0.05)),
)


####  [in-class practice] plots the accuracies
plot(tree.fit3)

####  [in-class practice] draw the tree plot
plot(tree.fit3$finalModel)
text(tree.fit3$finalModel)

####  [in-class practice] more beautiful plots with rpart.plot
rpart.plot(tree.fit3$finalModel)
#rpart.plot(tree.fit1$finalModel,fallen.leaves = FALSE)


#### [in-class practice] plot the importance of variables
importance=varImp(tree.fit3)$importance
featureImportance <- data.frame(Feature=row.names(importance), Importance=importance[,1])

# better looking plot
ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  xlab("variables") +
  ylab("Importance") + 
  ggtitle("Feature Importance\n")
