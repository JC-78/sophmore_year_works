
# change working directory (replace according to your own setting)
setwd("~/DM_S21_05_Performance I")

library(ggplot2)


######################################################################### 
##################################    PCA  ##############################
######################################################################### 
#------------ (Example 2) PCA with more than two variables -------#

#### load the dataset: Cereals.csv
cereals.df <- read.csv("Cereals.csv") 

# [in-class practice] construct new dataset with all columns except c(1:3)
data_tmp = cereals.df[,-c(1:3)]

# [in-class practice] note: omit missing values using na.omit
data_tmp = na.omit(data_tmp)

# [in-class practice] compute PCs
pcs<-prcomp(data_tmp)
summary(pcs)

# [in-class practice] get the loading scores
pcs$rotation

#Loadings are interpreted as the coefficients of the linear combination of the
#initial variables from which the principal components are constructed. 
#Positive loadings indicate a variable and a principal component are positively correlated: an increase in one results in an
#increase in the other. Negative loadings indicate a negative correlation. 
#Large (either positive or negative) loadings indicate that a variable has a strong effect on that principal component.

# [in-class practice] get the corresponding scores of data points
scores <- data.frame(pcs$x)
scores$PC1
head(scores)

# [in-class practice] Plot the data points on PC1 and PC2
ggplot(scores, aes(x=PC1, y=PC2))+
  geom_point()

# Compute the proportion of variance of each PC
pcs_var = pcs$sdev^2
pcs_var_pct = round(pcs_var/sum(pcs_var)*100,2)

# [in-class practice] Plot the proportion of variance
pcs_var_pct = data.frame(pcs_var_pct)
pcs_var_pct$id <- as.numeric(row.names(pcs_var_pct))

ggplot(pcs_var_pct,aes(x=id, y=pcs_var_pct)) +
  geom_bar(stat = 'identity')


#------------ (Example 3) PCA with normalization -------#
#[in-class practice]
pcs.scaled<-prcomp(data_tmp, scale. = TRUE, center = TRUE)
summary(pcs.scaled)

######################################################################### 
#######################    Data Partition  ##############################
######################################################################### 

#### Load file UniversalBank.csv
bank<-read.csv("UniversalBank.csv")

# ## Get the index of missing values
# # Insert missing value in Age variable at row 1,3
# bank$Age[c(1,3)]<-NA
# 
# # Obtain indexes of missing values
# which(is.na(bank$Age))

# use set.seed() to get the same partitions when re-running the R code
set.seed(1)

####----- [in-class practice] partition data in training (60%) and test data (40%)
train_rows<-sample(row.names(bank), dim(bank)[1]*0.6)
test_rows<-setdiff(row.names(bank),train_rows)
train_data<-bank[train_rows,] 
test_data <-bank[test_rows,]


####----- [in-class practice] partition data in training (50%), validation (30%), and test (20%)
# step 1: randomly sample 50% of the row IDs for training
train_rows<-sample(row.names(bank), dim(bank)[1]*0.5)

# step 2: sample 30% of the row IDs into the valiation set, 
# drawing only from records not already in the training set
# use setdiff() to find records not already in the training set
validation_rows<-sample(setdiff(row.names(bank),train_rows),
                        dim(bank)[1]*0.3)

# step 3: assign the remaining 20% row IDs serve as test
test_rows<-setdiff(row.names(bank),union(train_rows,validation_rows))

# final step: create the 3 data frames by collecting all columns from the rows created above
train_data<-bank[train_rows,] 
validation_data<-bank[validation_rows,] 
test_data<-bank[test_rows,] 


######################################################################### 
############ Evaluation of Numerical Predictions  #######################
#########################################################################  
housing.dat<-read.csv("WestRoxbury.csv")

#### partition data in training (80%) and test data (20%)
housing.train_rows<-sample(row.names(housing.dat), dim(housing.dat)[1]*0.6)
housing.test_rows<-setdiff(row.names(housing.dat),housing.train_rows)
housing.train_data<-housing.dat[housing.train_rows,] 
housing.test_data <-housing.dat[housing.test_rows,]

#### Fit model to the training data
# Use lm function (“linear model”):
reg <- lm(TOTAL.VALUE ~ ., data = housing.train_data)
#reg <- lm(TOTAL.VALUE ~ .-TAX, data = housing.df, subset = train.rows) # remove TAX

library(forecast)

#### Assess accuracy for the training data
# TOTAL.VALUE is getting predicted; (= "fitted" values for the training data)  
# [in-class practice] print the predicted TOTAL.VALUE
reg$fitted.values

# compare the predicted TOTAL.VALUE side-by-side with the actual TOTAL.VALUE
tr.res <- data.frame(housing.train_data$TOTAL.VALUE, reg$fitted.values, reg$residuals)
head(tr.res)

#[in-class practice] compute the accuracy of the numerical prediction (based on the training set)
accuracy(reg$fitted.values, housing.train_data$TOTAL.VALUE)

#### predicting the testing data
#[in-class practice] compute the accuracy of the numerical prediction (based on the testing set)
pred <- predict(reg, newdata = housing.test_data)
ts.res <- data.frame(housing.test_data$TOTAL.VALUE, pred, 
                     residuals = housing.test_data$TOTAL.VALUE - pred)
head(ts.res)

#[in-class practice] compute the accuracy of the numerical prediction (based on the testing set)
accuracy(pred, housing.test_data$TOTAL.VALUE)

