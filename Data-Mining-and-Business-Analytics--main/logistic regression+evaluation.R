
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
# Why are we removing zip-codes? Any Guess?
bank$ID<-NULL
bank$ZIP.Code<-NULL

# ## Get the index of missing values
# # Insert missing value in Age variable at row 1,3
# bank$Age[c(1,3)]<-NA
# 
# # Obtain indexes of missing values
# which(is.na(bank$Age))


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

# first 8 actual and predicted records
pred[1:8]
test_data$Personal.Loan[1:8]

# create a dataframe to compare the prediction score and actual loan outcome
df_compare = data.frame(pred, test_data$Personal.Loan)

colnames(df_compare) = c("pred_score","actual_outcome")

df_compare$pred_outcome_09 = as.factor(
  ifelse(df_compare$pred_score>0.9,1,0)
)

