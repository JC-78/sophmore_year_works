

# change working directory (replace according to your own setting)
setwd("~/DM_S21_04_DimensionReduction")


##################### Sampling and Oversampling         ##############
# load data
housing.dat<-read.csv("WestRoxbury.csv") 

# [in-class practice] random sample of 20 observations

sample_rows<-sample(row.names(housing.dat),20)


# if we want to fix the sampled rows, we can start with a given random seed
set.seed(1)

# [in-class practice] oversample 20 houses with strictly more than 10 rooms

sample_rows<-sample(row.names(housing.dat),20,prob=ifelse(housing.dat$ROOMS>10,0.9,0.01))
# Note: prob: a vector of probability weights for obtaining 
#       the elements of the vector being sampled. 
housing.dat_sampled = housing.dat[sample_rows,]

housing.dat_sampled
table(housing.dat_sampled$ROOMS)

ggplot(housing.dat_sampled, aes(x=ROOMS))+
  geom_histogram(binwidth = 2)

###################     PCA         ###################################

library(ggplot2)

#PCA object contains the following information:
#the center point ($center), scaling ($scale)
# standard deviation(sdev) of each principal component
# The relationship (correlation or anticorrelation, etc) between the initial variables 
#   and the principal components ($rotation)
# The values of each sample in terms of the principal components ($x)

#### BREAKOUT SESSION starts
#### [in-class practice] load the dataset: Cereals.csv
cereal.df=read.csv('Cereals.csv')

# a. Dimensions of the dataset
dim(cereal.df)

# b. Names of the variables, which ones are non-numeric variables
names(cereal.df)
# c. Summary statistics of the variables
summary(cereal.df)

# [in-class practice] covariance matrix for calories and ratings.
cov(cereal.df$calories,cereal.df$rating)

cov(cereal.df[c("calories","ratings")])
# [in-class practice] Scatter Plot between calories and rating
ggplot(cereal.df,aes(x=calories,y=rating))+
  geom_blank()+
  geom_point(alpha=0.5)+
  ggtitle("Scatter Plot Calories vs Rating") +
  xlab("Calories")+ylab("Rating")

#### BREAKOUT SESSION ends

#------------ (Example 1) Two variable PCA -------#
# New dataset with only two columns
data_tmp = cereal.df[,c("calories","rating")]
#alternative way of creating the new dataset
data_tmp = data.frame(cereal.df$calories,cereal.df$rating)

# [in-class practice] PCA for the new dataset (which contains only two columns)
# Get summary of principal components
pcs=prcomp(data_tmp)
summary(pcs) #list of length 5. Not 5 PCs. 5 elements in each PC. ex.sdev, rotation,center, scale,x)
# [in-class practice] get the loading scores

pcs$rotation
#each row is the component of PC. The value given is how much it consists of the row variable
#rotation indicates component loading values. 
#Loadings scores/values are interpreted as the coefficients of the linear combination 
#of the initial variables from which the principal components are constructed. 

# [in-class practice] get the corresponding scores of data points
pcs$x   #where the points will lie on the new graph with the new PC
scores=data.frame(pcs$x)

# Plot the data points on PC1 and PC2
ggplot(scores, aes(x=PC1, y=PC2))+
  geom_point()

# Compute the proportion of variance of each PC
pcs$sdev #how much information is proportionally represented by PC

pcs_var = pcs$sdev^2
pcs_var_pct = round(pcs_var/sum(pcs_var)*100,2)

# Plot the proportion of variance
pcs_var_pct = data.frame(pcs_var_pct)
pcs_var_pct$id <- as.numeric(row.names(pcs_var_pct))

ggplot(pcs_var_pct,aes(x=id, y=pcs_var_pct)) +
  geom_bar(stat = 'identity')

#------------ (Example 2) PCA of all columns except c(1:3) -------#
# [in-class practice] construct new dataset with all columns except c(1:3)


# [in-class practice] note: omit missing values using na.omit


# [in-class practice] compute PCs



#------------ (Example 3) PCA with normalization -------#
#[in-class practice]





