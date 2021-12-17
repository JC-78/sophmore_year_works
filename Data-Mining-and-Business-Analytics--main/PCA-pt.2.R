

# change working directory (replace according to your own setting)
setwd("~/DM_S21_04_DimensionReduction")


##################### Sampling and Oversampling         ##############
# load data
housing.dat<-read.csv("WestRoxbury.csv") 

# [in-class practice] random sample of 20 observations
sample_rows <- sample(row.names(housing.dat), 20)
sample_rows
housing.dat_sampled = housing.dat[sample_rows,]

ggplot(housing.dat_sampled, aes(x=ROOMS))+
  geom_histogram(binwidth = 2)

# if we want to fix the sampled rows, we can start with a given random seed
set.seed(1)

# [in-class practice] oversample 20 houses with strictly more than 10 rooms
table(housing.dat$ROOMS)
sample_rows <- sample(row.names(housing.dat), 20, 
                      prob = ifelse(housing.dat$ROOMS>10, 0.9, 0.01))
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


#### [in-class practice] load the dataset: Cereals.csv
# a. Dimensions of the dataset
# b. Names of the variables, which ones are non-numeric variables
# c. Summary statistics of the variables

cereals.df <- read.csv("Cereals.csv") 
dim(cereals.df)
names(cereals.df)
summary(cereals.df)

# [in-class practice] covariance matrix for calories and ratings.
cov(cereals.df[c("calories",'rating')])

# [in-class practice] Scatter Plot between calories and rating
ggplot(cereals.df,aes(x=calories,y=rating))+
  geom_point()

#------------ (Example 1) Two variable PCA -------#
# New dataset with only two columns
data_tmp = cereals.df[,c("calories","rating")]
#alternative way of creating the new dataset
data_tmp = data.frame(cereals.df$calories,cereals.df$rating)

# [in-class practice] PCA for the new dataset (which contains only two columns)
pcs <- prcomp(data_tmp) 
# Get summary of principal components
summary(pcs) 

# [in-class practice] get the loading scores
pcs$rotation

# [in-class practice] get the corresponding scores of data points
scores <- data.frame(pcs$x)
scores$PC1
head(scores)

# Plot the data points on PC1 and PC2
ggplot(scores, aes(x=PC1, y=PC2))+
  geom_point()

# Compute the proportion of variance of each PC
pcs_var = pcs$sdev^2
pcs_var_pct = round(pcs_var/sum(pcs_var)*100,2)

# Plot the proportion of variance
pcs_var_pct = data.frame(pcs_var_pct)
pcs_var_pct$id <- as.numeric(row.names(pcs_var_pct))

ggplot(pcs_var_pct,aes(x=id, y=pcs_var_pct)) +
  geom_bar(stat = 'identity')

#------------ (Example 2) PCA of all columns except c(1:3) -------#
# [in-class practice] construct new dataset with all columns except c(1:3)
data_tmp = cereals.df[,-c(1:3)]

# [in-class practice] note: omit missing values using na.omit
data_tmp = na.omit(data_tmp)

# [in-class practice] compute PCs
pcs<-prcomp(data_tmp)
summary(pcs)

#------------ (Example 3) PCA with normalization -------#
#[in-class practice]
pcs.scaled<-prcomp(data_tmp, scale. = TRUE, center = TRUE)
summary(pcs.scaled)


