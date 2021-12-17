# Data Mining 70374 In-class R-script --- Ch 2

#### (1) Getting Familiar with R

2+3 # addition
2-3 # subtraction
2*3 # multiplication
2/3 # division
2^3 # exponentiation
2*3-4 # combined multiplication and subtraction

x=6 # assigning 6 to variable x
y=2 # assigning 2 to variable y
x+y # adding x and y

log(100) # natural logarithm of 100
exp(3) # e^3

1:4 # integer sequence. or equivalently seq(1,4)
4:1 # decreasing sequence

z=c(1,3,4,6,7) # assigning a sequence (vector) to z

2*z # multiplying z with 2

length(z) # find the length of a vector 


sample(c(1:100),10)

#### (2) Preliminary Exploration in R: loading data, viewing it, summary statistics

#--------a. loading data
setwd("~/DM_S21_02_Data&R") # change working directory
housing.dat<-read.csv("WestRoxbury.csv") # load data
View(housing.dat) # show all the data in a new tab

#--------b.  viewing variables in r
colnames(housing.dat) # print column names (the list of variables)
colnames(housing.dat)[1] <- c("TOTAL_VALUE") # change the first column's name

ncol(housing.dat) # number of columns
nrow(housing.dat) # number of rows
dim(housing.dat) # find the dimension of data frame

housing.dat$REMODEL #  show the whole "REMODEL" column

housing.dat[,14] # selecting a column
housing.dat[1,] # selecting a row
housing.dat[1:10,1] # show the first 10 rows of the first column
housing.dat[1:8,] # selecting the first 8 rows
housing.dat[5,1:10] # show the fifth row of the first 10 columns
housing.dat[5,c(1:2, 4, 8:10)] # show the fifth row of some columns c(1:2, 4, 8:10)

housing.dat[1:10,]$TOTAL.VALUE # show the first 10 rows of the first column only

housing.dat$REMODEL <- as.factor(housing.dat$REMODEL) # change REMODEL into a factor variable

class(housing.dat$REMODEL) # class for a column
class(housing.dat[,14]) 

levels(housing.dat$REMODEL) # levels for a factor variable 

class(housing.dat$BEDROOMS) # BEDROOMS is an integer variable
class(housing.dat$TOTAL_VALUE) # TOTAL_VALUE is a numeric variable

summary(housing.dat)# find summary statistics for each column
summary(housing.dat$FIREPLACE) # printing the summary of a column
mean(housing.dat$TOTAL_VALUE) # find the mean of the first column



#### (3) Preliminary Exploration in R: creating dummy variables

# option 1: creating dummy variables for "REMODEL" with ifelse
housing.dat$REMODEL_None <- ifelse(housing.dat$REMODEL == 'None', 1, 0)
housing.dat$REMODEL_Old <- ifelse(housing.dat$REMODEL == 'Old', 1, 0)
housing.dat$REMODEL_Recent <- ifelse(housing.dat$REMODEL == 'Recent', 1, 0)

# option 2: creating dummy variables with fastDummies
install.packages('fastDummies')
library("fastDummies")
housing.dat <- dummy_cols(housing.dat, select_columns = 'REMODEL')



