# Data Mining 70374 In-class R-script --- Ch 3


#### (0) Loading Data
# change working directory
setwd("~/Dropbox/4_Teaching/1_Data Mining/1_slides/DM_S21_03_Data Exploration")
# load data
housing.dat<-read.csv("WestRoxbury.csv") 


#### (1) Replacing missing data with median
# To illustrate how we deal with the missing data problem, we first convert a few
# entries for bedrooms to NA's. 
rows.setto.missing <- sample(row.names(housing.dat),10)
housing.dat[rows.setto.missing,]$BEDROOMS <- NA
summary(housing.dat$BEDROOMS) # NOW we have 10 NA's and the median of the remaining values is 3

# Then we impute these missing values using the median of the remaining values. 
median(housing.dat$BEDROOMS,na.rm=TRUE) # compute median while ignoring missing values
housing.dat[rows.setto.missing,]$BEDROOMS <- median(housing.dat$BEDROOMS,na.rm=TRUE)
summary(housing.dat$BEDROOMS) 


#### (2) Data transformation
housing.dat$TOTAL.VALUE
library(ggplot2)
ggplot(housing.dat,aes(x=TOTAL.VALUE))+ geom_histogram(binwidth = 20) 

# min-max normalization
housing.dat$TOTAL.VALUE_uni <- (housing.dat$TOTAL.VALUE-min(housing.dat$TOTAL.VALUE))/(max(housing.dat$TOTAL.VALUE)-min(housing.dat$TOTAL.VALUE))
ggplot(housing.dat,aes(x=TOTAL.VALUE_uni))+ geom_histogram(binwidth = 0.02) 

# z score normalization
housing.dat$TOTAL.VALUE_std_norm <- (housing.dat$TOTAL.VALUE-mean(housing.dat$TOTAL.VALUE))/sd(housing.dat$TOTAL.VALUE)
ggplot(housing.dat,aes(x=TOTAL.VALUE_std_norm))+ geom_histogram(binwidth = 0.2) 

#### (3) Preliminary Exploration -- Data Summary

#------ Boston housing data: Load BostonHousing.csv ----#
## This data contain information on census tracts in Boston
housing.df<-read.csv("BostonHousing.csv")

# dimensions of data frame
dim(housing.df)

# show the first few rows
head(housing.df)

# find summary statistics of each column
summary(housing.df)

# mean/min of a variable
mean(housing.df$CRIM)
min(housing.df$CRIM)

# Change variable MEDV to Median_Value
names(housing.df)[13]<-"Median_Value"

# Change variable LSTAT to Low_Status
names(housing.df)[12]<-"Low_Status"


#### (4) Preliminary Exploration -- Data Visualization

#------------- BASICS of ggplot    --------------------#
## install and import ggplot2
library('ggplot2')

# Use dev.off() if you get error: "invalid graphics state"


# Scatter plot between Low_Status and Median_Value using ggplot

ggplot(housing.df,aes(x=Low_Status,y=Median_Value)) + 
  geom_blank() + ylim(0,50) + xlim(0,40)+ # a black sheet with xrange and yrange
  geom_point(alpha=0.5) + # aggregation functions or points/lines
  geom_hline(yintercept = 20) + # adding a horizontal line
  geom_vline(xintercept = 20) + # adding a vertical line
  ggtitle("An example ggplot")+
  xlab("Low Status") + ylab("Median Value")
  

#------------- One Variable Plots    --------------------#
## Plot Barchart of CHAS
housing.df$CHAS<-as.factor(housing.df$CHAS)
ggplot(housing.df,aes(x=CHAS))+
  geom_bar() # aggregation function here is geom_bar

## Plot histogram of Median_Value
ggplot(housing.df,aes(x=Median_Value))+
  geom_histogram(binwidth = 5) + # aggregation function here is geom_histogram
  stat_bin(aes(y=..count.., label=..count..),binwidth = 5, geom="text") 
# there is an automatic backet func with histogram, which is used to deal with continous variables


#------------- Two Variable Plots    --------------------#

# Scatter plot between Low_Status and Median_Value 
ggplot(housing.df,aes(x=Low_Status,y=Median_Value)) + 
  geom_blank() + ylim(0,50) + xlim(0,40)+ # a black sheet with xrange and yrange
  geom_point(alpha=0.5)  # aggregation functions or points/lines


## boxplot of MEDV for different values of CHAS
ggplot(housing.df,aes(x=CHAS,y=Median_Value))+
  geom_boxplot()

## compute mean Median_Value per CHAS = (0, 1)
View(housing.df)
mean_per_chas<-aggregate(housing.df$Median_Value,by=list(housing.df$CHAS),FUN=mean)
mean_per_chas
names(mean_per_chas) <- c("CHAS","MeanMEDV")
ggplot(mean_per_chas,aes(CHAS,MeanMEDV))+
  geom_bar(stat = "identity")

# Note that: The heights of the bars commonly represent one of two things: 
# (i) stat="bin": either a count of cases in each group, which is the default. 
#     The height of each bar equal to the number of cases in each group.
# (ii) stat="identity": the values in a column of the data frame. 
#     The heights of the bars to represent values in the data, 

#------------- Advanced Plots    --------------------#
library("reshape")

housing.df$CHAS<-as.numeric(housing.df$CHAS)

cor_matrix<-cor(housing.df)
cor_mat_melt<-melt(round(cor_matrix,2))

## heatmap for correlation among all variables
ggplot(cor_mat_melt,aes(x=X1,y=X2))+
  geom_tile(aes(fill=value))+
  scale_fill_gradientn(colours = c("blue", "white", "red"), limits = c(-1,1.0))+
  geom_text(aes(label=value))+
  guides(x =  guide_axis(angle = 90)) 

## color plot between NOX and Low_Status. Use CAT..MEDV as colour
# table(housing.df$CAT..MEDV)
housing.df$CAT..MEDV <- as.factor(housing.df$CAT..MEDV)
ggplot(housing.df,aes(x=Low_Status,y=NOX))+
  geom_point(aes(colour=CAT..MEDV))

#------------- Rescaling    --------------------#

## Scatter plot between CRIM and MEDV
ggplot(housing.df,aes(x=CRIM,y=Median_Value))+
  geom_point(alpha=0.5)

## Create a new variable for Log_Crime for log(CRIM)
housing.df$Log_Crime<-log(housing.df$CRIM)

## Scatter plot between log_Crime and MEDV
ggplot(housing.df,aes(x=Log_Crime,y=Median_Value))+
  geom_point(alpha=0.5)



View(housing.df)
