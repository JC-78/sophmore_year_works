## Read in data 
utilities.df = read.csv("Utilities.csv")

# set row names to the column "company"
row.names(utilities.df) <- utilities.df[,1]

# remove the column "company"
utilities.df <- utilities.df[,-1]

## --- compute Euclidean distance
d <- dist(utilities.df, method = "euclidean")

## --- standardize all the variables
library(caret)
# Estimate Pre-processing transformation (centering, scaling etc.) 
#    Estimated from the training data and applied to any data set with the same variables.
preproc = preProcess(utilities.df) # default: method = c("center", "scale")
utilities.df.norm = predict(preproc, utilities.df)

## -- run k means
set.seed(100)
km <- kmeans(utilities.df.norm, centers = 6, iter.max=1000)

# show cluster membership
km$cluster

# show cluster centers
km$centers

# within cluster sum of squared
km$withinss

# cluster size
km$size


## -- [optional/advanced content] plot the features of clusters based on centroids
# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = 
       c(min(km$centers), max(km$centers)), xlim = c(0, 8))
# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),"black", "dark grey"))

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))

