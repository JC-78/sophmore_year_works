library(rpart)
library(caret)

## Building ensemble
# Load breast cancer data: wisc_bc_data.csv
wbcd <- read.csv("wisc_bc_data.csv")

# Remove ID variable
wbcd$id <- NULL

# Select the following variables as predictor variables: "area_mean","area_se","area_worst","compactness_mean","compactness_se","compactness_worst","concavity_mean","concavity_se","concavity_worst","dimension_mean","dimension_se","dimension_worst","perimeter_mean","perimeter_se","perimeter_worst","points_mean","points_se","points_worst","radius_mean","radius_se","radius_worst","smoothness_mean","smoothness_se","smoothness_worst","symmetry_mean","symmetry_se","symmetry_worst","texture_mean","texture_se","texture_worst"
predictors <- c("area_mean","area_se","area_worst","compactness_mean","compactness_se","compactness_worst","concavity_mean","concavity_se","concavity_worst","dimension_mean","dimension_se","dimension_worst","perimeter_mean","perimeter_se","perimeter_worst","points_mean","points_se","points_worst","radius_mean","radius_se","radius_worst","smoothness_mean","smoothness_se","smoothness_worst","symmetry_mean","symmetry_se","symmetry_worst","texture_mean","texture_se","texture_worst")

# Remove all variables apart from predictors and "diagnosis"
wbcd <- wbcd[,c(predictors, "diagnosis")]

# Use factor() to replace "B" and "M" by "Benign" and "Malignant" respectively in variable "diagnosis". 
# Hint: Use levels = c("B", "M") and labels = c("Benign", "Malignant")
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

# Partition data: 60/40
set.seed(1)
train_rows <- sample(row.names(wbcd), length(wbcd$area_mean)*0.6)
test_rows <- setdiff(row.names(wbcd), train_rows)
train_data <- wbcd[train_rows,]
test_data <- wbcd[test_rows,]


########################################
####  manually ensembling ####
########################################

# Use train(x,y,method='knn') for knn
knn_model <- train(diagnosis~., data = train_data, method = "knn")
#Predicting using knn model
pred_knn <- predict(knn_model, test_data)
# Obtain confusion matrix using confusionMatrix(prediction, truth)
confusionMatrix(pred_knn, test_data$diagnosis)


#Training Decision Tree using train(x,y,method='C5.0') or "rpart"
dt_model <- train(diagnosis~., data = train_data, method = 'C5.0')
#Predicting using C5.0 model
pred_dt <- predict(dt_model, test_data)
# Obtain confusionMatrix(prediction, truth)
confusionMatrix(pred_dt, test_data$diagnosis)


##Training the logistic model train(x,y,method='glm')
logistic_model <- train(diagnosis~., data = train_data, method = "glm")
# Predict using logistic
pred_logistic <- predict(logistic_model, test_data)
# Obtain confusionMatrix(prediction, truth)
confusionMatrix(pred_logistic, test_data$diagnosis)


##--------------------------------------##
## Option 1: use majority vote to predict
##--------------------------------------##
# Predict class instead of probability, and convert the class into 0/1 values
pred_knn <- predict(knn_model, test_data)
pred_knn_0_1 <- ifelse(pred_knn == "Benign", 1, 0)

pred_dt <- predict(dt_model, test_data)
pred_dt_0_1 <- ifelse(pred_dt=="Benign", 1, 0)

pred_logistic <- predict(logistic_model, test_data)
pred_logistic_0_1 <- ifelse(pred_logistic == "Benign", 1, 0)


# Add the votes
Majority_Vote <- pred_knn_0_1 + pred_dt_0_1 + pred_logistic_0_1


# Predict using majority rule.
pred_majority <-factor(ifelse(Majority_Vote >=2, "Benign", "Malignant"))
confusionMatrix(pred_majority, test_data$diagnosis)

##--------------------------------------##
## Option 2: using avg among predicted probabilities to votes
##--------------------------------------##
# Predict classification probabilities for knn, dection tree, and logistic models
pred_pr_knn <- predict(knn_model, test_data, type = "prob")
pred_pr_knn_B <- pred_pr_knn$Benign

pred_pr_dt <- predict(dt_model, test_data, type = "prob")
pred_pr_dt_B <- pred_pr_dt$Benign

pred_pr_logistic <- predict(logistic_model, test_data, type = "prob")
pred_pr_logistic_B <- pred_pr_logistic$Benign

# Compute the average of classification probabilities (for Benign class) 
Avg_prob <- (pred_pr_knn_B+pred_pr_dt_B+pred_pr_logistic_B)/3

# Take weighted average (weighted by model accuracies) to create ensembles.
Wtd_Avg_prob <- (0.9254*pred_pr_knn_B+0.9605*pred_pr_dt_B+0.9079*pred_pr_logistic_B)/(0.9254+0.9605+0.9079)

# Predict as "Benign" if Avg_Prob>0.5 otherwise as "Malignant"
pred_avg <- factor(ifelse(Avg_prob > 0.5, "Benign", "Malignant"))

# Obtain confusionMatrix(prediction, truth)
confusionMatrix(pred_avg, test_data$diagnosis)

########################################
####  ensembling with caret models ####
########################################

#install.packages("caretEnsemble")
library("caretEnsemble")
library("rpart")


ctr = trainControl(method="cv",
                   savePredictions="final",
                   classProbs=TRUE,
                   index=createResample(train_data$diagnosis, 10),
                   summaryFunction=twoClassSummary)

# index:  specify the resampling iteration. (Each list element is a vector of 
#        integers corresponding to the rows used for training at that iteration.)
# prediction: a logical to save the hold-out predictions for each resample

# caretList is the preferred way to construct list of caret models in this package, 
#          as it will ensure the resampling indexes are identical across all models. 
modelList = caretList(diagnosis~., 
                      data=train_data,
                      trControl=ctr,
                      preProc=c('center','scale'),
                      methodList=c("knn", "C5.0",'glm'),
                      metric="ROC")

modelList

# predict
yhatModelList = lapply(modelList, predict, newdata=test_data)
confusionMatrix(data=yhatModelList$knn, test_data$diagnosis)
confusionMatrix(data=yhatModelList$C5.0, test_data$diagnosis)



# ensembled model
resultEnsemble = caretEnsemble(modelList, 
                               metric="ROC",
                               trControl=ctr)
summary(resultEnsemble)


# predictions
predictEnsemble = predict(resultEnsemble, test_data)
confusionMatrix(data=predictEnsemble, test_data$diagnosis)
