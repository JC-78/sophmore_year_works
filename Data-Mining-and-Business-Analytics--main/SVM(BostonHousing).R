## load packages
library(caret)
# svm
library(kernlab)
# ROC curve 
library(pROC)	

######################## Preparation #########################
## Read in data 
Boston.df <- read.csv("BostonHousing.csv")
str(Boston.df)

## Create Y Variable: highCrime
threshold = 0.5
Boston.df$highCrime = as.factor(
  ifelse(Boston.df$CRIM > threshold,"yes","no")
)
summary(Boston.df$highCrime)
# remove crim variable
Boston.df$CRIM = NULL


## partition data in training (60%) and test data (40%)
set.seed(100)
df.train_rows<-sample(row.names(Boston.df), dim(Boston.df)[1]*0.6)
df.test_rows<-setdiff(row.names(Boston.df),df.train_rows)
Boston.df_train<-Boston.df[df.train_rows,] 
Boston.df_test <-Boston.df[df.test_rows,]

######################## Preset Resampling Details ##########

## setting the resampling method
ctrl = trainControl(method="cv",      # simple cross-validation
                    number = 10,      # 10 folds
                    summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                    classProbs=TRUE  # this should be included so that the 
                    #    algorithm will store predicted probabilities 
                    #     for later generating ROC plot.
                    )

## another way of resampling, using repeated cross-validation.
# ctrl = trainControl(method="repeatedcv",   # repeated Cross-validation
#                     number = 10,
#                     repeats=3,		         # do 3 repetitions of cv
#                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
#                     classProbs=TRUE        
# )

######################## SVM Linear ##########

# training svm model - Linear kernel
set.seed(100)
svmFit = train(highCrime ~ ., data=Boston.df_train, 
               method="svmLinear",   # linear kernel
               tuneGrid=expand.grid(C = c(0.01,0.1,1,10,100)),
               preProc=c("center","scale"),  # Center and scale data
               metric="ROC",
               trControl=ctrl)

# Note:  (1) "C" (also known as Cost): tuning parameter to adjust how hard or soft 
#            your margin should be. A larger C means harder margin.
#            Mathmetically, it determines the possible misclassifications. It 
#            essentially imposes a penalty to the model for making an error: 
#            the higher the value of C, the less likely it is that the SVM algorithm # 
#            will misclassify a point. (Default C=1)

#        (2) in SVM linear C parameter is held constant when using tuneLength.
#            so if you want to change it, you need to specify it in tuneGrid

# the resulting model
svmFit$finalModel
plot(svmFit)


# predict on the test data
testlinearSVM = predict(svmFit, newdata=Boston.df_test)
testlinearSVMProb = predict(svmFit, newdata=Boston.df_test, type="prob")

# analyze model results
confusionMatrix(data=testlinearSVM, Boston.df_test$highCrime)

# ROC
svmLinearROC = roc(predictor=testlinearSVMProb$yes,
                   response=Boston.df_test$highCrime,
                   levels=levels(Boston.df_test$highCrime),
                   direction = "<")
## Notes:
# "response": a vector of true classes
# "predictor": the predicted value (probability) of each observation 
# "levels": control = 0, case = 1, respectively
# "levels": control = 0, case = 1, respectively
# "direction": controls < cases
#     “>”: if the predictor values for the control group are higher than the values of the case group (controls > t >= cases). 
#     “<”: if the predictor values for the control group are lower or equal than the values of the case group (controls < t <= cases). 

plot(svmLinearROC, type="S", col="red")

auc(svmLinearROC)


########################### SVM Radial ###########################
set.seed(100)
svmFit = train(highCrime ~ ., data=Boston.df_train, 
                method = "svmRadial",   # Radial kernel
                tuneLength = 10,					# try 10 different values for each parameter
                # tuneGrid = expand.grid(sigma = c(0.01,0.1,0.2), 
                #                        C = c(0.25,0.5,0.75,1,1.25,1.5)),
                preProc = c("center","scale"),  # Center and scale data
                metric ="ROC",
                trControl = ctrl)

# Note that: in SVM radial sigma parameter is held constant when using tuneLength
#           so if you want to change it, you need to specify it in tuneGrid
#           tuneGrid = expand.grid(sigma = c(0.01,0.1,0.2), C = c(0.25,0.5,0.75,1,1.25,1.5))

# the resulting model
svmFit$finalModel
plot(svmFit)

# predict on the test data
testRadialSVM = predict(svmFit, newdata = Boston.df_test)
#trainSVM = predict(svmFit, newdata = Boston.df_train)

testRadialSVMProb = predict(svmFit, newdata= Boston.df_test, type = "prob")

# analyze model results
confusionMatrix(data = testRadialSVM, Boston.df_test$highCrime)
#confusionMatrix(data = trainSVM, Boston.df_train$highCrime)


# ROC
svmRadialROC <- roc(predictor = testRadialSVMProb$yes,
                    response = Boston.df_test$highCrime,
                    levels=levels(Boston.df_test$highCrime),
                    direction = "<")
## Notes:
# "response": a vector of true classes
# "predictor": the predicted value (probability) of each observation 
# "levels": control = 0, case = 1, respectively
# "levels": control = 0, case = 1, respectively
# "direction": controls < cases
#     “>”: if the predictor values for the control group are higher than the values of the case group (controls > t >= cases). 
#     “<”: if the predictor values for the control group are lower or equal than the values of the case group (controls < t <= cases). 

plot(svmRadialROC, type = "S", col = "orange")

auc(svmRadialROC)



########################### SVM POLY ###########################
set.seed(100)
svmFit = train( highCrime ~ ., data=Boston.df_train, 
                method = "svmPoly",    # Polynomial kernel
                tuneLength = 3,				 # try 3 different values for each parameter
                # tuneGrid = expand.grid(scale = c(0.01,0.1,0.2), 
                #                        degree = c(1,2,3), 
                #                        C = c(0.25,0.5,0.75,1,1.25,1.5)),
                preProc = c("center","scale"),  # Center and scale data
                metric ="ROC",
                trControl = ctrl)

# Note that: alternatively, you can also specify tuning parameters in tuneGrid


# the resulting model
svmFit$finalModel
plot(svmFit)

# predict on the test data
testPolySVM = predict(svmFit, newdata = Boston.df_test)
#trainSVM = predict(svmFit, newdata = Boston.df_train)

testPolySVMProb = predict(svmFit, newdata= Boston.df_test, type = "prob")

# analyze model results
confusionMatrix(data = testPolySVM, Boston.df_test$highCrime)
#confusionMatrix(data = trainSVM, Boston.df_train$highCrime)


# ROC
svmPolyROC <- roc(predictor = testPolySVMProb$yes,
                  response = Boston.df_test$highCrime,
                  levels=levels(Boston.df_test$highCrime),
                  direction = "<")
## Notes:
# "response": a vector of true classes
# "predictor": the predicted value (probability) of each observation 
# "levels": control = 0, case = 1, respectively
# "levels": control = 0, case = 1, respectively
# "direction": controls < cases
#     “>”: if the predictor values for the control group are higher than the values of the case group (controls > t >= cases). 
#     “<”: if the predictor values for the control group are lower or equal than the values of the case group (controls < t <= cases). 

plot(svmPolyROC, type = "S", col = "black")

auc(svmPolyROC)

########################plot all roc on the same plot ###########################
plot(svmLinearROC, type = "S", col = "blue")
plot(svmPolyROC, type= "S", add = TRUE, col = "black")
plot(svmRadialROC, type= "S", add = TRUE, col = "orange")

legend("bottomright", legend=c("linearSVM","polySVM","radialSVM"),
       lty=1:1, cex=0.8,col=c( "blue","black","orange"))


