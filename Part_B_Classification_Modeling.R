# Feedback
# EDA was reasonable but brief, there are a number of things you can look at and then highlighting the important ones is key. Picked up on the fact that Gender and Age_band have high null values but did not propose what to do about it. ID column was removed. Recognised the class imbalance but did not note what they would do with this. Could just be aware not to use accuracy and be considerate of the confusion matrix or could try sampling methods.

# Overall consider more granular formatting of your work. More sections and subsections, dot points etc to make an easier read for the business stakeholder. 
# 
# Had a linear and a tree based model. Included precision, recall, F1 and AUC to compare. Did not note which specific metric to optimise for. Several metrics were given but which ONE would you consider above all others? When having to make a choice between recall up and precision down - is that a good or bad choice to make? You will come across these crossroads so having a primary metric will allow you to make these decisions. Consider the business case - is it more important to NOT miss out on a sale or to NOT annoy a customer who potentially will not buy?
#   
#   Partial dependency plots were created but not interpreted in a business sense. Feature importances also generated but not dealt with deeply in a business sense. 
# 
# Good evidence of a feature selection method for LASSO but not mentioned for random forest. Generally good quality code. A reasonable result for your models. Consider in future more advanced tuning to build even better models. 
# 
# Good use of the CRISP-DM framework throughout. Some discussion on ethics and privacy throughout, though only in brief and in passing.

# Final total Grade: D

#-----------------------------------------
# Hendrik Schmidt(13295403)
# DAM - Assignment 2B 
#
# Classification modelling
#-----------------------------------------

rm(list=ls())
setwd("~/Desktop/Data, Algorithms and Meaning/DAM Assignment 2/DAM_A1_B")
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(caret)
library(corrplot)
library(glmnet)
library(rpart)
library(rpart.plot)
library(mlbench)
library(randomForest)
library(ROCR)
library(pROC)

# Importing Repurchase_Training.csv File

repurchase_data<-read.csv("repurchase_training.csv",header = TRUE)

# Check the data set
dim(repurchase_data)
glimpse(repurchase_data)

# Check the data for missing values ####
attach(repurchase_data) 
summary(Target)     ## no missing values 
summary(age_band)   ## 112.375 missing values 
summary(gender)     ## 69.308 missing values
summary(car_model)  ## no missing values 
summary(car_segment)## Other category appear 58 times 
detach(repurchase_data)

# set Target as.factor()
repurchase_data <- repurchase_data %>% 
  mutate(Target = as.factor(Target))

# Exploratory Data Analysis on repurchase dataset and plotting categorical variables
# fill selected columns using the previous entry.

repurchase_data %>% 
  ggplot(aes(x=age_band, fill=Target)) + 
  geom_bar()
# without empty entities for age_band
repurchase_data %>% 
  filter(age_band != "NULL") %>%
  ggplot(aes(age_band, fill=Target)) + 
  geom_bar()

repurchase_data %>% 
  ggplot(aes(x=gender, fill=Target)) + 
  geom_bar()
# without empty entities for gender
repurchase_data %>% 
  filter(gender != "NULL") %>% 
  ggplot(aes(x=gender, fill=Target)) + 
  geom_bar()

repurchase_data %>% 
  filter(Target == T) %>%  
  ggplot(aes(gender)) + 
  geom_bar()

repurchase_data %>% 
  ggplot(aes(car_model, fill=Target)) + 
  geom_bar()

repurchase_data %>% 
  filter(Target == T) %>%  
  ggplot(aes(car_model)) + 
  geom_bar()

repurchase_data %>% 
  ggplot(aes(car_segment, fill=Target)) + 
  geom_bar()
# without the entities "Other" for car_segment
repurchase_data %>% 
  filter(car_segment != "Other") %>% 
  ggplot(aes(car_segment,  fill=Target)) + 
  geom_bar()

# Distribution of numeric variables
qplot(as.factor(repurchase_data$age_of_vehicle_years))
qplot(as.factor(repurchase_data$sched_serv_warr))
qplot(as.factor(repurchase_data$non_sched_serv_warr))
qplot(as.factor(repurchase_data$sched_serv_warr))
qplot(as.factor(repurchase_data$total_paid_services))
qplot(as.factor(repurchase_data$total_services))
qplot(as.factor(repurchase_data$mth_since_last_serv))
qplot(as.factor(repurchase_data$annualised_mileage))
qplot(as.factor(repurchase_data$num_dealers_visited))
qplot(as.factor(repurchase_data$num_serv_dealer_purchased))

# Correlation of numeric variables, corrplot, correlogram
numeric_var <- names(dplyr::select_if(repurchase_data, is.numeric))
numeric_variables_corr <- cor(repurchase_data[, numeric_var])
corrplot(numeric_variables_corr, 
         method="number", 
         type="lower")

# Partition the data - Create train and test set
# Creat reset train and test set because of the GLM setup
set.seed(42)
train_dummy <- createDataPartition(y = repurchase_data$Target, p = 0.7, list = FALSE)
training_set <- repurchase_data[train_dummy, ]
testing_set <- repurchase_data[-train_dummy, ]
reset_training_set = training_set
reset_testing_set = testing_set


#-------------------------------------------------------------------------------------------
#Logistic Regression
#-------------------------------------------------------------------------------------------
#F1:  0.344776119 
#Sensitivity/Recall: 0.218750000 
#Precision Value:  0.813380282 
#AUC = 0.6087

#             Reference
#Prediction     0     1
#         0 38291   825
#         1    53   231

# Create models
fit_glm <-  glm(formula = Target ~. -ID, data = training_set, family = "binomial")
summary(fit_glm)

# predict the model
testing_set$probability <- predict(fit_glm, newdata=testing_set, type = "response")


# Set probability threshold to 0.5

testing_set$prediction = "0"
testing_set[testing_set$probability >= 0.5, "prediction"] = "1"

# Setting up confusion matrix
conM_glm <- confusionMatrix(data = as.factor(testing_set$prediction), testing_set$Target, positive="1")
conM_glm

# Setting up the AUC
testing_set$prediction <- as.numeric(testing_set$prediction)
roc_object <- roc(response = testing_set$Target, predictor = testing_set$prediction)
roc_object #AUC = 0.7541
plot(roc_object)

# Get F1, Sensitivity, Precision Value
conM_glm$byClass

#Collinearity Check


#-------------------------------------------------------------------------------------------
#LASSO Regression
#-------------------------------------------------------------------------------------------

#F1: 0.201666667 
#Sensitivity/recall: 0.114583333
#Precision value: 0.840277778
#AUC = 0.557

#           Reference
#Prediction     0     1
#         0 38321   935
#         1    23   121

# Set up X and Y for lasso Regression with CV
# remove ID and Target from Data set
train <-  reset_training_set
test <-  reset_testing_set
x <-  model.matrix(~ ., train[, c(-1,-2)])
y <-  train$Target

# Alpha = 1 specifies lasso regression
# Create model
set.seed(42)
fit_lasso <-  cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Plot lasso model 
plot(fit_lasso)

# Choose either lambda.min or lambda.1se 
# Lambda.min tend to overfit model
fit_lasso$lambda.min 
fit_lasso$lambda.1se 
coef(fit_lasso, fit_lasso$lambda.min)
max(abs(coef(fit_lasso, fit_lasso$lambda.1se)))



# Predict the model
pred_lasso <-  predict(fit_lasso$glmnet.fit, 
                           newx = model.matrix(~ ., test[, c(-1,-2)]), 
                           type = "class", s = fit_lasso$lambda.1se)

# Setting up confusion matrix
conM_lasso <-  confusionMatrix(data = as.factor(pred_lasso), test$Target, positive="1")

# Get F1, Sensitivity, Precision Value
conM_lasso$byClass 
conM_lasso

# Setting up the AUC
pred_lasso <- as.numeric(pred_lasso)
roc_object <- roc(response = test$Target, predictor = pred_lasso)
roc_object #AUC = 0.7541
plot(roc_object)

#-------------------------------------------------------------------------------------------
#Decision Tree 
#-------------------------------------------------------------------------------------------
#F1: 0.60975610 
#Sensitivity/recall: 0.47348485  
#Precision value:  0.85616438  
#AUC = 0.7356

#             Reference
#Prediction     0     1
#         0 38260   556
#         1    84   500

set.seed(42)
# Set up X and Y for lasso Regression with CV
# remove ID and Target from Data set
train <-  reset_training_set
test <-  reset_testing_set
x <-  model.matrix(~ ., train[, c(-1,-2)])
y <-  train$Target

# Create model
fit_dtree <-  rpart(Target ~.-ID,data = train, method="class") 

# Plot tree
prp(fit_dtree)

# Predict the model
pred_dtree = predict(fit_dtree,test,type="class")

# Setting up confusion matrix
conM_dtree = confusionMatrix(data = as.factor(pred_dtree), test$Target, positive="1")
conM_dtree

# Get F1, Sensitivity, Precision Value
conM_dtree$byClass

# Setting up the AUC
pred_dtree <- as.numeric(pred_dtree)
roc_object <- roc(response = test$Target, predictor = pred_dtree)
roc_object #AUC = 0.7541
plot(roc_object)

#-------------------------------------------------------------------------------------------
#Random Forests
#-------------------------------------------------------------------------------------------
#F1 - 0.85699374 
#Sensitivity/recall: 0.77746212 
#precision value: 0.95465116 
#AUC = 0.8897

#           Reference
#Prediction     0     1
#           0 38305   235
#           1    39   821

# Set up X and Y for lasso Regression with CV
# remove ID and Target from Data set
train <-  reset_training_set
test <-  reset_testing_set
x <-  model.matrix(~ ., train[, c(-1,-2)])
y <-  train$Target

# Create model
fit_RF = randomForest(Target ~. -ID, data = train, importance=TRUE,
                      xtest=test[,c(-1,-2)], keep.forest=TRUE, ntree=1000)

# Get confusion matrix of test set
fit_RF

# Predict the model (probability)
prob_RF = predict(fit_RF, test, type="prob")
# Predict the model (prediction)
pred_RF = predict(fit_RF, test, type="class")

# Setting up confusion matrix
conM_RF = confusionMatrix(data = as.factor(pred_RF), test$Target, positive="1")
conM_RF

# Get F1, Sensitivity, Precision Value
conM_RF$byClass

# Measure of importancs   
importance(fit_RF)

# Plot of most important variables
varImpPlot(fit_RF)

# Plot the ROC curve for Random Forest
roc_RF = prediction(prob_RF[,2],test$Target)

# 1. True Positive and Negative Rate
tpr_fpr = performance(roc_RF, "tpr","fpr")

# 2. Plot the ROC curve for RF
plot(tpr_fpr,main="ROC Curve Random Forest Model",col=2,lwd=2)
abline(a=0,b=1,lwd=1,lty=3,col="blue")

#Partial Dependency Plots for Random Forest Model

autoplot(partial(fit_RF, pred.var=c("annualised_mileage"), chull = TRUE))
autoplot(partial(fit_RF, pred.var=c("mth_since_last_serv"), chull = TRUE))
autoplot(partial(fit_RF, pred.var=c("gender"), chull = TRUE))
autoplot(partial(fit_RF, pred.var=c("num_serv_dealer_purchased"), chull = TRUE))
autoplot(partial(fit_RF, pred.var=c("age_of_vehicle_years"), chull = TRUE))

# Setting up the AUC
pred_RF <- as.numeric(pred_RF)
roc_object <- roc(response = test$Target, predictor = pred_RF)
roc_object 
plot(roc_object)

# Comparing Random Forest with Logistic Regression
model_compare_metrics<-cbind(data.frame(conM_RF$byClass),data.frame(conM_glm$byClass))
model_compare_metrics


#-------------------------------------------------------------------------------------------
#Validation Data Set
#-------------------------------------------------------------------------------------------

# Read validation Data set
validation<-read.csv("repurchase_validation.csv",header = TRUE)

# Check the data
glimpse(validation)

# Create ID column and target column in the data set
validation$ID <- seq.int(nrow(validation))
lvl<-c("0","1")
validation$Target <- factor(lvl,levels=c("0","1"))

validation <- rbind(repurchase_data[1, ] , validation)
validation <- validation[-1,]
rf_validation_pred <- predict(fit_RF,validation)
validation$target_class<-rf_validation_pred

# Calculate the Probability
rf_validation_pred_prob<-predict(fit_RF,validation,type="prob")
validation$target_probability<-rf_validation_pred_prob[,2]

#Write output
write.csv((validation%>%select(ID=ID,target_probability=target_probability,target_class=target_class)),
          file="repurchase_validation_13295403",row.names = FALSE)

pxp <- read.csv("repurchase_validation_13295403",header = TRUE)
