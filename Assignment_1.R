# Feedback:
#   Described the pre-processing of the data and gained two insights from EDA that would be of interest to the manager.
# Properly accounted month as categorical variable in the model. 
# In the future, check the linear assumptions to check appropriateness of model.
# Train/test split data accordingly and constructed models for all industry/location combinations but did not use a loop.
# The root-mean-square error (RMSE) was correctly used to measure out of sample error.
# Made a prediction for December 2016. A plot of predicted vs actuals can assist diagnosing models, an idea for future modelling.
# Overall nice logical report and applied the CRISP framework.
# Final Grade: D

#////////////////////////////////////////////////////////
# 36106 - Data Algorithms and Meaning 
# Assignment 1 Part A: Linear Regression

# Hendrik Maximilian Gerhard Schmidt
# 13295403
#////////////////////////////////////////////////////////


library(dplyr) #for data cleaning
library(ggplot2) #for plotting
library(readr)
library(tidyr)
library(ISLR)
library(hydroGOF)
library(WriteXLS)


# Clear the global environment everything
rm(list=ls())

#////////////////////////////////////////////////////////
# Task 1a: Undertake an EDA on Dataset
#////////////////////////////////////////////////////////

# Import the data set and encode the vectors industry and location as a factors
trans_data <- read_csv("transactions.csv")

trans_data$industry <- as.factor(trans_data$industry)
trans_data$location <- as.factor(trans_data$location)
trans_data$date <- as.Date(trans_data$date, format="%d/%m/%y")

# Check for NA
apply(is.na(trans_data),2,sum)

#////////////////////////////////////////////////////////
# Task 1b: Aggregate dataset with a mean of monthly_amount
#////////////////////////////////////////////////////////

# Aggregate dataset with a mean of monthly_amount
trans_data.agg <- trans_data %>% 
  group_by(date,industry, location) %>%
  summarise(monthly_mean = mean(monthly_amount)) 

# Looking at the distribution of monthly_mean for industry and
# location.
# The mean of the monthly sales of industry 6 and 10 are more widely spread than the
# other observations. The values in industry 6 and location 10 are significantly higher.

trans_data.agg %>%
  ggplot(aes(x=industry, y=monthly_mean, color = industry)) + 
  geom_jitter(size = 1.5) +
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Mean Monthly Sales", 
       x = "Industry", 
       y = "Monthly Sales (Mean)", 
       colour = "Industry")

# The mean of the monthly sales of location 1 and industry 8 are more wideley spread than the
# other observations. The values in location 1 and industry 8 are significantly higher.

trans_data.agg %>% 
  ggplot(aes(x=location, y=monthly_mean, color=location)) + 
  geom_jitter(size = 1.5) +
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Mean Monthly Sales", 
       x = "Location", 
       y = "Monthly Sales (Mean)", 
       colour = "Location")

#Check the combinations of INDUSTRY and LOCATION for industry 6 and 10.

combination_plot <- function(trans_data.agg, i) {
  
  trans_data.agg.indu <- trans_data.agg %>% 
    filter(industry %in% i) 
    
  trans_data.agg.indu %>% 
    ggplot(aes(x=location, y=monthly_mean, color=location)) + 
    geom_jitter(size = 1.5) +
    scale_y_continuous(labels = scales::dollar) + 
    labs(title = paste("Mean Monthly Sales for Industry",i), 
         x = "Location", 
         y = "Monthly Sales (Mean)", 
         colour = "Location")
}

# Industry 10 -- Combination of industry 10 and location 8 and industry 6 
# and location 1 have the highest values. 

  trans_data.agg %>% 
  combination_plot(i=10)
  
  trans_data.agg %>% 
    combination_plot(i=1)

#///////////////////////////////////////////////////////////
# Task 2a.i: Creat an aggregated dataset using the fields date, 
# industry and location with a mean of monthly_amount (trans_data.agg), see Task 1b.
#////////////////////////////////////////////////////////
 
  trans_data.agg.11 <-  trans_data.agg %>% 
    filter(industry==1, location==1)
  
#////////////////////////////////////////////////////////
# Task 2a.ii: Create a dataset (trans_data.agg.11) and plot in which only "Industry 1" and 
# "Location 1" is shown.
#//////////////////////////////////////////////////////// 

# Add a linear regression to show a general trend within the plot 
trans_data.agg.11 %>% 
  ggplot(aes(x=date, y=monthly_mean)) + 
  geom_line(colour="blue", size=1) +
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_date(date_breaks = "3 month", date_labels = "%m/%Y") +
  labs(title = "Mean Monthly Sales for Industry 1 and Location 1 ", 
       x = "Date", 
       y = "Monthly Sales (Mean)") +
       geom_smooth(method = "lm", se = FALSE,colour = "red") +
       theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Notice the peaks in this chart for industry = 1 and location = 1.
# Seasonality could be a local trend

#//////////////////////////////////////////////////////// 
# Task 2a.iii: 
#////////////////////////////////////////////////////////   

# Training a linear regression model with mean monthly_amount as target for industry = 1 and location = 1
# Setting up features for the regression model, first create a time variable for each month and year.

trans_data.agg.features <- trans_data.agg %>% 
  mutate( year = format(date, "%Y"), 
          month = format(date, "%m"))

# Convert the month variable to a factor and the year variable to an integer. 

trans_data.agg.features$year <- as.integer(trans_data.agg.features$year)
trans_data.agg.features$month <- as.factor(trans_data.agg.features$month)


# Function for the lm (linear model) regression

fit_regression <- function (data, x_to_y){
  fit.lm <- lm(data = data, formula = x_to_y)
  return(fit.lm)
}

# Setting up the train and test set for the entire data set
# for the training set use the first 36 months of data
# for the testing set use the remaining month, ie, month 37 - 47

trainset <- trans_data.agg.features %>% 
  filter(date >= as.Date("2013-01-01") & 
           date <= as.Date("2015-12-01"))

testset <- trans_data.agg.features %>% 
  filter(date > as.Date("2015-12-01"))

# Setting up training and testing sets for industry = 1 and location = 1. 

trainset.11 <-  trainset %>% 
  filter(industry==1, location==1)

testset.11 <-  testset %>% 
  filter(industry==1, location==1)

# Call the function "fit_regression" with the time variable year and month

fit_1 <- fit_regression(trainset.11 , x_to_y = monthly_mean ~ month + year)
  mmp_fit_1 <- predict(fit_1, testset.11)
  summary(fit_1)
  rmse(testset.11$monthly_mean, mmp_fit_1)
  
# Multiple R-squared:  0.8329, Adjusted R-squared:  0.7457 , RMSE: 11304.63
# Use the predict() function to make a prediction on the test set using the trained linear model
# mmp = monthly mean prediction
  
# Call the function "fit_regression" with the time variable year^3 (polynomial) and month
  # A polynomial is used for the variable year and the prediction performs slightly better.
  
fit_2<- fit_regression(trainset.11 , x_to_y = monthly_mean ~ month + I(year^3))
  mmp_fit_2 <- predict(fit_2,testset.11)
  summary(fit_2)
  rmse(testset.11$monthly_mean, mmp_fit_2)

# Multiple R-squared:  0.8329, Adjusted R-squared:  0.7458 , RMSE: 11298.02

# Prediction for mean monthly_amount in December 2016

predict(fit_2, data.frame(year=2016, month=factor(12)),interval="prediction")

# Setting up the point for the predicition of December 2016 in the plot

date <- as.Date("2016-12-1")
monthly_mean <- 168003.5 
pred_2016 <- data.frame(date, monthly_mean)
pred_2016$date <- as.Date(pred_2016$date, format="%d/%m/%y")

# Prediction for monthly_amount in December 2016 included in the plot

trans_data.agg.11 %>% 
  ggplot(aes(x=date, y=monthly_mean)) + 
  geom_line(colour="blue", size=1) +
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_date(date_breaks = "3 month", date_labels = "%m/%Y") +
  labs(title = "Mean Monthly Sales for Industry 1 and Location 1 ", 
       x = "Date", 
       y = "Monthly Sales (Mean)") +
          geom_smooth(method = "lm", se = FALSE,colour = "red") +
          theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
          geom_point(data = pred_2016, mapping = aes(x=date, y= monthly_mean))
        
#////////////////////////////////////////////////////////
# Task 3. Applying the modelling process for industry 1 and location 1 
# to all industries and locations programmatically.
#////////////////////////////////////////////////////////

# Generate the Summary data.frame and set it to NULL
summary <-  NULL

# Loop to fit models for all industries & locations 
for (i in unique(testset$industry)) {
  for (l in unique(testset$location)) {
    #Catch error and let the loop go to the next iteration
    tryCatch({
      dum <-  NULL
      dum <-  subset(testset, industry==i & location==l)
      
        if (nrow(dum) != 0 ) {
          
          #Run model for each industry & location combination
          subset_train <- subset(trainset, industry==i & location==l)
          subset_test <- subset(testset, industry==i & location==l)
          mod <- fit_regression(subset_train , x_to_y = monthly_mean ~ month + I(year^3))
          mod_r2 <- summary(mod)$r.squared
          mod_adj.r2 <- summary(mod)$adj.r.squared
          mmp_fit <- predict(mod,subset_test)
          mmp_fit_rmse <- rmse(subset_test$monthly_mean, mmp_fit)
          predict.2016 <- predict(mod, data.frame(year=2016, month=factor(12)),interval="prediction")
          print(paste("Industry: ",i ,"Location: ",l))
          
          # Add the following measures to the final model "Summary" 
          # (Industry location combination, R^2, adj. R^2, 
          # RMSE and prediction for Dec 2016).
          summary = rbind(summary, data.frame(industry = i , location = l, R2 = mod_r2 ,adj.R2 = mod_adj.r2,
                                              RMSE = mmp_fit_rmse, fit_Dec_2016 = predict.2016[1],
                                              lwr_Dec_2016 = predict.2016[2], upr_Dec_2016 = predict.2016[3])) 
  
        }
          
          else {
           print(paste("------no data for Industry ",i," and Location ",l))
         } }, error=function(e){cat("ERROR in Industry ",i," Location ",l,
                                    "\n",conditionMessage(e), "\n")})
  }
}

# Inspect the poorest performing models based on r squared statistic.
arrange(summary, R2)

# Checking the p-value for the combination of industry = 10 and location = 7,
# because adj. r^2 is negative.

trainset.107 <-  trainset %>% 
  filter(industry==10, location==7)
testset.107 <-  testset %>% 
  filter(industry==10, location==7)

test<- fit_regression(trainset.107 , x_to_y = monthly_mean ~ month + I(year^3))
mmp_fit_test <- predict(test,testset.107)
summary(test)
rmse(testset.107$monthly_mean, mmp_fit_test)

# Lack of significance for variables leads to poor fit in some industry location combinations.
     
write_csv(summary, "./summary_and_prediction_dec_2016.csv")
WriteXLS(summary,"./summary_and_prediction_dec_2016.xls")
      