rm(list = ls(all.names = TRUE))
gc()
options(digits=10)

## Install packages
install.packages(c('tibble', 'dplyr', 'readr'))
install.packages("ggplot2")
install.packages("lattice")
install.packages("car")
install.packages("glmtoolbox")
install.packages("misty")
install.packages("corrplot")
install.packages("fastDummies")
## Install libraries
library(tibble)
library(dplyr)
library(readr)
library(caret)
library(MASS)
library(dplyr)
library(car)
library(glmtoolbox)
library(stringr)
library(tidyverse)
library(misty)
library(corrplot)
library(boot)
library(fastDummies)

## Run the training data-clean up code
source("C:/Users/tinad/OneDrive - The Pennsylvania State University/Penn_State_Online_MAS/STAT_580/Project 2/Data Files/Combined_dataset_Clean-Up.R")  

################################################################################
## xgboost - with engineered OverallQual and OverallCond Variables
################################################################################

library(data.table)
library(caret)
library(xgboost)
library(Metrics)

final.df <- subset(Combined.df, select = -c(Utilities,uniqueID))
# Reduce levels of OverallQual
final.df["OverallQuality"] <- NA
final.df$OverallQuality[final.df$OverallQual %in% c("Very Poor", "Poor", "Fair", "Below Average")] <- "Below Average"
final.df$OverallQuality[final.df$OverallQual %in% c("Average")] <- "Average"
final.df$OverallQuality[final.df$OverallQual %in% c("Above Average", "Good", "Very Good", "Excellent", "Very Excellent")] <- "Above Average"
final.df$OverallQuality <- as.factor(final.df$OverallQuality)
final.df <- subset(final.df, select = -OverallQual)

# Reduce levels of OverallCond
final.df["OverallCondition"] <- NA
final.df$OverallCondition[final.df$OverallCond %in% c("Very Poor", "Poor", "Fair", "Below Average")] <- "Below Average"
final.df$OverallCondition[final.df$OverallCond %in% c("Average")] <- "Average"
final.df$OverallCondition[final.df$OverallCond %in% c("Above Average", "Good", "Very Good", "Excellent", "Very Excellent")] <- "Above Average"
final.df$OverallCondition <- as.factor(final.df$OverallCondition)
final.df <- subset(final.df, select = -OverallCond)

finalTrain.df <- subset(final.df, dataset %in% c("Train"))
finalTrain.df <- subset(finalTrain.df, select = -dataset)
finalSample.df <- subset(final.df, dataset %in% c("Sample"))
finalSample.df <- subset(finalSample.df, select = -dataset)

#set.seed(1)
set.seed(1)
train <- createDataPartition(paste(finalTrain.df$Location, sep = ""), p = 0.8, list = FALSE)
train.data <- finalTrain.df[train, ]
test.data <- finalTrain.df[-train, ]

# Create dummies for the categorical predictors
cat_data.train <- subset(train.data, select = -c(BedroomAbvGr, Fireplaces, FullBath, OpenPorchSF,
                                                 BsmtFinSF1, HalfBath, WoodDeckSF, TotRmsAbvGrd,
                                                 GrLivArea, LotArea, LotFrontage, HouseAge, SalePrice))
cat_data.train <- dummy_cols(cat_data.train)
cat_data.train <- cat_data.train[-c(1:24)]
num_data.train <- subset(train.data, select = c(BedroomAbvGr, Fireplaces, FullBath, OpenPorchSF,
                                                BsmtFinSF1, HalfBath, WoodDeckSF, TotRmsAbvGrd,
                                                GrLivArea, LotArea, LotFrontage, HouseAge, SalePrice))

training.data <- cbind(num_data.train,cat_data.train)

# Create dummy variables in the test.data
cat_data.test <- subset(test.data, select = -c(BedroomAbvGr, Fireplaces, FullBath, OpenPorchSF,
                                               BsmtFinSF1, HalfBath, WoodDeckSF, TotRmsAbvGrd,
                                               GrLivArea, LotArea, LotFrontage, HouseAge, SalePrice))
cat_data.test <- dummy_cols(cat_data.test)
cat_data.test <- cat_data.test[-c(1:24)]
num_data.test <- subset(test.data, select = c(BedroomAbvGr, Fireplaces, FullBath, OpenPorchSF,
                                              BsmtFinSF1, HalfBath, WoodDeckSF, TotRmsAbvGrd,
                                              GrLivArea, LotArea, LotFrontage, HouseAge, SalePrice))

testing.data <- cbind(num_data.test,cat_data.test)

data.full <- rbind(training.data, testing.data)
  
# Prepare the training and test data sets as matrices
SalePrice.train <- as.matrix(training.data$SalePrice)
SalePrice.test <- as.matrix(testing.data$SalePrice)
SalePrice.full <- as.matrix(data.full$SalePrice)

x.train <- as.matrix(subset(training.data, select = -SalePrice))
x.test <- as.matrix(subset(testing.data, select = -SalePrice))
x.full <- as.matrix(subset(data.full, select = -SalePrice))
  
# Prep for XGboost
dtrain <- xgb.DMatrix(x.train, label = SalePrice.train)
dtest <- xgb.DMatrix(x.test)
dfull <- xgb.DMatrix(x.full)

# Use cross validation 
tuning_para <-NULL #creating a file with tuning parameters
# Initial parameter setting
param <- list(  objective           = "reg:squarederror",
                gamma               = 0.002, # default = 0, min loss reduction required to make a further partition on a leaf node
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.05, #default = 0.3, also known as learning rate step size shrinkage, range:[0,1]
                max_depth           = 5, # default = 6, Maximum depth of a tree.
                min_child_weight    = 1, # default = 1, Minimum sum of instance weight (hessian) needed in a child
                subsample           = 0.55, # default = 1, Sub-sample ratio of the training instances
                colsample_bytree    = 0.95, # default = 1, is the subsample ratio of columns when constructing each tree. 
                tree_method = 'hist' #The tree construction algorithm used in XGBoost. 
)

#set.seed(8) - 93.2%
#set.seed(9) - 92.9%
#set.seed(1) - 92.92%
#set.seed(2) - 93.01%
#set.seed(3) - 93.01%
#set.seed(4) - 92.92%
#set.seed(888) - 92.89%
set.seed(8)
XGBm <- xgb.cv(params=param, nfold=10, nrounds=10000, missing=NA, data=dtrain, 
               print_every_n=1, early_stopping_rounds=25)

# Saving the tuning parameter results for model selection
iter_results <- data.table(t(param), best_iter = XGBm$best_iteration, rmse = XGBm$evaluation_log$test_rmse_mean[XGBm$best_iteration])
tuning_para <- rbind(tuning_para, iter_results)
tuning_para

# Fit the model to all of the training data 
watchlist <- list(train = dtrain)
set.seed(8)
XGBm.final <- xgb.train(params=param,nrounds=86,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=1)

# Make predictions
pred <- predict(XGBm.final, newdata = dtest)
pred.full <- predict(XGBm.final, newdata = dfull)
pred.train <- predict(XGBm.final, newdata = dtrain)

# Evaluating predictive performance
rmse(SalePrice.test,pred) # 13590
rsq_XGBm <- cor(SalePrice.test, pred)^2
rsq_XGBm # 0.932

rmse(SalePrice.full,pred.full) # 12795
rsq_XGBm.full <- cor(SalePrice.full, pred.full)^2
rsq_XGBm.full # 0.961

rmse(SalePrice.train, pred.train) # 12598
rsq_XGBm.train <- cor(SalePrice.train, pred.train)^2
rsq_XGBm.train # 0.9682

################################################################################
## Prediction for Sample data set
################################################################################

## Prepare the sample data set for the xgboost program

# Create dummies for the categorical predictors
cat_data.sample <- subset(finalSample.df, select = -c(BedroomAbvGr, Fireplaces, FullBath, OpenPorchSF,
                                                 BsmtFinSF1, HalfBath, WoodDeckSF, TotRmsAbvGrd,
                                                 GrLivArea, LotArea, LotFrontage, HouseAge, SalePrice))
cat_data.sample <- dummy_cols(cat_data.sample)
cat_data.sample <- cat_data.sample[-c(1:24)]
num_data.sample <- subset(finalSample.df, select = c(BedroomAbvGr, Fireplaces, FullBath, OpenPorchSF,
                                                BsmtFinSF1, HalfBath, WoodDeckSF, TotRmsAbvGrd,
                                                GrLivArea, LotArea, LotFrontage, HouseAge, SalePrice))

sample.data <- cbind(num_data.sample,cat_data.sample)

# Prepare the sample data sets as a matrix
x.sample <- as.matrix(subset(sample.data, select = -SalePrice))

# Prep for XGboost
x.dsample <- xgb.DMatrix(x.sample)

# Make SalePrice predictions for the sample data set
pred.sample <- predict(XGBm.final, newdata = x.dsample)

# Create a dataframe with uniqueID and  SalePrice prediction columns
SalePrice <- as.vector.data.frame(round(pred.sample, digits = 2))
IDnum <- as.character(as.vector(seq(1, 67, by=1)))
uniqueID <- paste("House",IDnum,sep = ".")
pred.vec <- cbind(uniqueID, SalePrice)

options(digits=10)
# Export to CSV 
write.csv(pred.vec,file='C:/Users/tinad/OneDrive - The Pennsylvania State University/Penn_State_Online_MAS/STAT_580/Project 2/Data Files/sampleSubmission_Tina.csv', row.names = FALSE)

# Read back the file
sampleSubmission <- read.csv(file="sampleSubmission_Tina.csv")
View(sampleSubmission)

# Export to CSV 
write.csv(x.sample,file='C:/Users/tinad/OneDrive - The Pennsylvania State University/Penn_State_Online_MAS/STAT_580/Project 2/Data Files/x.sample.csv', row.names = FALSE)
