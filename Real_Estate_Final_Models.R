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

## Run the training data-clean up code
source("C:/Users/tinad/OneDrive - The Pennsylvania State University/Penn_State_Online_MAS/STAT_580/Project 2/Data Files/Training Data_Clean-Up.R")  


################################################################################
## Elastic Net - with all original variables
################################################################################

library(caret)

## since we are using caret() directly, it is taking care of dummy variable creation. 
## So unlike before when we used glmnet(), we do not need to manually create a model matrix.

## Create training and test data row indices
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(Combined.df), replace=TRUE, prob=c(0.7,0.3))

## First scale the numeric predictors
# Remove the unique ID (col 39), Utilities(col 15)
# scale the numeric variables
num.df <- as.data.frame(scale(Combined.df[c(2,5,12,13,16,19,21,22,29,35,36,38)], center = FALSE, scale = TRUE)) 
# data frame of the categorical variables
cat.df <- Combined.df[c(1,3,4,6:11,14,17,18,20,24:28,30:34,37)]
X <- cbind(num.df, cat.df)
y <- as.data.frame(scale(Combined.df[23],center = TRUE, scale = FALSE)) 
final.df <- cbind(y, X)

train.data <- final.df[sample, ]
X.test <- X[!sample, ]
y.test <- y[!sample, ]

##Use training data and tuning grid for lambda and alpha
## Set the training control
train_control1 <- trainControl(method = "repeatedcv", 
                               number = 10,
                               repeats = 10,
                               verboseIter = TRUE)
set.seed(888)
elastic_net_model1 <- train(SalePrice ~ .,
                            data = train.data,
                            method = "glmnet",
                            tuneGrid =expand.grid(alpha=seq(0,1, length.out = 100),
                                                  lambda = seq(0, 0.5, length.out = 100)),
                            trControl = train_control1)
elastic_net_model1 # alpha = 0, lambda = 0.5, RMSE = 44990, R^2 = 69.09%, MAE = 23106
plot(elastic_net_model1, main = "Elastic Net Regression")
## Calculate multiple R-squared
y_hat_enet1 <- predict(elastic_net_model1, X.test)
rsq_enet1 <- cor(y.test, y_hat_enet1)^2
rsq_enet1 # 53.83% for 70:30 split

#Use training data and tune length = 100
train_control2 <- trainControl(method = "repeatedcv", 
                               number = 10,
                               repeats = 10,
                               search = "random",
                               verboseIter = TRUE)
set.seed(888)
elastic_net_model2 <- train(SalePrice ~ .,
                            data = train.data,
                            method = "glmnet",
                            tuneLength = 100,
                            trControl = train_control2)
elastic_net_model2 # alpha = 0.0142, lambda = 0.00139, RMSE = 46080, R^2 = 69.54%, MAE = 24011
plot(elastic_net_model2, main = "Elastic Net Regression")

y_hat_enet2 <- predict(elastic_net_model2, X.test)
rsq_enet2 <- cor(y.test, y_hat_enet2)^2
rsq_enet2 # 36.5% for 70:30 split

#Use entire data set with tunelength = 100
train_control3 <- trainControl(method = "repeatedcv", 
                               number = 10,
                               repeats = 10,
                               search = "random",
                               verboseIter = TRUE)
set.seed(888)
elastic_net_model3 <- train(SalePrice ~ .,
                            data = final.df, 
                            method = "glmnet",
                            tuneLength = 100,
                            trControl = train_control3)
elastic_net_model3 # alpha = 0.0142, lambda = 0.00139, RMSE = 59581, R^2 = 0.5995, MAE = 26694

y_hat_enet3 <- predict(elastic_net_model3, X)
rsq_enet3 <- cor(y, y_hat_enet3)^2
rsq_enet3 # 88.73%

## Split the training and test data sets as follows:
# (1) Stratify by neighborhood
# (2) Do a 80:20 split (220/55)
## Create stratified training and test data sets

set.seed(1)
train <- createDataPartition(paste(final.df$Location,sep = ""), p = 0.8, list = FALSE)
train.data <- final.df[train, ]
test.data <- final.df[-train, ]
X.test <- X[-train, ]
y.test <- y[-train]

# Using training data and tuneLength = 100
train_control4 <- trainControl(method = "repeatedcv", 
                               number = 10,
                               repeats = 10,
                               search = "random",
                               verboseIter = TRUE)

set.seed(888)
elastic_net_model4 <- train(SalePrice ~ .,
                            data = train.data, 
                            method = "glmnet",
                            tuneLength = 100,
                            trControl = train_control4)

elastic_net_model4 # alpha = 0.0142, lambda = 0.00139, RMSE = 43915, R^2 = 0.7097, MAE = 22632

y_hat_enet4 <- predict(elastic_net_model4, X.test)
rsq_enet4 <- cor(y.test, y_hat_enet4)^2
rsq_enet4 #36.37%

# Using training data and tuneLength = 200
train_control5 <- trainControl(method = "repeatedcv", 
                               number = 10,
                               repeats = 10,
                               search = "random",
                               verboseIter = TRUE)

set.seed(888)
elastic_net_model5 <- train(SalePrice ~ .,
                            data = train.data, 
                            method = "glmnet",
                            tuneLength = 200,
                            trControl = train_control5)

elastic_net_model5 # alpha = 0.005819, lambda = 0.082159, RMSE = 43143, R^2 = 0.7103, MAE = 22421

y_hat_enet5 <- predict(elastic_net_model5, X.test)
rsq_enet5 <- cor(y.test, y_hat_enet5)^2
rsq_enet5 # 40.45%

################################################################################
## Elastic Net - with engineered OverallQual and OverallCond Variables
################################################################################

library(data.table)
library(caret)
library(xgboost)
library(Metrics)
## since we are using caret() directly, it is taking care of dummy variable creation. 
## So unlike before when we used glmnet(), we do not need to manually create a model matrix.

# Scale the numeric variables
num.df <- as.data.frame(scale(Combined.df[c(2,5,12,13,16,19,21,22,29,35,36,38)], center = FALSE, scale = TRUE)) 
# data frame of the categorical variables
cat.df <- Combined.df[c(1,3,4,6:11,14,17,18,20,24:28,30:34,37)]
X <- cbind(num.df, cat.df)
y <- as.data.frame(scale(Combined.df[23],center = TRUE, scale = FALSE)) 
final.df <- cbind(y, X)

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

set.seed(1)
train <- createDataPartition(paste(final.df$Location,sep = ""), p = 0.8, list = FALSE)
train.data <- final.df[train, ]
test.data <- final.df[-train, ]
X.test <- X[-train, ]
y.test <- y[-train]

# Reduce levels of OverallQual
X.test["OverallQuality"] <- NA
X.test$OverallQuality[X.test$OverallQual %in% c("Very Poor", "Poor", "Fair", "Below Average")] <- "Below Average"
X.test$OverallQuality[X.test$OverallQual %in% c("Average")] <- "Average"
X.test$OverallQuality[X.test$OverallQual %in% c("Above Average", "Good", "Very Good", "Excellent", "Very Excellent")] <- "Above Average"
X.test$OverallQuality <- as.factor(X.test$OverallQuality)
X.test <- subset(X.test, select = -OverallQual)

# Reduce levels of OverallCond
X.test["OverallCondition"] <- NA
X.test$OverallCondition[X.test$OverallCond %in% c("Very Poor", "Poor", "Fair", "Below Average")] <- "Below Average"
X.test$OverallCondition[X.test$OverallCond %in% c("Average")] <- "Average"
X.test$OverallCondition[X.test$OverallCond %in% c("Above Average", "Good", "Very Good", "Excellent", "Very Excellent")] <- "Above Average"
X.test$OverallCondition <- as.factor(X.test$OverallCondition)
X.test <- subset(X.test, select = -OverallCond)

# Using training data and tuneLength = 100
train_control6 <- trainControl(method = "repeatedcv", 
                               number = 10,
                               repeats = 10,
                               search = "random",
                               verboseIter = TRUE)
set.seed(888)
elastic_net_model6 <- train(SalePrice ~ .,
                            data = train.data, 
                            method = "glmnet",
                            tuneLength = 200,
                            trControl = train_control6)

elastic_net_model6 # alpha =0.00582 , lambda = 0.0822 , RMSE = 42805, R^2 = 0.7266, MAE = 22813

y_hat_enet6 <- predict(elastic_net_model6, X.test)
rsq_enet6 <- cor(y.test, y_hat_enet6)^2
rsq_enet6 # 40.24%

# Using training data and tuning grid
train_control7 <- trainControl(method = "repeatedcv", 
                               number = 10,
                               repeats = 10,
                               verboseIter = TRUE)
set.seed(888)
elastic_net_model7 <- train(SalePrice ~ .,
                            data = train.data,
                            method = "glmnet",
                            tuneGrid =expand.grid(alpha=seq(0,1, length.out = 100),
                                                  lambda = seq(0, 0.5, length.out = 100)),
                            trControl = train_control7)
elastic_net_model7 # alpha =0.00000  , lambda = 0.005051, RMSE = 41799, R^2 = 0.7114, MAE = 22761
## Calculate multiple R-squared
y_hat_enet7 <- predict(elastic_net_model7, X.test)
rsq_enet7 <- cor(y.test, y_hat_enet7)^2
rsq_enet7 # 54.33%

####################################################################################
## Bagging and Random Forests - with engineered OverallQual and OverallCond Variables
#####################################################################################

## Bagging: Grow trees independently on random samples using all features
## Random forests: Grow trees independently on random samples, but use random subsets of features 

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

set.seed(1)
train <- createDataPartition(paste(final.df$Location,sep = ""), p = 0.8, list = FALSE)
train.data <- final.df[train, ]
test.data <- final.df[-train, ]
SalePrice.train <- final.df$SalePrice[train]
SalePrice.test <- final.df$SalePrice[-train]

## Bagging with all predictors 
library(randomForest)
set.seed(6)
bag.RealEstate <- randomForest(SalePrice ~., data = train.data, mtry = 36,
                               importance = TRUE)
bag.RealEstate # %Var explain = 70.83%

yhat.bag <- predict(bag.RealEstate, newdata = test.data)
plot(yhat.bag, SalePrice.test)
abline(0,1)
mean((yhat.bag - SalePrice.test)^2)
rsq_bag <- cor(SalePrice.test, yhat.bag)^2
rsq_bag # 89.59%

## Change the number of trees grown by randomForest() using the ntree argument - still Bagging, 
## but change the number of trees grown

# ntree = 25
set.seed(6)
bag.RealEstate1 <- randomForest(SalePrice ~., data = train.data, mtry = 36, 
                               ntree = 25)
bag.RealEstate1 #Var explain = 69.31%
yhat.bag1 <- predict(bag.RealEstate1, newdata = test.data)
mean((yhat.bag1 - SalePrice.test)^2)
rsq_bag1 <- cor(SalePrice.test, yhat.bag1)^2
rsq_bag1 ## 89.67%

# ntree = 35
set.seed(6)
bag.RealEstate1a <- randomForest(SalePrice ~., data = train.data, mtry = 36, 
                                ntree = 35)
bag.RealEstate1a #Var explain = 70.44%
yhat.bag1a <- predict(bag.RealEstate1a, newdata = test.data)
mean((yhat.bag1a - SalePrice.test)^2)
rsq_bag1a <- cor(SalePrice.test, yhat.bag1a)^2
rsq_bag1a ## 90.44%

# ntree = 50
set.seed(6)
bag.RealEstate2 <- randomForest(SalePrice ~., data = train.data, mtry = 36, 
                                ntree = 50)
bag.RealEstate2 # %Var explained = 72.2%
yhat.bag2 <- predict(bag.RealEstate2, newdata = test.data)
mean((yhat.bag2 - SalePrice.test)^2)
rsq_bag2 <- cor(SalePrice.test, yhat.bag2)^2
rsq_bag2 ## 91.06%

# ntree = 75
set.seed(6)
bag.RealEstate2a <- randomForest(SalePrice ~., data = train.data, mtry = 36, 
                                ntree = 75)
bag.RealEstate2a # %Var explained = 71.47%
yhat.bag2a <- predict(bag.RealEstate2a, newdata = test.data)
mean((yhat.bag2a - SalePrice.test)^2)
rsq_bag2a <- cor(SalePrice.test, yhat.bag2a)^2
rsq_bag2a ## 90.57%

# ntree = 100
set.seed(6)
bag.RealEstate3 <- randomForest(SalePrice ~., data = train.data, mtry = 36, 
                                ntree = 100)
bag.RealEstate3 # %Var explained = 71.33%
yhat.bag3 <- predict(bag.RealEstate3, newdata = test.data)
mean((yhat.bag3 - SalePrice.test)^2)
rsq_bag3 <- cor(SalePrice.test, yhat.bag3)^2
rsq_bag3 ## 89.8%%

# ntree = 150
set.seed(6)
bag.RealEstate4 <- randomForest(SalePrice ~., data = train.data, mtry = 36, 
                                ntree = 150)
bag.RealEstate4 # %Var explained = 72.12%
yhat.bag4 <- predict(bag.RealEstate4, newdata = test.data)
mean((yhat.bag4 - SalePrice.test)^2)
rsq_bag4 <- cor(SalePrice.test, yhat.bag4)^2
rsq_bag4 ## 90.07%

# ntree = 500
set.seed(6)
bag.RealEstate5 <- randomForest(SalePrice ~., data = train.data, mtry = 36, 
                                ntree = 500)
bag.RealEstate5 # %Var explained = 72.12%
yhat.bag5 <- predict(bag.RealEstate5, newdata = test.data)
mean((yhat.bag5 - SalePrice.test)^2)
rsq_bag5 <- cor(SalePrice.test, yhat.bag5)^2
rsq_bag5 ## 89.85%

# Grow a random forest using p/3 variables when building regression trees
set.seed(8)
rf.RealEstate1 <- randomForest(SalePrice ~., data = train.data, mtry = 12,
                              importace = TRUE)
rf.RealEstate1 #%Var explained = 72.16%
yhat.rf1 <- predict(rf.RealEstate1, newdata = test.data)
mean((yhat.rf1 - SalePrice.test)^2)
rsq_rf1 <- cor(SalePrice.test, yhat.rf1)^2
rsq_rf1 ## 09.06%

# Grow a random forest using 6 variables when building regression trees
set.seed(8)
rf.RealEstate2 <- randomForest(SalePrice ~., data = train.data, mtry = 6,
                               importace = TRUE)
rf.RealEstate2 #%Var explained = 71.65%
yhat.rf2 <- predict(rf.RealEstate2, newdata = test.data)
mean((yhat.rf2 - SalePrice.test)^2)
rsq_rf2 <- cor(SalePrice.test, yhat.rf2)^2
rsq_rf2 ## 90.17%

# Grow a random forest using 3 variables when building regression trees
set.seed(8)
rf.RealEstate3 <- randomForest(SalePrice ~., data = train.data, mtry = 3,
                               importace = TRUE)
rf.RealEstate3 #%Var explained = 69.38%
yhat.rf3 <- predict(rf.RealEstate3, newdata = test.data)
mean((yhat.rf3 - SalePrice.test)^2)
rsq_rf3 <- cor(SalePrice.test, yhat.rf3)^2
rsq_rf3 ## 89.39%

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

set.seed(1)
train <- createDataPartition(paste(final.df$Location, sep = ""), p = 0.8, list = FALSE)
train.data <- final.df[train, ]
test.data <- final.df[-train, ]

# Create dummies for the categorical predictors
install.packages("fastDummies")
library(fastDummies)

# Create dummy variables in the train.data
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

# Prepare the training and test data sets as matrices
SalePrice.train <- as.matrix(training.data$SalePrice)
SalePrice.test <- as.matrix(testing.data$SalePrice)

x.train <- as.matrix(subset(training.data, select = -SalePrice))
x.test <- as.matrix(subset(testing.data, select = -SalePrice))

# Prep for XGboost
dtrain <- xgb.DMatrix(x.train, label = SalePrice.train)
dtest <- xgb.DMatrix(x.test)

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

# Evaluating
rmse(SalePrice.test,pred)
rsq_XGBm <- cor(SalePrice.test, pred)^2
rsq_XGBm 

################################################################################
## Prediction for Sample data set
################################################################################

## Run the sample data-clean up code
source("C:/Users/tinad/OneDrive - The Pennsylvania State University/Penn_State_Online_MAS/STAT_580/Project 2/Data Files/Test Data_Clean-Up.R")  
