#######################################################################################################################
## Project 2: Real Estate Data Analysis - Decision Trees
## Author: Tina Dhekial-Phukan
#######################################################################################################################

rm(list = ls(all.names = TRUE))
gc()
options(digits=4)

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

## Run the data-clean up code
source("C:/Users/tinad/OneDrive - The Pennsylvania State University/Penn_State_Online_MAS/STAT_580/Project 2/Data Files/Training Data_Clean-Up.R")  

################################################################################
## Fit a regression tree with SalePrice as the response variable 
################################################################################

library(tree)

## Fit a regression tree and create a training and test data sets

set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(Combined.df), replace=TRUE, prob=c(0.7,0.3))
final.df <- subset(Combined.df, select = -c(Utilities,uniqueID))
train.data <- final.df[sample, ]
test.data <- final.df[!sample, ]
SalePrice.train <- final.df$SalePrice[sample]
SalePrice.test <- final.df$SalePrice[!sample]
SalePrice.fulltest <- final.df$SalePrice

tree.RealEstate <- tree(SalePrice ~., train.data, split = "deviance")
summary(tree.RealEstate)
tree.RealEstate

# Plot the tree
par(mfrow = c(1,1))
plot(tree.RealEstate)
text(tree.RealEstate, pretty = 0, cex= 0.5)

# Compute test data set error rate for the unpruned tree
yhat <- predict(tree.RealEstate, test.data)
mean((yhat - SalePrice.test)^2)
rsq <- cor(SalePrice.test, yhat)^2
rsq # 66.23%

# Select the most complex tree under consideration by cross validation.
# Use the full data set
set.seed(2)
tree.RealEstate <- tree(SalePrice ~., final.df, split = "deviance")
cv.RealEstate <- cv.tree(tree.RealEstate, K = 10)
plot(cv.RealEstate$size, cv.RealEstate$dev, type = "b") # Recommended best size = 2, 5, 9 or 10

# Prune the most complex tree by using the best size obtained from the cross-validation
prune.RealEstate <- prune.tree(tree.RealEstate , best = 10)
plot(prune.RealEstate)
text(prune.RealEstate, pretty = 0, cex= 0.5)

prune.RealEstateSize2 <- prune.tree(tree.RealEstate, best = 2)
prune.RealEstateSize5 <- prune.tree(tree.RealEstate, best = 5)
prune.RealEstateSize9 <- prune.tree(tree.RealEstate, best = 9)
prune.RealEstateSize10 <- prune.tree(tree.RealEstate, best = 10)

# Compute test error rate for the pruned tree using the full data set
yhatSize2 <- predict(prune.RealEstateSize2, final.df) 
mean((yhatSize2 - SalePrice.fulltest)^2)
rsqSize2 <- cor(SalePrice.fulltest, yhatSize2)^2
rsqSize2 # 47.27%

yhatSize5 <- predict(prune.RealEstateSize5, final.df) 
mean((yhatSize5 - SalePrice.fulltest)^2)
rsqSize5 <- cor(SalePrice.fulltest, yhatSize5)^2
rsqSize5 # 67.65%

yhatSize9 <- predict(prune.RealEstateSize9, final.df) 
mean((yhatSize9 - SalePrice.fulltest)^2)
rsqSize9 <- cor(SalePrice.fulltest, yhatSize9)^2
rsqSize9 # 77.44%

yhatSize10 <- predict(prune.RealEstateSize10, final.df) 
mean((yhatSize10 - SalePrice.fulltest)^2)
rsqSize10 <- cor(SalePrice.fulltest, yhatSize10)^2
rsqSize10 # 78.61%

# Select the most complex tree under consideration by cross validation.
# Use the training data set
set.seed(2)
tree.RealEstate1 <- tree(SalePrice ~., train.data, split = "deviance")
cv.RealEstate1 <- cv.tree(tree.RealEstate1, K = 10)
plot(cv.RealEstate1$size, cv.RealEstate1$dev, type = "b") # Recommended best size = 2, 5, 7

# Prune the most complex tree by using the best size obtained from the cross-validation
prune.RealEstate1 <- prune.tree(tree.RealEstate1, best = 2)
prune.RealEstate2 <- prune.tree(tree.RealEstate1, best = 5)
prune.RealEstate3 <- prune.tree(tree.RealEstate1, best = 7)

# Compute test error rate for the pruned tree using the test data set
yhatSize2 <- predict(prune.RealEstate1, test.data) 
mean((yhatSize2 - SalePrice.test)^2)
rsqSize2 <- cor(SalePrice.test, yhatSize2)^2
rsqSize2 # 47.7%

yhatSize5 <- predict(prune.RealEstate2, test.data) 
mean((yhatSize5 - SalePrice.test)^2)
rsqSize5 <- cor(SalePrice.test, yhatSize5)^2
rsqSize5 # 56.73%

yhatSize7 <- predict(prune.RealEstate3, test.data) 
mean((yhatSize7 - SalePrice.test)^2)
rsqSize7 <- cor(SalePrice.test, yhatSize7)^2
rsqSize7 # 59.92%

################################################################################
## Bagging and Random Forests 
################################################################################

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

# Create the training and test data sets
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(final.df), replace=TRUE, prob=c(0.7,0.3))
train.data <- final.df[sample, ]
test.data <- final.df[!sample, ]
SalePrice.train <- final.df$SalePrice[sample]
SalePrice.test <- final.df$SalePrice[!sample]

library(randomForest)
set.seed(1)
bag.RealEstate <- randomForest(SalePrice ~., data = train.data, mtry = 36,
                               importance = TRUE)
bag.RealEstate # % Var explained: 68.58

yhat.bag <- predict(bag.RealEstate, newdata = test.data)
plot(yhat.bag, SalePrice.test)
abline(0,1)
mean((yhat.bag - SalePrice.test)^2)
rsq_bag <- cor(SalePrice.test, yhat.bag)^2
rsq_bag ## 85.92%

# Change the number of trees grown by randomForest() using the 
# ntree argument

bag.RealEstate <- randomForest(SalePrice ~., data = train.data,mtry = 36, 
                               ntree = 25)
yhat.bag <- predict(bag.RealEstate, newdata = test.data)
mean((yhat.bag - SalePrice.test)^2)
rsq_bag <- cor(SalePrice.test, yhat.bag)^2
rsq_bag ## 85.57%

# Grow a random forest using p/3 variables when building regression trees
set.seed(1)
rf.RealEstate <- randomForest(SalePrice ~., data = train.data, mtry = 12,
                              importace = TRUE)
yhat.rf <- predict(rf.RealEstate, newdata = test.data)
mean((yhat.rf - SalePrice.test)^2)
rsq_rf <- cor(SalePrice.test, yhat.rf)^2
rsq_rf ## 86.38%
importance(rf.RealEstate, type = 3)
varImpPlot(rf.RealEstate)

################################################################################
## Boosting 
################################################################################

# Use original data and don't draw any random samples. Trees are grown successively
# and use a slow learning approach.

install.packages("gbm")
library(gbm)

set.seed(1)
boost.RealEstate <- gbm(SalePrice ~., data = train.data, distribution = "gaussian",
                        n.trees = 5000, interaction.depth = 4)
summary(boost.RealEstate)
plot(boost.RealEstate, i = "GrLivArea")
plot(boost.RealEstate, i = "OverallQual")
plot(boost.RealEstate, i = "HouseAge")

yhat.boost <- predict(boost.RealEstate, newdata = test.data, n.trees = 5000)
mean((yhat.boost - SalePrice.test)^2)
rsq_boost <- cor(SalePrice.test, yhat.boost)^2
rsq_boost ## 80.11%

## Tuning the shrinkage parameter -Trial 1
hyper_grid <- expand.grid(
  learning_rate = c(0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05, 0.001),
  n.minobsinnode = c(5, 10, 15),
  int_depth = c(3,4,5,6,7),
  min_train.error = 0  # a place to hold the error results
)

# total number of combinations
nrow(hyper_grid)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  # reproducibility
  set.seed(123)
  # train model
  gbm.tune <- gbm(
    formula = SalePrice ~ .,
    distribution = "gaussian",
    data = train.data,
    n.trees = 5000,
    interaction.depth = hyper_grid$int_depth[i],
    shrinkage = hyper_grid$learning_rate[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = 0.5,
    cv.folds = 10,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  # compute RMSE
  # add SSE, trees, and training time to results
  hyper_grid$min_train.error[i] <- which.min(gbm.tune$train.error)
}

hyper_grid %>% 
  dplyr::arrange(min_train.error) %>%
  head(10)

# for reproducibility
set.seed(123)
# train final GBM model with the tuned parameters
gbm.fit.final <- gbm(
  formula = SalePrice ~ .,
  distribution = "gaussian",
  data = train.data,
  n.trees = 5000,
  interaction.depth = 5,
  shrinkage = 0.4,
  n.minobsinnode = 5,
  bag.fraction = 0.5, 
  train.fraction = 0.7,
  cv.folds = 10,
  n.cores = NULL, # will use all cores by default
  verbose = TRUE
)  

par(mar = c(5, 8, 1, 1))
summary(gbm.fit.final, cBars = 10, method = relative.influence, las = 2)

install.packages("vip")
library(vip)
vip::vip(gbm.fit.final)

install.packages("vip")
library(vip)
vip::vip(gbm.fit.final)

## Partial dependence plots for "Insulin", "Glucose", "Age" and "SkinThickness"
plot(gbm.fit.final, i = "OverallQual")
plot(gbm.fit.final, i = "GrLivArea")
plot(gbm.fit.final, i = "KitchenQual")
plot(gbm.fit.final, i = "HouseAge")

## Use the tuned model to predict "SalePrice" on the test set. 
yhat.tunedboost <- predict(gbm.fit.final, newdata = test.data)
mean((yhat.tunedboost - SalePrice.test)^2)
rsq_tboost <- cor(SalePrice.test, yhat.tunedboost)^2
rsq_tboost ## 64.86%
importance(rf.RealEstate, type = 3)
varImpPlot(rf.RealEstate)

## Tuning the shrinkage parameter -Trial 2
hyper_grid2 <- expand.grid(
  learning_rate = c(0.5, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05, 0.01, 0.001),
  n.minobsinnode = c(5, 10, 15),
  int_depth = c(3,4,5,6,7),
  min_train.error = 0,  # a place to hold the error results
  RMSE = 0,
  trees = 0
)

# total number of combinations
nrow(hyper_grid2)

# grid search 
for(i in 1:nrow(hyper_grid2)) {
  # reproducibility
  set.seed(123)
  # train model
  gbm.tune2 <- gbm(
    formula = SalePrice ~ .,
    distribution = "gaussian",
    data = train.data,
    n.trees = 6000,
    interaction.depth = hyper_grid2$int_depth[i],
    shrinkage = hyper_grid2$learning_rate[i],
    n.minobsinnode = hyper_grid2$n.minobsinnode[i],
    bag.fraction = 0.5,
    cv.folds = 10,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  # compute RMSE
  # add SSE, trees, and training time to results
  hyper_grid2$min_train.error[i] <- which.min(gbm.tune$train.error)
  hyper_grid2$RMSE[i]  <- sqrt(min(gbm.tune$cv.error))
  hyper_grid2$trees[i] <- which.min(gbm.tune$cv.error)
}

hyper_grid2 %>% 
  dplyr::arrange(RMSE) %>%
  head(100)

# for reproducibility
set.seed(123)
# train final GBM model with the tuned parameters
gbm.fit.final2 <- gbm(
  formula = SalePrice ~ .,
  distribution = "gaussian",
  data = train.data,
  n.trees = 6000,
  interaction.depth = 7,
  shrinkage = 0.001,
  n.minobsinnode = 5,
  bag.fraction = 0.5, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = TRUE
)  

## Use the tuned model to predict "SalePrice" on the test set. 
yhat.tunedboost2 <- predict(gbm.fit.final2, newdata = test.data)
mean((yhat.tunedboost2 - SalePrice.test)^2)
rsq_tboost2 <- cor(SalePrice.test, yhat.tunedboost2)^2
rsq_tboost2 

## ntrees = 5000,  Shrinkage = 0.5, rsq = 70.07%
## ntrees = 5000,Shrinkage = 0.45, rsq = 61.84
## ntrees = 5000,Shrinkage = 0.4, rsq = 73.12%
## ntrees = 5000,Shrinkage = 0.35, rsq = 71.4%
## ntrees = 5000,Shrinkage = 0.30, rsq = 72.34%
## ntrees = 5000,Shrinkage = 0.25, rsq = 70.05%
## ntrees = 5000,Shrinkage = 0.20, rsq = 71.99%
## ntrees = 5000,Shrinkage = 0.15, rsq = 75.64%
## ntrees = 5000,Shrinkage = 0.10, rsq = 78.34%
## ntrees = 5000,Shrinkage = 0.05, rsq = 80.04%
## ntrees = 5000,Shrinkage = 0.025, rsq = 80.17%
## ntrees = 5000,Shrinkage = 0.01, rsq = 80.57%
## ntrees = 5000,Shrinkage = 0.001, rsq = 83.2%
## ntrees = 5000,Shrinkage = 0.0001, rsq = 70.74%
## ntrees = 6000,Shrinkage = 0.001, rsq = 83.53%
## ntrees = 6000,Shrinkage = 0.001, int.depth = 4, rsq = 84.06%
## ntrees = 6000,Shrinkage = 0.001, int.depth = 7, rsq = 84.71%

################################################################################
## xgboost 
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

# Create dummies for the categorical predictors
install.packages("fastDummies")
library(fastDummies)
# Create dummies for the categorical predictors
install.packages("fastDummies")
library(fastDummies)

# Separate out the numeric and categorical variables
cat_data <- subset(final.df, select = -c(BedroomAbvGr, Fireplaces, FullBath, OpenPorchSF,
                                         BsmtFinSF1, HalfBath, WoodDeckSF, TotRmsAbvGrd,
                                         GrLivArea, LotArea, LotFrontage, HouseAge, SalePrice))
cat_data <- dummy_cols(cat_data)
cat_data <- cat_data[-c(1:24)]
num_data <- subset(final.df, select = c(BedroomAbvGr, Fireplaces, FullBath, OpenPorchSF,
                                        BsmtFinSF1, HalfBath, WoodDeckSF, TotRmsAbvGrd,
                                        GrLivArea, LotArea, LotFrontage, HouseAge, SalePrice))

final.df1 <- cbind(num_data,cat_data)


# Create the training and test data sets
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(final.df1), replace=TRUE, prob=c(0.7,0.3))
train.data <- final.df1[sample, ]
test.data <- final.df1[!sample, ]
SalePrice.train <- as.matrix(final.df1$SalePrice[sample])
SalePrice.test <- as.matrix(final.df1$SalePrice[!sample])

x.train <- as.matrix(subset(train.data, select = -SalePrice))
x.test <- as.matrix(subset(test.data, select = -SalePrice))

# Prep for XGboost
dtrain <- xgb.DMatrix(x.train, label = SalePrice.train)
dtest <- xgb.DMatrix(x.test)

# Use cross validation 
tuning_para <-NULL #creating a file with tuning parameters
# Initial parameter setting
param <- list(  objective           = "reg:squarederror",
                gamma               = c(0.002, 0.02, 0.05, 0.1, 0.2), # default = 0, min loss reduction required to make a further partition on a leaf node
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = c(0.002, 0.02, 0.05, 0.1, 0.2, 0.3), #default = 0.3, also known as learning rate step size shrinkage, range:[0,1]
                max_depth           = c(20, 10, 5, 2), # default = 6, Maximum depth of a tree.
                min_child_weight    = c(1, 5, 10, 25, 50), # default = 1, Minimum sum of instance weight (hessian) needed in a child
                subsample           = c(1.0, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5), # default = 1, Sub-sample ratio of the training instances
                colsample_bytree    = c(1.0, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5), # default = 1, is the subsample ratio of columns when constructing each tree. 
                tree_method = 'hist' #The tree construction algorithm used in XGBoost. 
)

XGBm <- xgb.cv(params=param,nfold=5,nrounds=10000,missing=NA,data=dtrain,
               print_every_n=1, early_stopping_rounds=25)

# Saving the tuning parameter results for model selection
iter_results <- data.table(t(param), best_iter = XGBm$best_iteration, rmse = XGBm$evaluation_log$test_rmse_mean[XGBm$best_iteration])
tuning_para <- rbind(tuning_para, iter_results)
tuning_para

# Fit the model to all of the data 
watchlist <- list(train = dtrain)
XGBm <- xgb.train(params=param,nrounds=2442,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=1)

# Make predictions
pred <- predict(XGBm, newdata = dtest)

# Evaluating
rmse(SalePrice.test,pred)
rsq_XGBm <- cor(SalePrice.test, pred)^2
rsq_XGBm 

