rm(list = ls(all.names = TRUE))
gc()
options(digits=12)

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

## since we are using caret() directly, it is taking care of dummy variable creation. 
## So unlike before when we used glmnet(), we do not need to manually create a model matrix.
# Scale the numeric variables
numVar.df <- subset(final.df, select = c(BedroomAbvGr,Fireplaces,FullBath,OpenPorchSF,BsmtFinSF1,HalfBath,WoodDeckSF,
                                      TotRmsAbvGrd,GrLivArea,LotArea,LotFrontage,HouseAge))
numX.df <- as.data.frame(scale(numVar.df, center = FALSE, scale = TRUE)) 

# Data frame of the categorical variables
catX.df <- subset(final.df, select = -c(BedroomAbvGr,Fireplaces,FullBath,OpenPorchSF,BsmtFinSF1,HalfBath,
                                          WoodDeckSF,TotRmsAbvGrd,GrLivArea,LotArea,LotFrontage,HouseAge, 
                                          SalePrice))

X <- cbind(numX.df, catX.df)
y <- as.data.frame(scale(subset(final.df, select = SalePrice),center = TRUE, scale = FALSE)) 
final1.df <- cbind(y, X)

finalTrain.df <- subset(final1.df, dataset %in% c("Train"))
finalTrain.df <- subset(finalTrain.df, select = -dataset)
finalSample.df <- subset(final1.df, dataset %in% c("Sample"))
finalSample.df <- subset(finalSample.df, select = -dataset)

# Create training and test data sets
set.seed(1)
train <- createDataPartition(paste(finalTrain.df$Location,sep = ""), p = 0.8, list = FALSE)
train.data <- finalTrain.df[train, ]
test.data <- finalTrain.df[-train, ]
X.test <- subset(test.data, select=-SalePrice)
y.test <- subset(test.data, select=SalePrice)

################################################################################
## Elastic Net Regression
################################################################################

library(data.table)
library(caret)
library(xgboost)
library(Metrics)
## since we are using caret() directly, it is taking care of dummy variable creation. 
## So unlike before when we used glmnet(), we do not need to manually create a model matrix.

# Using tuneLength = 500
train_control1 <- trainControl(method = "repeatedcv", 
                               number = 10,
                               repeats = 10,
                               search = "random",
                               verboseIter = TRUE)
set.seed(8)
elastic_net_model1 <- train(SalePrice ~ .,
                            data = train.data, 
                            method = "glmnet",
                            tuneLength = 500,
                            trControl = train_control1)

elastic_net_model1 #Fitting alpha = 0.001287, lambda = 7.2521825, RMSE = 42736, R^2 = 71.38, MAE = 23244
y_hat_enet1 <- predict(elastic_net_model1, X.test)
rsq_enet1 <- cor(y.test, y_hat_enet1)^2
rsq_enet1 # 52.38%

# Using tuning grid
train_control2 <- trainControl(method = "repeatedcv", 
                               number = 10,
                               repeats = 10,
                               search = "random",
                               verboseIter = TRUE)
set.seed(8)
elastic_net_model2 <- train(SalePrice ~ .,
                            data = train.data, 
                            method = "glmnet",
                            tuneGrid =expand.grid(alpha=seq(0,1, length.out = 100),
                                                  lambda = c(0, 0.01, 0.05, 0.1, 0.2, 0.5, 1, 10, 20, 30, 40, 50, 100)),
                            trControl = train_control2)

elastic_net_model2 #Fitting alpha = 0, lambda = , RMSE = 23261, R^2 = 71.09, MAE = 23261
y_hat_enet2 <- predict(elastic_net_model2, X.test)
rsq_enet2 <- cor(y.test, y_hat_enet2)^2
rsq_enet2 # 55.21