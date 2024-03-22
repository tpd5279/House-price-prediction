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

finalTrain.df <- subset(final.df, dataset %in% c("Train"))
finalTrain.df <- subset(finalTrain.df, select = -dataset)
finalSample.df <- subset(final.df, dataset %in% c("Sample"))
finalSample.df <- subset(finalSample.df, select = -dataset)

#set.seed(1)
set.seed(1)
train <- createDataPartition(paste(finalTrain.df$Location, sep = ""), p = 0.8, list = FALSE)
train.data <- finalTrain.df[train, ]
test.data <- finalTrain.df[-train, ]
SalePrice.train <- finalTrain.df$SalePrice[train]
SalePrice.test <- finalTrain.df$SalePrice[-train]

## Bagging with all predictors 
library(randomForest)
set.seed(6)
bag.RealEstate <- randomForest(SalePrice ~., data = train.data, mtry = 36,
                               importance = TRUE)
bag.RealEstate # %Var explain = 70.83%

yhat.bag <- predict(bag.RealEstate, newdata = test.data)
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


