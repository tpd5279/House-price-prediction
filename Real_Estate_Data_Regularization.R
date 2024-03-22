#######################################################################################################################
## Project 2: Real Estate Data Analysis - Regularization
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
## Correlation of numeric variables
################################################################################

## Create subset of the numeric variables names
num_var <- names(Combined.df[c(2, 5, 12, 13, 16, 19, 21, 22, 23, 29, 35, 36, 38)])
num_vec <- as.vector(num_var)

# Compute Pearson's correlation matrix 
library(corrplot)
corr_matrix1 <- cor(subset(Combined.df, select=c(num_vec)), method = "pearson")
par(mfrow=c(1,1))
corrplot(corr_matrix1, method = 'number', tl.cex=0.6, number.cex = 0.55, type = "lower",
         diag = FALSE,col = colorRampPalette(c("white", "orange", "red"))(100))
# Correlated variables
# TotRmsAbvGrd & BedroomAbvGr, r = 0.66
# GrLivAre & BedroomAbvGr, r = 0.66
# TotRmsAbvGrd & FullBath, r = 0.55
# SalePrice & FullBath, r = 0.64
# GrLivArea & FullBath, r = 0.58
# GrLivArea & TotRmsAbvGrd, r = 0.81
# GrLivArea & SalePrice, r = 0.58
# LotArea & GrLivArea, r = 0.58
# LotFrontage & LotArea, r = 0.71

## New data set with missing values imputed to the median
par(mfrow=c(3,3))
for (v in vnames) 
{
  hist(wiki.medNA[, v], main="", ylab="Freq", xlab = paste(v, "_medNA" , sep=""))
}


##################################################################################
## Setting random number generator option and ridge regression and LASSO libraries
##################################################################################
RNGversion("3.6.0")
install.packages("Matrix")
install.packages("KRMM")
install.packages("glmmLasso")
library(glmnet)
library(psych)
library(KRMM)
library(glmmLasso)

################################################################################
## Multiple Linear Regression
################################################################################

################################################################################
## Ridge Regression
################################################################################

x <- model.matrix(SalePrice~ ., Combined.df[-c(15, 39)])[, -1] #[, -1] excludes duplication of intercept
y <- Combined.df$SalePrice
grid <- 10^seq(10, -2, length = 100)

## Create training and test data sets
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(Combined.df), replace=TRUE, prob=c(0.7,0.3))

## Using Validation Set Approach
ridge.mod <- glmnet(x[sample, ], y[sample], alpha = 0, lambda = grid, thresh = 1e-12, standardize = TRUE)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[!sample, ]) # For lambda = 4
mean((ridge.pred - y[!sample])^2)

## Plot coefficients by value of lambda parameter
ridge.mod <- glmnet(x[sample, ], y[sample], alpha = 0, lambda = grid, thresh = 1e-12, standardize = TRUE)
par(mfrow=c(1,1))
plot(ridge.mod, xvar="lambda", main="Coefficients by lambda parameter", ylim = c(-1e+05, 1e+05))

## Select best lambda using cross-validation
cv.ridge.out <- cv.glmnet(x[sample, ], y[sample], alpha = 0, nfold = 10)
par(mfrow=c(1,1))
plot(cv.ridge.out, main="Ridge MSE by lambda parameter")
ridgeLAMDA.min <- cv.ridge.out$lambda.min
ridgeLAMDA.min # 259548
ridgeLAMDA.1se <- cv.ridge.out$lambda.1se
ridgeLAMDA.1se # 1262077

## Calculate test MSE using lambda minimum
ridge.pred <- predict(ridge.mod, s = ridgeLAMDA.min, newx = x[!sample, ])
mse.ridge <- mean((ridge.pred - y[!sample])^2)
mse.ridge

## Calculate test MSE using lambda 1se
ridge.pred1 <- predict(ridge.mod, s = ridgeLAMDA.1se, newx = x[!sample, ])
mse.ridge1 <- mean((ridge.pred1 - y[!sample])^2)
mse.ridge1


## Finally, we refit our ridge regression model on the full data set using
## ridgeLAMDA.min
ridge.out <- glmnet(x, y, alpha = 0)
ridgecoefs = predict(ridge.out, type="coefficients", s=ridgeLAMDA.min)
ridgecoefs
ridge.final.pred <- predict(ridge.out, type = "coefficients", 
                            s = ridgeLAMDA.min)[1:84, ]
ridge.final.pred

plot(ridge.out, xvar="lambda", label=TRUE, main="Ridge coefficients")
abline(v=log(ridgeLAMDA.min), lty=2)

################################################################################
## Ridge Regression - Part 2
################################################################################

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

x_var <- model.matrix(SalePrice~ ., final.df)[, -1] #[, -1] excludes duplication of intercept
y_var <- Combined.df$SalePrice

## Select best lambda using cross-validation
cv.ridge.out <- cv.glmnet(x_var[sample, ], y_var[sample], alpha = 0, nfold = 10)
par(mfrow=c(1,1))
plot(cv.ridge.out, main="Ridge MSE by lambda parameter")
ridgeLAMDA.min <- cv.ridge.out$lambda.min
ridgeLAMDA.min # 215481
ridgeLAMDA.1se <- cv.ridge.out$lambda.1se
ridgeLAMDA.1se # 1262077

## Plot coefficients by value of lambda parameter
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x_var[sample, ], y_var[sample], alpha = 0, lambda = grid, thresh = 1e-12)
par(mfrow=c(1,1))
plot(ridge.mod, xvar="lambda", main="Coefficients by lambda parameter", ylim = c(-1e+05, 1e+05))

## Calculate test MSE using lambda minimum
ridge.pred <- predict(ridge.mod, s = ridgeLAMDA.min, newx = x_var[!sample, ])
mse.ridge <- mean((ridge.pred - y_var[!sample])^2)
mse.ridge # MSE = 756539519, RMSE = 27505.26348
rsq_ridge <- cor(ridge.pred, y_var[!sample])^2
rsq_ridge # 76.92%
install.packages("Metrics")                   
library("Metrics")
mse(y_var[!sample], ridge.pred)
rmse(y_var[!sample], ridge.pred)

## Calculate test MSE using lambda 1se
ridge.pred1 <- predict(ridge.mod, s = ridgeLAMDA.1se, newx = x_var[!sample, ])
mse.ridge1 <- mean((ridge.pred1 - y_var[!sample])^2)
mse.ridge1 # MSE = 1.555e+09, RMSE = 39433.48831
rsq_ridge1 <- cor(ridge.pred1, y_var[!sample])^2
rsq_ridge1 # 70.99%

## Finally, we refit our ridge regression model on the full data set using
## ridgeLAMDA.min
ridge.out <- glmnet(x_var, y_var, alpha = 0)
ridgecoefs = predict(ridge.out, type="coefficients", s=ridgeLAMDA.min)
ridgecoefs
ridge.final.pred <- predict(ridge.out, type = "coefficients", 
                            s = ridgeLAMDA.min)[1:84, ]
ridge.final.out <-  predict(ridge.out, s = ridgeLAMDA.min, newx = x_var)
mse.ridge.final.out <- mean((ridge.final.out - y_var)^2)
mse.ridge.final.out # MSE = 1.085e+09
rsq_ridge.final.out <- cor(ridge.final.out, y_var)^2
rsq_ridge.final.out ## 76.71%

plot(ridge.out, xvar="lambda", label=TRUE, main="Ridge coefficients")
abline(v=log(ridgeLAMDA.min), lty=2)

################################################################################
## LASSO
################################################################################

x <- model.matrix( SalePrice~ ., Combined.df[-c(15, 39)])[, -1] #[, -1] excludes duplication of intercept
y <- Combined.df$SalePrice
grid <- 10^seq(10, -2, length = 100)

## Create training and test data row indices
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(Combined.df), replace=TRUE, prob=c(0.7,0.3))

## Using Validation Set Approach
lasso.mod <- glmnet(x[sample, ], y[sample], alpha = 1, lambda = grid, thresh = 1e-12, standardize = TRUE)
lasso.pred <- predict(lasso.mod, s = 4, newx = x[!sample, ]) # For lambda = 4
mean((lasso.pred - y[!sample])^2)

## Plot coefficients by value of lambda parameter
lasso.mod <- glmnet(x[sample, ], y[sample], alpha = 1, lambda = grid, thresh = 1e-12, standardize = TRUE)
par(mfrow=c(1,1))
plot(lasso.mod, xvar="lambda", main="Coefficients by lambda parameter", ylim = c(-1e+05, 1e+05))

## Select best lambda using cross-validation
cv.lasso.out <- cv.glmnet(x[sample, ], y[sample], alpha = 1, nfold = 10)
par(mfrow=c(1,1))
plot(cv.lasso.out, main="Lasso MSE by lambda parameter")
lassoLAMDA.min <- cv.lasso.out$lambda.min
lassoLAMDA.min # 14177
lassoLAMDA.1se <- cv.lasso.out$lambda.1se
lassoLAMDA.1se # 29842

## Calculate test MSE using lambda minimum
lasso.pred <- predict(lasso.mod, s = lassoLAMDA.min, newx = x[!sample, ])
mse.lasso <- mean((lasso.pred - y[!sample])^2)
mse.lasso

## Calculate test MSE using lambda 1se
lasso.pred1 <- predict(lasso.mod, s = lassoLAMDA.1se, newx = x[!sample, ])
mse.lasso1 <- mean((lasso.pred1 - y[!sample])^2)
mse.lasso1


## Finally, we refit our ridge regression model on the full data set using
## ridgeLAMDA.min
lasso.out <- glmnet(x, y, alpha = 1)
lassocoefs = predict(lasso.out, type="coefficients", s=lassoLAMDA.min)
lassocoefs
lasso.final.pred <- predict(lasso.out, type = "coefficients", 
                            s = lassoLAMDA.min)[1:84, ]
lasso.final.pred

plot(lasso.out, xvar="lambda", label=TRUE, main="Lasso coefficients")
abline(v=log(lassoLAMDA.min), lty=2)

################################################################################
## Lasso - Part 2
################################################################################

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

x_var <- model.matrix(SalePrice~ ., final.df)[, -1] #[, -1] excludes duplication of intercept
y_var <- Combined.df$SalePrice

## Select best lambda using cross-validation
cv.lasso.out <- cv.glmnet(x_var[sample, ], y_var[sample], alpha = 1, nfold = 10)
par(mfrow=c(1,1))
plot(cv.lasso.out, main="Ridge MSE by lambda parameter")
lassoLAMDA.min <- cv.lasso.out$lambda.min
lassoLAMDA.min # 14177
lassoLAMDA.1se <- cv.lasso.out$lambda.1se
lassoLAMDA.1se # 29842

## Plot coefficients by value of lambda parameter
grid <- 10^seq(10, -2, length = 100)
lasso.mod <- glmnet(x_var[sample, ], y_var[sample], alpha = 1, lambda = grid, thresh = 1e-12)
par(mfrow=c(1,1))
plot(lasso.mod, xvar="lambda", main="Coefficients by lambda parameter", ylim = c(-1e+05, 1e+05))

## Calculate test MSE using lambda minimum
lasso.pred <- predict(lasso.mod, s = lassoLAMDA.min, newx = x_var[!sample, ])
mse.lasso <- mean((lasso.pred - y_var[!sample])^2)
mse.lasso # MSE = 997578597, RMSE = 31584.46765
rsq_lasso <- cor(lasso.pred, y_var[!sample])^2
rsq_lasso # 68.24%

## Calculate test MSE using lambda 1se
lasso.pred1 <- predict(lasso.mod, s = lassoLAMDA.1se, newx = x_var[!sample, ])
mse.lasso1 <- mean((lasso.pred1 - y_var[!sample])^2)
mse.lasso1 # MSE = 1.623e+09, RMSE = 40286.47416
rsq_lasso1 <- cor(lasso.pred1, y_var[!sample])^2
rsq_lasso1 # 63.71%%

## Finally, we refit our ridge regression model on the full data set using
## ridgeLAMDA.min
lasso.out <- glmnet(x_var, y_var, alpha = 1)
lassocoefs = predict(lasso.out, type="coefficients", s=lassoLAMDA.min)
lassocoefs
lasso.final.pred <- predict(lasso.out, type = "coefficients", 
                            s = lassoLAMDA.min)[1:84, ]
lasso.final.pred

plot(lasso.out, xvar="lambda", label=TRUE, main="Ridge coefficients")
abline(v=log(lassoLAMDA.min), lty=2)

################################################################################
## Elastic Net
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

## Set the training control
train_control1 <- trainControl(method = "repeatedcv", 
                              number = 10,
                              repeats = 10,
                              verboseIter = TRUE)
# Train the model
set.seed(888)
elastic_net_model1 <- train(SalePrice ~ .,
                           data = final.df,
                           method = "glmnet",
                           tuneGrid =expand.grid(alpha=seq(0,1, length.out = 100),
                                                 lambda = seq(0, 0.5, length.out = 100)),
                           trControl = train_control1)
elastic_net_model1 # alpha = 0, lambda = 10, RMSE = 44990, R^2 = 64.65%, MAE = 22996
plot(elastic_net_model1, main = "Elastic Net Regression")

train_control2 <- trainControl(method = "repeatedcv", 
                               number = 10,
                               repeats = 10,
                               search = "random",
                               verboseIter = TRUE)

set.seed(888)
elastic_net_model2 <- train(SalePrice ~ .,
                            data = final.df,
                            method = "glmnet",
                            tuneLength = 100,
                            trControl = train_control2)
elastic_net_model2 # alpha = 0.0007609, lambda = 0.0153315, RMSE = 42557, R^2 = 70.25%, MAE = 22940
plot(elastic_net_model2, main = "Elastic Net Regression")


## Calculate multiple R-squared
y_hat_enet1 <- predict(elastic_net_model1, X)
rsq_enet1 <- cor(y, y_hat_enet1)^2
rsq_enet1 # 87.88%

y_hat_enet2 <- predict(elastic_net_model2, X)
rsq_enet2 <- cor(y, y_hat_enet2)^2
rsq_enet2 # 53.84%



