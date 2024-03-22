###############################################################################################
## Project 2: Real Estate Data  - Exploratory Data Analysis
## Author: Tina Dhekial-Phukan
###############################################################################################
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

## Run the training data-clean up code
source("C:/Users/tinad/OneDrive - The Pennsylvania State University/Penn_State_Online_MAS/STAT_580/Project 2/Data Files/Combined_dataset_Clean-Up.R")  

##########################################################################################################################
## Exploratory Data Analysis 
#######################################################################################################################

final.df <- subset(Combined.df, select = -c(Utilities,uniqueID))
# Reduce levels of OverallQual
final.df["OverallQuality"] <- NA
final.df$OverallQuality[final.df$OverallQual %in% c("Very Poor", "Poor", "Fair", "Below Average")] <- "1-Below Average"
final.df$OverallQuality[final.df$OverallQual %in% c("Average")] <- "2-Average"
final.df$OverallQuality[final.df$OverallQual %in% c("Above Average", "Good", "Very Good", "Excellent", "Very Excellent")] <- "3-Above Average"
final.df$OverallQuality <- as.factor(final.df$OverallQuality)

# Reduce levels of OverallCond
final.df["OverallCondition"] <- NA
final.df$OverallCondition[final.df$OverallCond %in% c("Very Poor", "Poor", "Fair", "Below Average")] <- "1-Below Average"
final.df$OverallCondition[final.df$OverallCond %in% c("Average")] <- "2-Average"
final.df$OverallCondition[final.df$OverallCond %in% c("Above Average", "Good", "Very Good", "Excellent", "Very Excellent")] <- "3-Above Average"
final.df$OverallCondition <- as.factor(final.df$OverallCondition)

finalTrain.df <- subset(final.df, dataset %in% c("Train"))
finalTrain.df <- subset(finalTrain.df, select = -dataset)
finalSample.df <- subset(final.df, dataset %in% c("Sample"))
finalSample.df <- subset(finalSample.df, select = -dataset)
## Checking for missing data in each column
NA.sum <- colSums(is.na(CombinedTrain.df))
NA.sum

## Create subset of the numeric variables names
num_var <- names(subset(CombinedTrain.df, select = c(BedroomAbvGr,Fireplaces,FullBath,OpenPorchSF,BsmtFinSF1,
                                                     HalfBath,WoodDeckSF,TotRmsAbvGrd,SalePrice,GrLivArea,
                                                     LotArea,LotFrontage,HouseAge)))
num_vec <- as.vector(num_var)

## Create subset of categorical variable names
ordcat_var <- names(subset(CombinedTrain.df, select = -c(BedroomAbvGr,Fireplaces,FullBath,OpenPorchSF,BsmtFinSF1,
                                                        HalfBath,WoodDeckSF,TotRmsAbvGrd,SalePrice,GrLivArea,
                                                        LotArea,LotFrontage,HouseAge,dataset)))
ordcat_vec <- as.vector(ordcat_var)

# Compute correlation matrix 
library(corrplot)
corr_matrix1 <- cor(subset(CombinedTrain.df, select=c(num_vec)), method = "pearson")
par(mfrow=c(1,1))
corrplot(corr_matrix1, method = 'number', tl.cex=0.6, number.cex = 0.55, type = "lower",
         diag = FALSE,col = colorRampPalette(c("white", "orange", "red"))(100))

## Plot univariate histograms for the numerical variables
par(mfrow=c(3,3))
for (v in num_var) 
{
  hist(CombinedTrain.df[, v], main="", ylab="Freq", xlab = paste(v, sep=""), breaks = 50)
}

# install.packages("ggplot2")
library(ggplot2)

## Box plots of the numeric variables grouped by texture
library(patchwork)
bxplt1 <- ggplot(finalTrain.df, aes(x=OverallQual, y=SalePrice, color=OverallQual))+ geom_boxplot()+theme(axis.text.x=element_text(size=1))
bxplt2 <- ggplot(finalTrain.df, aes(x=OverallCond, y=SalePrice, color=OverallCond))+ geom_boxplot()+theme(axis.text.x=element_text(size=1))
bxplt3 <- ggplot(finalTrain.df, aes(x=OverallQuality, y=SalePrice, color=OverallQuality))+ geom_boxplot()+theme(axis.text.x=element_text(size=1))
bxplt4 <- ggplot(finalTrain.df, aes(x=OverallCondition, y=SalePrice, color=OverallCondition))+ geom_boxplot()+theme(axis.text.x=element_text(size=1))
bxplt1/bxplt3|bxplt2/bxplt4

bxplt5 <- ggplot(finalTrain.df, aes(x=Location, y=SalePrice, color=Location))+ geom_boxplot()+theme(axis.text.x=element_text(size=10))
bxplt5

# Histogram by group in ggplot2
ggplot(finalTrain.df, aes(x = BedroomAbvGr, fill =  Location, colour =  Location)) + 
  geom_histogram(alpha = 0.5, position = "identity", bins=30) + 
  guides(fill = guide_legend(title = "BedroomAbvGr"),colour = guide_legend(title = "BedroomAbvGr"))

