#######################################################################################################################
## Project 2: Real Estate Data Analysis - Best Subsets
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

########################################################################################
## Best Subsets
########################################################################################

final.df <- subset(Combined.df, select = -c(Utilities,uniqueID))

library(ISLR2)
library(leaps)
regfit.full1 <- regsubsets(SalePrice ~., final.df, really.big = TRUE)

