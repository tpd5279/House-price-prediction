###############################################################################################
## Project 2: Real Estate Data Clean up
## Author: Tina Dhekial-Phukan
## Objective of analysis: Predict the house sale price for the houses with missing house prices
###############################################################################################
rm(list = ls(all.names = TRUE))
gc()
options(digits=4)

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

################################################################################
## College Cr Training Data-set clean-up
################################################################################

CollegeCr <- read.csv(file="CollegeCr.csv")
CollegeCr.mod1 <- CollegeCr

## SalePrice
CollegeCr.mod1$SalePrice <- as.numeric(CollegeCr.mod1$SalePrice)

## OverallQual 
CollegeCr.mod1$OverallQual <- ordered(CollegeCr.mod1$OverallQual, levels = 1:10,
                                      labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                                 "Average", "Above Average", "Good", "Very Good",
                                                 "Excellent", "Very Excellent"))
## BedroomAbvGr 
CollegeCr.mod1$BedroomAbvGr <- as.numeric(CollegeCr.mod1$BedroomAbvGr)

## Central Air
CollegeCr.mod1$CentralAir <- as.factor(CollegeCr.mod1$CentralAir)

## Fireplaces
CollegeCr.mod1$Fireplaces <- as.numeric(CollegeCr.mod1$Fireplaces)

## HouseStyle
CollegeCr.mod1$HouseStyle <- as.factor(CollegeCr.mod1$HouseStyle)

## HeatingQC
CollegeCr.mod1$HeatingQC <- factor(CollegeCr.mod1$HeatingQC, order = FALSE, 
                                   levels = c("Po", "Fa", "TA", "Gd", "Ex"))

## GarageType
CollegeCr.mod1$GarageType[CollegeCr.mod1$GarageType != "Attchd" & CollegeCr.mod1$GarageType != "Detchd"] <- "NoGarage"
CollegeCr.mod1$GarageType <- as.factor(CollegeCr.mod1$GarageType)

## RoofMatl
CollegeCr.mod1$RoofMatl <- as.factor(CollegeCr.mod1$RoofMatl)

## PavedDrive
CollegeCr.mod1$PavedDrive <- as.factor(CollegeCr.mod1$PavedDrive)

## SaleType
CollegeCr.mod1$SaleType <- as.factor(CollegeCr.mod1$SaleType)

## FullBath
CollegeCr.mod1$FullBath <- as.numeric(CollegeCr.mod1$FullBath)

## OpenPorchSF
CollegeCr.mod1$OpenPorchSF <- as.numeric(CollegeCr.mod1$OpenPorchSF)

## RoofStyle
CollegeCr.mod1$RoofStyle <- as.factor(CollegeCr.mod1$RoofStyle)

## Exterior 
library(purrr)
library(stringr)
library(tibble)
x <-str_split(CollegeCr.mod1$Exterior, ";")

# To force map to return a character vector instead of a list, we can use map_chr
Exterior.dat <- tibble(map_chr(x, 1),  
                       map_chr(x, 2),
                       map_chr(x, 3))
colnames(Exterior.dat) <- c("ExterCov", "ExterQual", "ExterCond")
CollegeCr.mod1 <- cbind(CollegeCr.mod1, Exterior.dat)
CollegeCr.mod1$ExterCov <- as.factor(CollegeCr.mod1$ExterCov)
CollegeCr.mod1$ExterQual <- as.factor(CollegeCr.mod1$ExterQual)
CollegeCr.mod1$ExterCond <- as.factor(CollegeCr.mod1$ExterCond)

## Utilities
CollegeCr.mod1$Utilities <- as.factor(CollegeCr.mod1$Utilities)

## Heating
CollegeCr.mod1$Heating <- as.factor(CollegeCr.mod1$Heating)

## LotInfo 
library(purrr)
library(stringr)
library(tibble)
y <-str_split(CollegeCr.mod1$LotInfo, ";")

# To force map to return a character vector instead of a list, we can use map_chr
Lot.dat <- tibble(map_chr(y, 1),  
                  map_chr(y, 2),
                  map_chr(y, 3),
                  map_chr(y, 4))
colnames(Lot.dat) <- c("LotConfig", "LotShape", "LotArea","LotFrontage")
CollegeCr.mod1 <- cbind(CollegeCr.mod1, Lot.dat)
CollegeCr.mod1$LotConfig <- as.factor(CollegeCr.mod1$LotConfig)
CollegeCr.mod1$LotShape <- as.factor(CollegeCr.mod1$LotShape)
CollegeCr.mod1$LotArea <- as.numeric(CollegeCr.mod1$LotArea)
CollegeCr.mod1$LotFrontage <- as.numeric(CollegeCr.mod1$LotFrontage)
CollegeCr.mod1$LotFrontage[is.na(CollegeCr.mod1$LotFrontage)] <- median(CollegeCr.mod1$LotFrontage, na.rm=TRUE)

## KitchenQual
CollegeCr.mod1$KitchenQual <- factor(CollegeCr.mod1$KitchenQual, order = FALSE, 
                                     levels = c("Fa", "TA", "Gd", "Ex"))

## HalfBath
CollegeCr.mod1$HalfBath <- as.numeric(CollegeCr.mod1$HalfBath)

## Basement conditions
# BsmtFinSF1
CollegeCr.mod1$BsmtFinSF1 <- as.numeric(CollegeCr.mod1$BsmtFinSF1)

# BsmtUnfSF
CollegeCr.mod1$BsmtUnfSF <- as.numeric(CollegeCr.mod1$BsmtUnfSF)

# BsmtCond, BsmtQual, BsmtFinType1
CollegeCr.mod1$TotBsmtSF <- CollegeCr.mod1$BsmtFinSF1 + CollegeCr.mod1$BsmtUnfSF

CollegeCr.mod1$BsmtCond[is.na(CollegeCr.mod1$BsmtCond)] <- "NA"
CollegeCr.mod1$BsmtCond[CollegeCr.mod1$BsmtCond == "NA" & CollegeCr.mod1$TotBsmtSF == 0] <- "NoBsmt"
CollegeCr.mod1$BsmtCond[CollegeCr.mod1$BsmtCond == "NA" & CollegeCr.mod1$TotBsmtSF != 0] <- "Unknown"
CollegeCr.mod1$BsmtCond <- as.factor(CollegeCr.mod1$BsmtCond)

CollegeCr.mod1$BsmtQual[!(CollegeCr.mod1$BsmtQual %in% c("Ex", "Fa", "Gd", "TA"))] <- "NA"
CollegeCr.mod1$BsmtQual[CollegeCr.mod1$BsmtQual == "NA" & CollegeCr.mod1$TotBsmtSF == 0] <- "NoBsmt"
CollegeCr.mod1$BsmtQual[CollegeCr.mod1$BsmtQual == "NA" & CollegeCr.mod1$TotBsmtSF != 0] <- "Unknown"
CollegeCr.mod1$BsmtQual <- as.factor(CollegeCr.mod1$BsmtQual)

CollegeCr.mod1$BsmtFinType1[!(CollegeCr.mod1$BsmtFinType1 %in% c("GLQ", "ALQ", "BLQ", "LwQ", "Unf"))] <- "NA"
CollegeCr.mod1$BsmtFinType1[CollegeCr.mod1$BsmtFinType1 == "NA" & CollegeCr.mod1$TotBsmtSF == 0] <- "NoBsmt"
CollegeCr.mod1$BsmtFinType1[CollegeCr.mod1$BsmtFinType1 == "NA" & CollegeCr.mod1$TotBsmtSF != 0] <- "Unknown"
CollegeCr.mod1$BsmtFinType1 <- as.factor(CollegeCr.mod1$BsmtFinType1)


## WoodDeckSF
CollegeCr.mod1$WoodDeckSF <- as.numeric(CollegeCr.mod1$WoodDeckSF)

## TotRmsAbvGrd
CollegeCr.mod1$TotRmsAbvGrd <- as.numeric(CollegeCr.mod1$TotRmsAbvGrd)

## Foundation
CollegeCr.mod1$Foundation <- as.factor(CollegeCr.mod1$Foundation)

## Electrical
CollegeCr.mod1$Electrical <- as.factor(CollegeCr.mod1$Electrical)

## BldgType
CollegeCr.mod1$BldgType <- as.factor(CollegeCr.mod1$BldgType)

## OverallCond
CollegeCr.mod1$OverallCond <- ordered(CollegeCr.mod1$OverallCond, levels = 1:10,
                                      labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                                 "Average", "Above Average", "Good", "Very Good",
                                                 "Excellent", "Very Excellent"))
## GrLivArea
CollegeCr.mod1$GrLivArea <- as.numeric(CollegeCr.mod1$GrLivArea)

## Create Location column
CollegeCr.mod1$Location <- as.factor("College Cr")

## YearBuilt
CollegeCr.mod1$YearBuilt <- as.numeric(CollegeCr.mod1$YearBuilt)

## YrSold
CollegeCr.mod1$YrSold <- as.numeric(CollegeCr.mod1$YrSold)

## Compute age of the house
CollegeCr.mod1$HouseAge <- CollegeCr.mod1$YrSold - CollegeCr.mod1$YearBuilt 

## Deleting 1 row where YrSold < YearBuilt
CollegeCr.mod1 <- CollegeCr.mod1[c(-30),]

## Remove Exterior and LotInfo columns
CollegeCr.mod1 <- subset(CollegeCr.mod1, select = -c(Exterior, LotInfo))

## Remove 'BsmtUnfSF' since this column is missing in the Edwards data set
CollegeCr.mod1 <- subset(CollegeCr.mod1, select = -BsmtUnfSF)

## Remove 'TotBsmtSF' since this is a temporary column
## Remove 'YearBuilt' and 'YrSold'
CollegeCr.mod1 <- subset(CollegeCr.mod1, select = -c(TotBsmtSF, YearBuilt, YrSold))

## Identify and remove duplicate rows
library(misty)
which(duplicated(CollegeCr.mod1))
duplicate_df <- df.duplicated(CollegeCr.mod1, first = TRUE, keep.all = TRUE, from.last = FALSE, keep.row.names = TRUE, check = TRUE)
CollegeCr.mod1 <- df.unique(CollegeCr.mod1, keep.all = TRUE, from.last = FALSE, keep.row.names = TRUE, check = TRUE)

## Add a unique ID to each record
CollegeCr.mod1$uniqueID <- as.vector(seq(1, 113, by=1))
CollegeCr.mod1 %>% relocate(uniqueID, SalePrice)

################################################################################
## Edwards Training Dataset clean-up
################################################################################

Edwards <- read.csv(file="Edwards.csv")
Edwards.mod1 <- Edwards

## SalePrice
Edwards.mod1$SalePrice <- as.numeric(Edwards.mod1$SalePrice)

## Electrical
Edwards.mod1$Electrical <- as.factor(Edwards.mod1$Electrical)

## HeatingQC
Edwards.mod1$HeatingQC <- factor(Edwards.mod1$HeatingQC, order = FALSE, 
                                 levels = c("Po", "Fa", "TA", "Gd", "Ex"))

## Basement conditions
# BsmtFinSF1
Edwards.mod1$BsmtFinSF1 <- as.numeric(Edwards.mod1$BsmtFinSF1)

# BsmtQual, BsmtCond, BsmtFinType1
Edwards.mod1$BsmtQual[!(Edwards.mod1$BsmtQual %in% c("Ex", "Fa", "Gd", "TA"))] <- "Unknown"            
Edwards.mod1$BsmtQual <- as.factor(Edwards.mod1$BsmtQual)

Edwards.mod1$BsmtCond[is.na(Edwards.mod1$BsmtCond)] <- "Unknown"
Edwards.mod1$BsmtCond <- as.factor(Edwards.mod1$BsmtCond)

Edwards.mod1$BsmtFinType1[!(Edwards.mod1$BsmtFinType1 %in% c("Unf","LwQ","BLQ", "ALQ", "GLQ"))] <- "Unknown"
Edwards.mod1$BsmtFinType1 <- as.factor(Edwards.mod1$BsmtFinType1)

## Foundation
Edwards.mod1$Foundation <- as.factor(Edwards.mod1$Foundation)

## RoofMatl
Edwards.mod1$RoofMatl <- as.factor(Edwards.mod1$RoofMatl)

## OverallCond
Edwards.mod1$OverallCond <- ordered(Edwards.mod1$OverallCond, levels = 1:10,
                                    labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                               "Average", "Above Average", "Good", "Very Good",
                                               "Excellent", "Very Excellent"))

## Utilities
Edwards.mod1$Utilities <- as.factor(Edwards.mod1$Utilities)

## WoodDeckSF
Edwards.mod1$WoodDeckSF <- as.numeric(Edwards.mod1$WoodDeckSF)

## LotInfo
library(purrr)
library(stringr)
library(tibble)
x1 <-str_split(Edwards.mod1$LotInfo, ";")
# To force map to return a character vector instead of a list, we can use map_chr
Lot1.dat <- tibble(map_chr(x1, 1),  
                   map_chr(x1, 2),
                   map_chr(x1, 3),
                   map_chr(x1, 4))
colnames(Lot1.dat) <- c("LotConfig", "LotShape", "LotArea","LotFrontage")
Edwards.mod1 <- cbind(Edwards.mod1, Lot1.dat)
Edwards.mod1$LotConfig <- as.factor(Edwards.mod1$LotConfig)
Edwards.mod1$LotShape <- as.factor(Edwards.mod1$LotShape)
Edwards.mod1$LotArea <- as.numeric(Edwards.mod1$LotArea)
Edwards.mod1$LotFrontage <- as.numeric(Edwards.mod1$LotFrontage)
Edwards.mod1$LotFrontage[is.na(Edwards.mod1$LotFrontage)] <- median(Edwards.mod1$LotFrontage, na.rm=TRUE)

## SaleType
Edwards.mod1$SaleType <- as.factor(Edwards.mod1$SaleType)

## BedroomAbvGr
Edwards.mod1$BedroomAbvGr <- as.numeric(Edwards.mod1$BedroomAbvGr)

## KitchenQual
Edwards.mod1$KitchenQual <- factor(Edwards.mod1$KitchenQual, order = FALSE, 
                                   levels = c("Fa", "TA", "Gd", "Ex"))

## HalfBath
Edwards.mod1$HalfBath <- as.numeric(Edwards.mod1$HalfBath)

## RoofStyle
Edwards.mod1$RoofStyle <- as.factor(Edwards.mod1$RoofStyle)

## GrLivArea
Edwards.mod1$GrLivArea <- as.numeric(Edwards.mod1$GrLivArea)

## OpenPorchSF
Edwards.mod1$OpenPorchSF <- as.numeric(Edwards.mod1$OpenPorchSF)

## Heating
Edwards.mod1$Heating <- as.factor(Edwards.mod1$Heating)

## Fireplaces
Edwards.mod1$Fireplaces <- as.numeric(Edwards.mod1$Fireplaces)

## BldgType
Edwards.mod1$BldgType <- as.factor(Edwards.mod1$BldgType)

## OverallQual 
Edwards.mod1$OverallQual <- ordered(Edwards.mod1$OverallQual, levels = 1:10,
                                    labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                               "Average", "Above Average", "Good", "Very Good",
                                               "Excellent", "Very Excellent"))

## KitchenAbvGr
Edwards.mod1$KitchenAbvGr <- as.numeric(Edwards.mod1$KitchenAbvGr)

## FullBath
Edwards.mod1$FullBath <- as.numeric(Edwards.mod1$FullBath)

## PavedDrive
Edwards.mod1$PavedDrive <- as.factor(Edwards.mod1$PavedDrive)

## GarageType
Edwards.mod1$GarageType[Edwards.mod1$GarageType != "Attchd" & Edwards.mod1$GarageType != "Detchd"] <- "NoGarage"
Edwards.mod1$GarageType <- as.factor(Edwards.mod1$GarageType)

## Central Air
Edwards.mod1$CentralAir <- as.factor(Edwards.mod1$CentralAir)

## Exterior
library(purrr)
library(stringr)
library(tibble)
y1 <-str_split(Edwards.mod1$Exterior, ";")
# To force map to return a character vector instead of a list, we can use map_chr
Exterior1.dat <- tibble(map_chr(y1, 1),  
                        map_chr(y1, 2),
                        map_chr(y1, 3))
colnames(Exterior1.dat) <- c("ExterCov", "ExterQual", "ExterCond")
Edwards.mod1 <- cbind(Edwards.mod1, Exterior1.dat)
Edwards.mod1$ExterCov <- as.factor(Edwards.mod1$ExterCov)
Edwards.mod1$ExterQual <- as.factor(Edwards.mod1$ExterQual)
Edwards.mod1$ExterCond <- as.factor(Edwards.mod1$ExterCond)

## HouseStyle
Edwards.mod1$HouseStyle <- as.factor(Edwards.mod1$HouseStyle)

## TotRmsAbvGrd
Edwards.mod1$TotRmsAbvGrd <- as.numeric(Edwards.mod1$TotRmsAbvGrd)

## Create Location column
Edwards.mod1$Location <- as.factor("Edwards")

## YearBuilt
Edwards.mod1$YearBuilt <- as.numeric(Edwards.mod1$YearBuilt)

## YrSold
Edwards.mod1$YrSold <- as.numeric(Edwards.mod1$YrSold)

## Compute age of the house
Edwards.mod1$HouseAge <- Edwards.mod1$YrSold - Edwards.mod1$YearBuilt   

## Remove Exterior and LotInfo columns
Edwards.mod1 <- subset(Edwards.mod1, select = -c(Exterior, LotInfo))

## Remove 'KitchenAbvGr' since this column is missing in the College Cr data set
## Remove 'YearBuilt' and 'YrSold'
Edwards.mod1 <- subset(Edwards.mod1, select = -c(KitchenAbvGr, YearBuilt, YrSold))

## Identify duplicate rows
library(misty)
which(duplicated(Edwards.mod1))

## Add a unique ID to each record
Edwards.mod1$uniqueID <- as.vector(seq(114, 187, by=1))
Edwards.mod1 %>% relocate(uniqueID, SalePrice)

################################################################################
## Old Town Training Dataset clean-up
################################################################################

OldTown <- read.csv(file="OldTown.csv")
OldTown.mod1 <- OldTown

## SalePrice
OldTown.mod1$SalePrice <- as.numeric(OldTown.mod1$SalePrice)

## Foundation
OldTown.mod1$Foundation <- as.factor(OldTown.mod1$Foundation)

## GrLivArea
OldTown.mod1$GrLivArea <- as.numeric(OldTown.mod1$GrLivArea)

## PavedDrive
OldTown.mod1$PavedDrive <- as.factor(OldTown.mod1$PavedDrive)

## RoofStyle
OldTown.mod1$RoofStyle <- as.factor(OldTown.mod1$RoofStyle)

## Fireplaces
OldTown.mod1$Fireplaces <- as.numeric(OldTown.mod1$Fireplaces)

## Lotinfo
library(stringr)
library(tibble)
y2 <-str_split(OldTown.mod1$LotInfo, ";")
# To force map to return a character vector instead of a list, we can use map_chr
Lot2.dat <- tibble(map_chr(y2, 1),  
                   map_chr(y2, 2),
                   map_chr(y2, 3),
                   map_chr(y2, 4))
colnames(Lot2.dat) <- c("LotConfig", "LotShape", "LotArea","LotFrontage")
OldTown.mod1 <- cbind(OldTown.mod1, Lot2.dat)
OldTown.mod1$LotConfig <- as.factor(OldTown.mod1$LotConfig)
OldTown.mod1$LotShape <- as.factor(OldTown.mod1$LotShape)
OldTown.mod1$LotArea <- as.numeric(OldTown.mod1$LotArea)
OldTown.mod1$LotFrontage <- as.numeric(OldTown.mod1$LotFrontage)
OldTown.mod1$LotFrontage[is.na(OldTown.mod1$LotFrontage)] <- median(OldTown.mod1$LotFrontage, na.rm=TRUE)

## WoodDeckSF
OldTown.mod1$WoodDeckSF <- as.numeric(OldTown.mod1$WoodDeckSF)

## BldgType
OldTown.mod1$BldgType <- as.factor(OldTown.mod1$BldgType)

## HalfBath
OldTown.mod1$HalfBath <- as.numeric(OldTown.mod1$HalfBath)

## FullBath
OldTown.mod1$FullBath <- as.numeric(OldTown.mod1$FullBath)

## KitchenQual
OldTown.mod1$KitchenQual <- factor(OldTown.mod1$KitchenQual, order = FALSE, 
                                   levels = c("Fa", "TA", "Gd", "Ex"))
## Basement conditions
# BsmtUnfSF
OldTown.mod1$BsmtUnfSF <- as.numeric(OldTown.mod1$BsmtUnfSF)

# BsmtFinSF1
OldTown.mod1$BsmtFinSF1 <- as.numeric(OldTown.mod1$BsmtFinSF1)

# BsmtQual, BsmtCond, BsmtFinType1
OldTown.mod1$TotBsmtSF <- OldTown.mod1$BsmtFinSF1 + OldTown.mod1$BsmtUnfSF

OldTown.mod1$BsmtCond[is.na(OldTown.mod1$BsmtCond)] <- "NA"
OldTown.mod1$BsmtCond[OldTown.mod1$BsmtCond == "NA" & OldTown.mod1$TotBsmtSF == 0] <- "NoBsmt"
OldTown.mod1$BsmtCond[OldTown.mod1$BsmtCond == "NA" & OldTown.mod1$TotBsmtSF != 0] <- "Unknown"
OldTown.mod1$BsmtCond <- as.factor(OldTown.mod1$BsmtCond)

OldTown.mod1$BsmtQual[!(OldTown.mod1$BsmtQual %in% c("Ex", "Fa", "Gd", "TA"))] <- "NA"
OldTown.mod1$BsmtQual[OldTown.mod1$BsmtQual == "NA" & OldTown.mod1$TotBsmtSF == 0] <- "NoBsmt"
OldTown.mod1$BsmtQual[OldTown.mod1$BsmtQual == "NA" & OldTown.mod1$TotBsmtSF != 0] <- "Unknown"
OldTown.mod1$BsmtQual <- as.factor(OldTown.mod1$BsmtQual)

OldTown.mod1$BsmtFinType1[!(OldTown.mod1$BsmtFinType1 %in% c("GLQ", "ALQ", "BLQ", "LwQ", "Unf"))] <- "NA"
OldTown.mod1$BsmtFinType1[OldTown.mod1$BsmtFinType1 == "NA" & OldTown.mod1$TotBsmtSF == 0] <- "NoBsmt"
OldTown.mod1$BsmtFinType1[OldTown.mod1$BsmtFinType1 == "NA" & OldTown.mod1$TotBsmtSF != 0] <- "Unknown"
OldTown.mod1$BsmtFinType1 <- as.factor(OldTown.mod1$BsmtFinType1)

## Central Air
OldTown.mod1$CentralAir <- as.factor(OldTown.mod1$CentralAir)

## SaleType
OldTown.mod1$SaleType <- as.factor(OldTown.mod1$SaleType)

## Electrical
OldTown.mod1$Electrical <- as.factor(OldTown.mod1$Electrical)

## Exterior
library(purrr)
library(stringr)
library(tibble)
x2 <-str_split(OldTown.mod1$Exterior, ";")
# To force map to return a character vector instead of a list, we can use map_chr
Exterior2.dat <- tibble(map_chr(x2, 1),  
                        map_chr(x2, 2),
                        map_chr(x2, 3))
colnames(Exterior2.dat) <- c("ExterCov", "ExterQual", "ExterCond")
OldTown.mod1 <- cbind(OldTown.mod1, Exterior2.dat)
OldTown.mod1$ExterCov <- as.factor(OldTown.mod1$ExterCov)
OldTown.mod1$ExterQual <- as.factor(OldTown.mod1$ExterQual)
OldTown.mod1$ExterCond <- as.factor(OldTown.mod1$ExterCond)

## OpenPorchSF
OldTown.mod1$OpenPorchSF <- as.numeric(OldTown.mod1$OpenPorchSF)

## RoofMatl
OldTown.mod1$RoofMatl <- as.factor(OldTown.mod1$RoofMatl)

## HeatingQC
OldTown.mod1$HeatingQC <- factor(OldTown.mod1$HeatingQC, order = FALSE, 
                                 levels = c("Po", "Fa", "TA", "Gd", "Ex"))

## BedroomAbvGr 
OldTown.mod1$BedroomAbvGr <- as.numeric(OldTown.mod1$BedroomAbvGr)

## Heating
OldTown.mod1$Heating <- as.factor(OldTown.mod1$Heating)

## OverallQual 
OldTown.mod1$OverallQual <- ordered(OldTown.mod1$OverallQual, levels = 1:10,
                                    labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                               "Average", "Above Average", "Good", "Very Good",
                                               "Excellent", "Very Excellent"))

## KitchenAbvGr
OldTown.mod1$KitchenAbvGr <- as.numeric(OldTown.mod1$KitchenAbvGr)

## GarageType
OldTown.mod1$GarageType[OldTown.mod1$GarageType != "Attchd" & OldTown.mod1$GarageType != "Detchd"] <- "NoGarage"
OldTown.mod1$GarageType <- as.factor(OldTown.mod1$GarageType)

## HouseStyle
OldTown.mod1$HouseStyle <- as.factor(OldTown.mod1$HouseStyle)

## OverallCond
OldTown.mod1$OverallCond <- ordered(OldTown.mod1$OverallCond, levels = 1:10,
                                    labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                               "Average", "Above Average", "Good", "Very Good",
                                               "Excellent", "Very Excellent"))

## TotRmsAbvGrd
OldTown.mod1$TotRmsAbvGrd <- as.numeric(OldTown.mod1$TotRmsAbvGrd)

## Utilities
OldTown.mod1$Utilities <- as.factor(OldTown.mod1$Utilities)

## Create Location column
OldTown.mod1$Location <- as.factor("Old town")

## YearBuilt
OldTown.mod1$YearBuilt <- as.numeric(OldTown.mod1$YearBuilt)

## YrSold
OldTown.mod1$YrSold <- as.numeric(OldTown.mod1$YrSold)

## Compute age of the house
OldTown.mod1$HouseAge <- OldTown.mod1$YrSold - OldTown.mod1$YearBuilt  

## Remove Exterior and LotInfo columns
OldTown.mod1 <- subset(OldTown.mod1, select = -c(Exterior, LotInfo))

## Remove 'KitchenAbvGr' and 'BsmtUnfSF' since these column are missing in the other two data sets
## Remove 'TotBsmtSF' since this is a temporary column
## Remove 'YearBuilt' and 'YrSold'
OldTown.mod1 <- subset(OldTown.mod1, select = -c(KitchenAbvGr, BsmtUnfSF, TotBsmtSF, YearBuilt, YrSold))

## Identify and remove duplicate rows
library(misty)
which(duplicated(OldTown.mod1))
duplicate_df2 <- df.duplicated(OldTown.mod1, first = TRUE, keep.all = TRUE, from.last = FALSE, keep.row.names = TRUE, check = TRUE)
OldTown.mod1 <- df.unique(OldTown.mod1, keep.all = TRUE, from.last = FALSE, keep.row.names = TRUE, check = TRUE)

## Add a unique ID to each record
OldTown.mod1$uniqueID <- as.vector(seq(188, 275, by=1))
OldTown.mod1 %>% relocate(uniqueID, SalePrice)

################################################################################
## Concatenate all three training datasets 
################################################################################

## Concatenate CollegeCr and Edwards data frames
CombinedTrain.df <- rbind(CollegeCr.mod1, Edwards.mod1, OldTown.mod1)
CombinedTrain.df$dataset <- as.factor("Train")

################################################################################
## College Cr Sample Data-set clean-up
################################################################################

CollegeCr.test <- read.csv(file="CollegeCr.test.csv")
CollegeCr.test.mod1 <- CollegeCr.test

## Checking for missing data in each column
NA.sum <- colSums(is.na(CollegeCr.test.mod1))
NA.sum
which(is.na(CollegeCr.test.mod1$BsmtCond))

# BsmtCond, BsmtQual, BsmtFinType1
CollegeCr.test.mod1$TotBsmtSF <- CollegeCr.test.mod1$BsmtFinSF1 + CollegeCr.test.mod1$BsmtUnfSF

CollegeCr.test.mod1$BsmtCond[is.na(CollegeCr.test.mod1$BsmtCond)] <- "NA"
CollegeCr.test.mod1$BsmtCond[CollegeCr.test.mod1$BsmtCond == "NA" & CollegeCr.test.mod1$TotBsmtSF == 0] <- "NoBsmt"
CollegeCr.test.mod1$BsmtCond[CollegeCr.test.mod1$BsmtCond == "NA" & CollegeCr.test.mod1$TotBsmtSF != 0] <- "Unknown"
CollegeCr.test.mod1$BsmtCond <- as.factor(CollegeCr.test.mod1$BsmtCond)

CollegeCr.test.mod1$BsmtQual[!(CollegeCr.test.mod1$BsmtQual %in% c("Ex", "Fa", "Gd", "TA"))] <- "NA"
CollegeCr.test.mod1$BsmtQual[CollegeCr.test.mod1$BsmtQual == "NA" & CollegeCr.test.mod1$TotBsmtSF == 0] <- "NoBsmt"
CollegeCr.test.mod1$BsmtQual[CollegeCr.test.mod1$BsmtQual == "NA" & CollegeCr.test.mod1$TotBsmtSF != 0] <- "Unknown"
CollegeCr.test.mod1$BsmtQual <- as.factor(CollegeCr.test.mod1$BsmtQual)

CollegeCr.test.mod1$BsmtFinType1[!(CollegeCr.test.mod1$BsmtFinType1 %in% c("GLQ", "ALQ", "BLQ", "LwQ", "Unf"))] <- "NA"
CollegeCr.test.mod1$BsmtFinType1[CollegeCr.test.mod1$BsmtFinType1 == "NA" & CollegeCr.test.mod1$TotBsmtSF == 0] <- "NoBsmt"
CollegeCr.test.mod1$BsmtFinType1[CollegeCr.test.mod1$BsmtFinType1 == "NA" & CollegeCr.test.mod1$TotBsmtSF != 0] <- "Unknown"
CollegeCr.test.mod1$BsmtFinType1 <- as.factor(CollegeCr.test.mod1$BsmtFinType1)

## OverallQual 
CollegeCr.test.mod1$OverallQual <- ordered(CollegeCr.test.mod1$OverallQual, levels = 1:10,
                                           labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                                      "Average", "Above Average", "Good", "Very Good",
                                                      "Excellent", "Very Excellent"))
## BedroomAbvGr 
CollegeCr.test.mod1$BedroomAbvGr <- as.numeric(CollegeCr.test.mod1$BedroomAbvGr)

## Central Air
CollegeCr.test.mod1$CentralAir <- as.factor(CollegeCr.test.mod1$CentralAir)

## Fireplaces
CollegeCr.test.mod1$Fireplaces <- as.numeric(CollegeCr.test.mod1$Fireplaces)

## HouseStyle
CollegeCr.test.mod1$HouseStyle <- as.factor(CollegeCr.test.mod1$HouseStyle)

## HeatingQC
CollegeCr.test.mod1$HeatingQC <- factor(CollegeCr.test.mod1$HeatingQC, order = FALSE, 
                                        levels = c("Po", "Fa", "TA", "Gd", "Ex"))

## GarageType
CollegeCr.test.mod1$GarageType[CollegeCr.test.mod1$GarageType != "Attchd" & CollegeCr.test.mod1$GarageType != "Detchd"] <- "NoGarage"
CollegeCr.test.mod1$GarageType <- as.factor(CollegeCr.test.mod1$GarageType)

## RoofMatl
CollegeCr.test.mod1$RoofMatl <- as.factor(CollegeCr.test.mod1$RoofMatl)

## PavedDrive
CollegeCr.test.mod1$PavedDrive <- as.factor(CollegeCr.test.mod1$PavedDrive)

## SaleType
CollegeCr.test.mod1$SaleType <- as.factor(CollegeCr.test.mod1$SaleType)

## FullBath
CollegeCr.test.mod1$FullBath <- as.numeric(CollegeCr.test.mod1$FullBath)

## OpenPorchSF
CollegeCr.test.mod1$OpenPorchSF <- as.numeric(CollegeCr.test.mod1$OpenPorchSF)

## RoofStyle
CollegeCr.test.mod1$RoofStyle <- as.factor(CollegeCr.test.mod1$RoofStyle)

## Exterior 
library(purrr)
library(stringr)
library(tibble)
x <-str_split(CollegeCr.test.mod1$Exterior, ";")

# To force map to return a character vector instead of a list, we can use map_chr
Exterior.dat <- tibble(map_chr(x, 1),  
                       map_chr(x, 2),
                       map_chr(x, 3))
colnames(Exterior.dat) <- c("ExterCov", "ExterQual", "ExterCond")
CollegeCr.test.mod1 <- cbind(CollegeCr.test.mod1, Exterior.dat)
CollegeCr.test.mod1$ExterCov <- as.factor(CollegeCr.test.mod1$ExterCov)
CollegeCr.test.mod1$ExterQual <- as.factor(CollegeCr.test.mod1$ExterQual)
CollegeCr.test.mod1$ExterCond <- as.factor(CollegeCr.test.mod1$ExterCond)

## Utilities
CollegeCr.test.mod1$Utilities <- as.factor(CollegeCr.test.mod1$Utilities)

## Heating
CollegeCr.test.mod1$Heating <- as.factor(CollegeCr.test.mod1$Heating)

## LotInfo 
library(purrr)
library(stringr)
library(tibble)
y <-str_split(CollegeCr.test.mod1$LotInfo, ";")

# To force map to return a character vector instead of a list, we can use map_chr
Lot.dat <- tibble(map_chr(y, 1),  
                  map_chr(y, 2),
                  map_chr(y, 3),
                  map_chr(y, 4))
colnames(Lot.dat) <- c("LotConfig", "LotShape", "LotArea","LotFrontage")
CollegeCr.test.mod1 <- cbind(CollegeCr.test.mod1, Lot.dat)
CollegeCr.test.mod1$LotConfig <- as.factor(CollegeCr.test.mod1$LotConfig)
CollegeCr.test.mod1$LotShape <- as.factor(CollegeCr.test.mod1$LotShape)
CollegeCr.test.mod1$LotArea <- as.numeric(CollegeCr.test.mod1$LotArea)
CollegeCr.test.mod1$LotFrontage <- as.numeric(CollegeCr.test.mod1$LotFrontage)
CollegeCr.test.mod1$LotFrontage[is.na(CollegeCr.test.mod1$LotFrontage)] <- median(CollegeCr.test.mod1$LotFrontage, na.rm=TRUE)

## KitchenQual
CollegeCr.test.mod1$KitchenQual <- factor(CollegeCr.test.mod1$KitchenQual, order = FALSE, 
                                          levels = c("Fa", "TA", "Gd", "Ex"))

## HalfBath
CollegeCr.test.mod1$HalfBath <- as.numeric(CollegeCr.test.mod1$HalfBath)

## Basement conditions
# BsmtFinSF1
CollegeCr.test.mod1$BsmtFinSF1 <- as.numeric(CollegeCr.test.mod1$BsmtFinSF1)

# BsmtUnfSF
CollegeCr.test.mod1$BsmtUnfSF <- as.numeric(CollegeCr.test.mod1$BsmtUnfSF)

## WoodDeckSF
CollegeCr.test.mod1$WoodDeckSF <- as.numeric(CollegeCr.test.mod1$WoodDeckSF)

## TotRmsAbvGrd
CollegeCr.test.mod1$TotRmsAbvGrd <- as.numeric(CollegeCr.test.mod1$TotRmsAbvGrd)

## Foundation
CollegeCr.test.mod1$Foundation <- as.factor(CollegeCr.test.mod1$Foundation)

## Electrical
CollegeCr.test.mod1$Electrical <- as.factor(CollegeCr.test.mod1$Electrical)

## BldgType
CollegeCr.test.mod1$BldgType <- as.factor(CollegeCr.test.mod1$BldgType)

## OverallCond
CollegeCr.test.mod1$OverallCond <- ordered(CollegeCr.test.mod1$OverallCond, levels = 1:10,
                                           labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                                      "Average", "Above Average", "Good", "Very Good",
                                                      "Excellent", "Very Excellent"))
## GrLivArea
CollegeCr.test.mod1$GrLivArea <- as.numeric(CollegeCr.test.mod1$GrLivArea)

## Create Location column
CollegeCr.test.mod1$Location <- as.factor("College Cr")

## YearBuilt
CollegeCr.test.mod1$YearBuilt <- as.numeric(CollegeCr.test.mod1$YearBuilt)

## YrSold
CollegeCr.test.mod1$YrSold <- as.numeric(CollegeCr.test.mod1$YrSold)

## Compute age of the house
CollegeCr.test.mod1$HouseAge <- CollegeCr.test.mod1$YrSold - CollegeCr.test.mod1$YearBuilt 

## Remove Exterior and LotInfo columns
CollegeCr.test.mod1 <- subset(CollegeCr.test.mod1, select = -c(Exterior, LotInfo))

## Remove 'BsmtUnfSF' since this column is missing in the Edwards data set
CollegeCr.test.mod1 <- subset(CollegeCr.test.mod1, select = -BsmtUnfSF)

## Remove 'TotBsmtSF' since this is a temporary column
## Remove 'YearBuilt' and 'YrSold'
CollegeCr.test.mod1 <- subset(CollegeCr.test.mod1, select = -c(TotBsmtSF, YearBuilt, YrSold))

## Identify and remove duplicate rows
library(misty)
which(duplicated(CollegeCr.test.mod1))

## Remove the character uniqueID column and add a numeric unique ID to each record
CollegeCr.test.mod1 <- subset(CollegeCr.test.mod1, select = -uniqueID)
CollegeCr.test.mod1$uniqueID <- as.vector(seq(276, 305, by=1))
## Create an empty SalePrice column in the sample data frame
CollegeCr.test.mod1["SalePrice"] <- NA
CollegeCr.test.mod1 %>% relocate(uniqueID, SalePrice)

################################################################################
## ## Edwards Sample Dataset clean-up
################################################################################

Edwards.test <- read.csv(file="Edwards.test.csv")
Edwards.test.mod1 <- Edwards.test

## Checking for missing data in each column
NA.sum <- colSums(is.na(Edwards.test.mod1))
NA.sum
which(is.na(Edwards.test.mod1$BsmtCond))

# BsmtQual, BsmtCond, BsmtFinType1
Edwards.test.mod1$BsmtCond[is.na(Edwards.test.mod1$BsmtCond)] <- "Unknown"
Edwards.test.mod1$BsmtCond <- as.factor(Edwards.test.mod1$BsmtCond)

Edwards.test.mod1$BsmtQual[!(Edwards.test.mod1$BsmtQual %in% c("Ex", "Fa", "Gd", "TA"))] <- "Unknown"            
Edwards.test.mod1$BsmtQual <- as.factor(Edwards.test.mod1$BsmtQual)

Edwards.test.mod1$BsmtFinType1[!(Edwards.test.mod1$BsmtFinType1 %in% c("Unf","LwQ","BLQ", "ALQ", "GLQ"))] <- "Unknown"
Edwards.test.mod1$BsmtFinType1 <- as.factor(Edwards.test.mod1$BsmtFinType1)

## Electrical
Edwards.test.mod1$Electrical <- as.factor(Edwards.test.mod1$Electrical)

## HeatingQC
Edwards.test.mod1$HeatingQC <- factor(Edwards.test.mod1$HeatingQC, order = FALSE, 
                                      levels = c("Po", "Fa", "TA", "Gd", "Ex"))

# BsmtFinSF1
Edwards.test.mod1$BsmtFinSF1 <- as.numeric(Edwards.test.mod1$BsmtFinSF1)

## Foundation
Edwards.test.mod1$Foundation <- as.factor(Edwards.test.mod1$Foundation)

## RoofMatl
Edwards.test.mod1$RoofMatl <- as.factor(Edwards.test.mod1$RoofMatl)

## OverallCond
Edwards.test.mod1$OverallCond <- ordered(Edwards.test.mod1$OverallCond, levels = 1:10,
                                         labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                                    "Average", "Above Average", "Good", "Very Good",
                                                    "Excellent", "Very Excellent"))

## Utilities
Edwards.test.mod1$Utilities <- as.factor(Edwards.test.mod1$Utilities)

## WoodDeckSF
Edwards.test.mod1$WoodDeckSF <- as.numeric(Edwards.test.mod1$WoodDeckSF)

## LotInfo
library(purrr)
library(stringr)
library(tibble)
x1 <-str_split(Edwards.test.mod1$LotInfo, ";")
# To force map to return a character vector instead of a list, we can use map_chr
Lot1.dat <- tibble(map_chr(x1, 1),  
                   map_chr(x1, 2),
                   map_chr(x1, 3),
                   map_chr(x1, 4))
colnames(Lot1.dat) <- c("LotConfig", "LotShape", "LotArea","LotFrontage")
Edwards.test.mod1 <- cbind(Edwards.test.mod1, Lot1.dat)
Edwards.test.mod1$LotConfig <- as.factor(Edwards.test.mod1$LotConfig)
Edwards.test.mod1$LotShape <- as.factor(Edwards.test.mod1$LotShape)
Edwards.test.mod1$LotArea <- as.numeric(Edwards.test.mod1$LotArea)
Edwards.test.mod1$LotFrontage <- as.numeric(Edwards.test.mod1$LotFrontage)
Edwards.test.mod1$LotFrontage[is.na(Edwards.test.mod1$LotFrontage)] <- median(Edwards.test.mod1$LotFrontage, na.rm=TRUE)

## SaleType
Edwards.test.mod1$SaleType <- as.factor(Edwards.test.mod1$SaleType)

## BedroomAbvGr
Edwards.test.mod1$BedroomAbvGr <- as.numeric(Edwards.test.mod1$BedroomAbvGr)

## KitchenQual
Edwards.test.mod1$KitchenQual <- factor(Edwards.test.mod1$KitchenQual, order = FALSE, 
                                        levels = c("Fa", "TA", "Gd", "Ex"))

## HalfBath
Edwards.test.mod1$HalfBath <- as.numeric(Edwards.test.mod1$HalfBath)

## RoofStyle
Edwards.test.mod1$RoofStyle <- as.factor(Edwards.test.mod1$RoofStyle)

## GrLivArea
Edwards.test.mod1$GrLivArea <- as.numeric(Edwards.test.mod1$GrLivArea)

## OpenPorchSF
Edwards.test.mod1$OpenPorchSF <- as.numeric(Edwards.test.mod1$OpenPorchSF)

## Heating
Edwards.test.mod1$Heating <- as.factor(Edwards.test.mod1$Heating)

## Fireplaces
Edwards.test.mod1$Fireplaces <- as.numeric(Edwards.test.mod1$Fireplaces)

## BldgType
Edwards.test.mod1$BldgType <- as.factor(Edwards.test.mod1$BldgType)

## OverallQual 
Edwards.test.mod1$OverallQual <- ordered(Edwards.test.mod1$OverallQual, levels = 1:10,
                                         labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                                    "Average", "Above Average", "Good", "Very Good",
                                                    "Excellent", "Very Excellent"))
## KitchenAbvGr
Edwards.test.mod1$KitchenAbvGr <- as.numeric(Edwards.test.mod1$KitchenAbvGr)

## FullBath
Edwards.test.mod1$FullBath <- as.numeric(Edwards.test.mod1$FullBath)

## PavedDrive
Edwards.test.mod1$PavedDrive <- as.factor(Edwards.test.mod1$PavedDrive)

## GarageType
Edwards.test.mod1$GarageType[Edwards.test.mod1$GarageType != "Attchd" & Edwards.test.mod1$GarageType != "Detchd"] <- "NoGarage"
Edwards.test.mod1$GarageType <- as.factor(Edwards.test.mod1$GarageType)

## Central Air
Edwards.test.mod1$CentralAir <- as.factor(Edwards.test.mod1$CentralAir)

## Exterior
library(purrr)
library(stringr)
library(tibble)
y1 <-str_split(Edwards.test.mod1$Exterior, ";")
# To force map to return a character vector instead of a list, we can use map_chr
Exterior1.dat <- tibble(map_chr(y1, 1),  
                        map_chr(y1, 2),
                        map_chr(y1, 3))
colnames(Exterior1.dat) <- c("ExterCov", "ExterQual", "ExterCond")
Edwards.test.mod1 <- cbind(Edwards.test.mod1, Exterior1.dat)
Edwards.test.mod1$ExterCov <- as.factor(Edwards.test.mod1$ExterCov)
Edwards.test.mod1$ExterQual <- as.factor(Edwards.test.mod1$ExterQual)
Edwards.test.mod1$ExterCond <- as.factor(Edwards.test.mod1$ExterCond)

## HouseStyle
Edwards.test.mod1$HouseStyle <- as.factor(Edwards.test.mod1$HouseStyle)

## TotRmsAbvGrd
Edwards.test.mod1$TotRmsAbvGrd <- as.numeric(Edwards.test.mod1$TotRmsAbvGrd)

## Create Location column
Edwards.test.mod1$Location <- as.factor("Edwards")

## YearBuilt
Edwards.test.mod1$YearBuilt <- as.numeric(Edwards.test.mod1$YearBuilt)

## YrSold
Edwards.test.mod1$YrSold <- as.numeric(Edwards.test.mod1$YrSold)

## Compute age of the house
Edwards.test.mod1$HouseAge <- Edwards.test.mod1$YrSold - Edwards.test.mod1$YearBuilt   

## Remove Exterior and LotInfo columns
Edwards.test.mod1 <- subset(Edwards.test.mod1, select = -c(Exterior, LotInfo))

## Remove 'KitchenAbvGr' since this column is missing in the College Cr data set
## Remove 'YearBuilt' and 'YrSold'
Edwards.test.mod1 <- subset(Edwards.test.mod1, select = -c(KitchenAbvGr, YearBuilt, YrSold))

## Identify duplicate rows
library(misty)
which(duplicated(Edwards.test.mod1))

## Remove the character uniqueID column and add a numeric uniqueID to each record
Edwards.test.mod1 <- subset(Edwards.test.mod1, select = -uniqueID)
Edwards.test.mod1$uniqueID <- as.vector(seq(306, 320, by=1))
## Create an empty SalePrice column in the sample data frame
Edwards.test.mod1["SalePrice"] <- NA
Edwards.test.mod1 %>% relocate(uniqueID, SalePrice)

################################################################################
## Old Town Sample Dataset clean-up
################################################################################

OldTown.test <- read.csv(file="OldTown.test.csv")
OldTown.test.mod1 <- OldTown.test

## Checking for missing data in each column
NA.sum <- colSums(is.na(OldTown.test.mod1))
NA.sum
which(is.na(OldTown.test.mod1$BsmtCond))

# BsmtQual, BsmtCond, BsmtFinType1
OldTown.test.mod1$TotBsmtSF <- OldTown.test.mod1$BsmtFinSF1 + OldTown.test.mod1$BsmtUnfSF

OldTown.test.mod1$BsmtCond[is.na(OldTown.test.mod1$BsmtCond)] <- "NA"
OldTown.test.mod1$BsmtCond[OldTown.test.mod1$BsmtCond == "NA" & OldTown.test.mod1$TotBsmtSF == 0] <- "NoBsmt"
OldTown.test.mod1$BsmtCond[OldTown.test.mod1$BsmtCond == "NA" & OldTown.test.mod1$TotBsmtSF != 0] <- "Unknown"
OldTown.test.mod1$BsmtCond <- as.factor(OldTown.test.mod1$BsmtCond)

OldTown.test.mod1$BsmtQual[!(OldTown.test.mod1$BsmtQual %in% c("Ex", "Fa", "Gd", "TA"))] <- "NA"
OldTown.test.mod1$BsmtQual[OldTown.test.mod1$BsmtQual == "NA" & OldTown.test.mod1$TotBsmtSF == 0] <- "NoBsmt"
OldTown.test.mod1$BsmtQual[OldTown.test.mod1$BsmtQual == "NA" & OldTown.test.mod1$TotBsmtSF != 0] <- "Unknown"
OldTown.test.mod1$BsmtQual <- as.factor(OldTown.test.mod1$BsmtQual)

OldTown.test.mod1$BsmtFinType1[!(OldTown.test.mod1$BsmtFinType1 %in% c("GLQ", "ALQ", "BLQ", "LwQ", "Unf"))] <- "NA"
OldTown.test.mod1$BsmtFinType1[OldTown.test.mod1$BsmtFinType1 == "NA" & OldTown.test.mod1$TotBsmtSF == 0] <- "NoBsmt"
OldTown.test.mod1$BsmtFinType1[OldTown.test.mod1$BsmtFinType1 == "NA" & OldTown.test.mod1$TotBsmtSF != 0] <- "Unknown"
OldTown.test.mod1$BsmtFinType1 <- as.factor(OldTown.test.mod1$BsmtFinType1)

## Foundation
OldTown.test.mod1$Foundation <- as.factor(OldTown.test.mod1$Foundation)

## GrLivArea
OldTown.test.mod1$GrLivArea <- as.numeric(OldTown.test.mod1$GrLivArea)

## PavedDrive
OldTown.test.mod1$PavedDrive <- as.factor(OldTown.test.mod1$PavedDrive)

## RoofStyle
OldTown.test.mod1$RoofStyle <- as.factor(OldTown.test.mod1$RoofStyle)

## Fireplaces
OldTown.test.mod1$Fireplaces <- as.numeric(OldTown.test.mod1$Fireplaces)

## Lotinfo
library(stringr)
library(tibble)
y2 <-str_split(OldTown.test.mod1$LotInfo, ";")
# To force map to return a character vector instead of a list, we can use map_chr
Lot2.dat <- tibble(map_chr(y2, 1),  
                   map_chr(y2, 2),
                   map_chr(y2, 3),
                   map_chr(y2, 4))
colnames(Lot2.dat) <- c("LotConfig", "LotShape", "LotArea","LotFrontage")
OldTown.test.mod1 <- cbind(OldTown.test.mod1, Lot2.dat)
OldTown.test.mod1$LotConfig <- as.factor(OldTown.test.mod1$LotConfig)
OldTown.test.mod1$LotShape <- as.factor(OldTown.test.mod1$LotShape)
OldTown.test.mod1$LotArea <- as.numeric(OldTown.test.mod1$LotArea)
OldTown.test.mod1$LotFrontage <- as.numeric(OldTown.test.mod1$LotFrontage)
OldTown.test.mod1$LotFrontage[is.na(OldTown.test.mod1$LotFrontage)] <- median(OldTown.test.mod1$LotFrontage, na.rm=TRUE)

## WoodDeckSF
OldTown.test.mod1$WoodDeckSF <- as.numeric(OldTown.test.mod1$WoodDeckSF)

## BldgType
OldTown.test.mod1$BldgType <- as.factor(OldTown.test.mod1$BldgType)

## HalfBath
OldTown.test.mod1$HalfBath <- as.numeric(OldTown.test.mod1$HalfBath)

## FullBath
OldTown.test.mod1$FullBath <- as.numeric(OldTown.test.mod1$FullBath)

## KitchenQual
OldTown.test.mod1$KitchenQual <- factor(OldTown.test.mod1$KitchenQual, order = FALSE, 
                                        levels = c("Fa", "TA", "Gd", "Ex"))
## Basement conditions
# BsmtUnfSF
OldTown.test.mod1$BsmtUnfSF <- as.numeric(OldTown.test.mod1$BsmtUnfSF)

# BsmtFinSF1
OldTown.test.mod1$BsmtFinSF1 <- as.numeric(OldTown.test.mod1$BsmtFinSF1)

## Central Air
OldTown.test.mod1$CentralAir <- as.factor(OldTown.test.mod1$CentralAir)

## SaleType
OldTown.test.mod1$SaleType <- as.factor(OldTown.test.mod1$SaleType)

## Electrical
OldTown.test.mod1$Electrical <- as.factor(OldTown.test.mod1$Electrical)

## Exterior
library(purrr)
library(stringr)
library(tibble)
x2 <-str_split(OldTown.test.mod1$Exterior, ";")
# To force map to return a character vector instead of a list, we can use map_chr
Exterior2.dat <- tibble(map_chr(x2, 1),  
                        map_chr(x2, 2),
                        map_chr(x2, 3))
colnames(Exterior2.dat) <- c("ExterCov", "ExterQual", "ExterCond")
OldTown.test.mod1 <- cbind(OldTown.test.mod1, Exterior2.dat)
OldTown.test.mod1$ExterCov <- as.factor(OldTown.test.mod1$ExterCov)
OldTown.test.mod1$ExterQual <- as.factor(OldTown.test.mod1$ExterQual)
OldTown.test.mod1$ExterCond <- as.factor(OldTown.test.mod1$ExterCond)

## OpenPorchSF
OldTown.test.mod1$OpenPorchSF <- as.numeric(OldTown.test.mod1$OpenPorchSF)

## RoofMatl
OldTown.test.mod1$RoofMatl <- as.factor(OldTown.test.mod1$RoofMatl)

## HeatingQC
OldTown.test.mod1$HeatingQC <- factor(OldTown.test.mod1$HeatingQC, order = FALSE, 
                                      levels = c("Po", "Fa", "TA", "Gd", "Ex"))

## BedroomAbvGr 
OldTown.test.mod1$BedroomAbvGr <- as.numeric(OldTown.test.mod1$BedroomAbvGr)

## Heating
OldTown.test.mod1$Heating <- as.factor(OldTown.test.mod1$Heating)

## OverallQual 
OldTown.test.mod1$OverallQual <- ordered(OldTown.test.mod1$OverallQual, levels = 1:10,
                                         labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                                    "Average", "Above Average", "Good", "Very Good",
                                                    "Excellent", "Very Excellent"))

## KitchenAbvGr
OldTown.test.mod1$KitchenAbvGr <- as.numeric(OldTown.test.mod1$KitchenAbvGr)

## GarageType
OldTown.test.mod1$GarageType[OldTown.test.mod1$GarageType != "Attchd" & OldTown.test.mod1$GarageType != "Detchd"] <- "NoGarage"
OldTown.test.mod1$GarageType <- as.factor(OldTown.test.mod1$GarageType)

## HouseStyle
OldTown.test.mod1$HouseStyle <- as.factor(OldTown.test.mod1$HouseStyle)

## OverallCond
OldTown.test.mod1$OverallCond <- ordered(OldTown.test.mod1$OverallCond, levels = 1:10,
                                         labels = c("Very Poor", "Poor", "Fair", "Below Average",
                                                    "Average", "Above Average", "Good", "Very Good",
                                                    "Excellent", "Very Excellent"))

## TotRmsAbvGrd
OldTown.test.mod1$TotRmsAbvGrd <- as.numeric(OldTown.test.mod1$TotRmsAbvGrd)

## Utilities
OldTown.test.mod1$Utilities <- as.factor(OldTown.test.mod1$Utilities)

## Create Location column
OldTown.test.mod1$Location <- as.factor("Old town")

## YearBuilt
OldTown.test.mod1$YearBuilt <- as.numeric(OldTown.test.mod1$YearBuilt)

## YrSold
OldTown.test.mod1$YrSold <- as.numeric(OldTown.test.mod1$YrSold)

## Compute age of the house
OldTown.test.mod1$HouseAge <- OldTown.test.mod1$YrSold - OldTown.test.mod1$YearBuilt  

## Remove Exterior and LotInfo columns
OldTown.test.mod1 <- subset(OldTown.test.mod1, select = -c(Exterior, LotInfo))

## Remove 'KitchenAbvGr' and 'BsmtUnfSF' since these column are missing in the other two data sets
## Remove 'TotBsmtSF' since this is a temporary column
## Remove 'YearBuilt' and 'YrSold'
OldTown.test.mod1 <- subset(OldTown.test.mod1, select = -c(KitchenAbvGr, BsmtUnfSF, TotBsmtSF, YearBuilt, YrSold))

## Identify and remove duplicate rows
library(misty)
which(duplicated(OldTown.test.mod1))

## Remove the character uniqueID column and add a numeric uniqueID to each record
OldTown.test.mod1 <- subset(OldTown.test.mod1, select = -uniqueID)
OldTown.test.mod1$uniqueID <- as.vector(seq(321, 342, by=1))
## Create an empty SalePrice column in the sample data frame
OldTown.test.mod1["SalePrice"] <- NA
OldTown.test.mod1 %>% relocate(uniqueID, SalePrice)

################################################################################
## Concatenate all three sample datasets 
################################################################################

## Concatenate CollegeCr and Edwards data frames
CombinedSample.df <- rbind(CollegeCr.test.mod1, Edwards.test.mod1, OldTown.test.mod1)
CombinedSample.df$dataset <- as.factor("Sample")

################################################################################
## Combine the training and sample data sets
################################################################################

Combined.df <- rbind(CombinedTrain.df, CombinedSample.df)




