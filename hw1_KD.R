############ HW 1 MACHINE LEARNING ###############
library(tidyverse)
library(ggplot2)
library(UsingR)
library(car)
library(stats)
library(DescTools)
library(AppliedPredictiveModeling)
library(lmtest)
library(MASS)
library(glmnet)
library(vcdExtra)
library(gmodels)
library(InformationValue)
library(naniar)
library(dplyr)

#### read in data #####
train <- read.csv("C:/Users/kat4538/Documents/MSA/FALL 3/machine learning/hw 1/insurance_t.csv")

# DETERMINE TYPE OF VARIABLES
# str(train)
# var <- sapply(train, n_distinct) 
# write.csv(var, "C:/Users/kat4538/Documents/MSA/FALL 3/machine learning/hw 1/var.csv")

##### check which variables have missing #####
gg_miss_var(train) #HMOWN, INV, CCPURC, CC have missing values

##### change missing value to missing category for categorical variables #####
train$INV[is.na(train$INV)] <- "M"
train %>% count(INV)

train$CC[is.na(train$CC)] <- "M"
train %>% count(CC)

train$CCPURC[is.na(train$CCPURC)] <- "M"
train %>% count(CCPURC)

##### check each variable for separation problems #####
# create list of predictor variable names
all <- names(train)[c(2,7:8,12,14,18,20,22,24,26:27,29:30,36,38)]

# print table for each column against INS
for(i in all){
  table_temp <- train %>% 
    dplyr::select(INS, i) %>% 
    table()
  
  print(table_temp) #CASHBK & MMCRED have quasi-separation
}

# combine categories
train$MMCRED <- as.character(train$MMCRED)
train$MMCRED[which(train$MMCRED > 2)] <- "3+" # new category for 3+ money market credits
table(train$INS, train$MMCRED)

#convert all ordinal & binary var to factors
col_names <- names(train)[c(2,7:8,12,14,18,20,22,24,26:27,29:30,36)]
train[,col_names] <- lapply(train[,col_names] , factor)
