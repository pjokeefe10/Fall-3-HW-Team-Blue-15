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
library(caret)
library(leaps)
library(earth)
library(mgcv)

#### read in data #####
train <- read.csv("C:/Users/kat4538/Documents/MSA/FALL 3/machine learning/hw 1/insurance_t.csv")

# DETERMINE TYPE OF VARIABLES
# str(train)
# var <- sapply(train, n_distinct) 
# write.csv(var, "C:/Users/kat4538/Documents/MSA/FALL 3/machine learning/hw 1/var.csv")

##### check which variables have missing #####
gg_miss_var(train) #HMOWN, INV, CCPURC, CC have missing values

##### change missing value to missing category for categorical variables #####
# train$INV[is.na(train$INV)] <- "M"
# train %>% count(INV)
# 
# train$CC[is.na(train$CC)] <- "M"
# train %>% count(CC)
# 
# train$CCPURC[is.na(train$CCPURC)] <- "M"
# train %>% count(CCPURC)

# coerce all binary and categorical to factor (16 cols)

#convert all categorical var to factors
col_names <- names(train)[c(2,7:8,12,14,18,20,22,24,26:27,29:30,36,38)]
train[,col_names] <- lapply(train[,col_names] , factor)

# create binary flag col for all variables
flag  = train %>%
  mutate(across(everything(), ~ is.na(.x), 
                .names = 'FLAG_NA_{.col}'))


# then drop col if there are no missing values
flag_sub = flag[39:ncol(flag)][colSums(abs(flag[39:ncol(flag)]), na.rm = TRUE) > 0]

# then drop all cols that are not numerical (because we only want the flag for numerical variables)
drop_flag = c('FLAG_NA_DDA','FLAG_NA_DIRDEP','FLAG_NA_NSF','FLAG_NA_SAV'
              ,'FLAG_NA_ATM','FLAG_NA_CD','FLAG_NA_IRA','FLAG_NA_INV','FLAG_NA_MM','FLAG_NA_MMCRED',
              'FLAG_NA_CC','FLAG_NA_CCPURC','FLAG_NA_SDB','FLAG_NA_INAREA','FLAG_NA_INS', 'FLAG_NA_BRANCH')
drop_vars <- names(flag_sub) %in% drop_flag

flag_sub_sub <- flag_sub[!drop_vars]

# add flags back to original data frame

train <- cbind(train, flag_sub_sub)

########## impute missing values ###########
# mode for categorical var
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

train$INV <- if_else(is.na(train$INV), calc_mode(train$INV), train$INV)
train$CC <- if_else(is.na(train$CC), calc_mode(train$CC), train$CC)
train$CCPURC <- if_else(is.na(train$CCPURC), calc_mode(train$CCPURC), train$CCPURC)
hist(train$INV)
hist(train$CC)
hist(train$CCPURC)

# median for continuous var
train <- train %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

##### check each variable for separation problems #####
# create list of categorical variable names
all <- names(train)[c(2,7:8,12,14,18,20,22,24,26:27,29:30,36,38)]

# print table for each column against INS
for(i in all){
  table_temp <- train %>% 
    dplyr::select(INS, i) %>% 
    table()
  
  print(table_temp) #MMCRED has quasi-separation
}

# roll up categories
train$MMCRED <- as.character(train$MMCRED)
train$MMCRED[which(train$MMCRED > 2)] <- "3+" # new category for 3+ money market credits
table(train$INS, train$MMCRED)

##### MARS modeling #####
# EARTH on all variables
mars <- earth(INS ~ ., data = train, glm=list(family=binomial))
summary(mars)

# Variable importance metric
evimp(mars2)

# ROC Curve MARS
train$p_hat <- predict(mars, type = "response")
p1 <- train$p_hat[train$Bonus == 1]
p0 <- train$p_hat[train$Bonus == 0] 

InformationValue::plotROC(train$INS, train$p_hat)
