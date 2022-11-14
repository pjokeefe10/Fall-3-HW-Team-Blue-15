############ HW 2 MACHINE LEARNING ###############
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
library(ROCR)
library(randomForest)
library(xgboost)
library(Ckmeans.1d.dp)
library(pdp)

#### read in data #####
train <- read.csv("https://github.com/pjokeefe10/Fall-3-HW-Team-Blue-15/raw/main/insurance_t.csv")

train$INS <- factor(train$INS)

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

train.df <- as.data.frame(train)
train.df$random <- rnorm(8495)

# Random Forest model
set.seed(444)
rf.bank <- randomForest(INS ~ ., data = train.df, ntree = 500, importance = TRUE)

# Plot the change in error across different number of trees
plot(rf.bank, main = "Number of Trees Compared to MSE")

# Tune an random forest mtry value
set.seed(444)
tuneRF(x = train.df[,-37], y = train.df[,"INS"], 
       plot = TRUE, ntreeTry = 500, stepFactor = 0.5)

set.seed(444)
rf.bank <- randomForest(INS ~ ., data = train.df, ntree = 200, mtry = 6, importance = TRUE)

varImpPlot(rf.bank,
           sort = TRUE,
           n.var = 15,
           main = "Order of Variables")
importance <- importance(rf.bank, type = 1)
importance2 <- importance(rf.bank, type = 2)


#############################################################
# Prepare data for XGBoost function - similar to what we did for glmnet
train_x <- model.matrix(INS ~ ., data = train)[, -37]
train_y <- as.double(train$INS)

xgb.ames <- xgboost(data = train_x, label = train_y, subsample = 0.5, nrounds = 100, objective = "binary:logistic")


xgbcv.bank <- xgb.cv(data = train_x, label = train_y, subsample = 0.5, nrounds = 100, nfold = 10, obj = "binary:logistic")


set.seed(444)
tune_grid <- expand.grid(
  nrounds = 24,
  eta = c(0.1, 0.15, 0.2, 0.25, 0.3),
  max_depth = c(1:10),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.25, 0.5, 0.75, 1)
)

set.seed(12345)
xgb.ames.caret <- train(x = train_x, y = train_y,
                        method = "xgbTree",
                        objective = "binary:logistic",
                        tuneGrid = tune_grid,
                        trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                 number = 10))

