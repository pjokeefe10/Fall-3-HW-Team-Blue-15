# Machine Learning Homework 2

# Author: Sam Weiner
# Version: 2022-11-15

# Packages
library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(Ckmeans.1d.dp)
library(pdp)
library(InformationValue)

#Parameters
train <- read_csv("insurance_t.csv")

# Code ===============================

# Data Pre-processing ------


## Missing value imputation
#convert all categorical var to factors
col_names <- names(train)[c(2,7:8,12,14,18,20,22,24,26:27,29:30,36,38)]
train[,col_names] <- lapply(train[,col_names] , factor)

# create binary flag col for all variables
flag  = train %>%
  mutate(across(everything(), ~ is.na(.x), 
                .names = 'FLAG_NA_{.col}'))


# drop col if there are no missing values
flag_sub = flag[39:ncol(flag)][colSums(abs(flag[39:ncol(flag)]), na.rm = TRUE) > 0]

# drop all cols that are not numerical (because we only want the flag for numerical variables)
drop_flag = c('FLAG_NA_DDA','FLAG_NA_DIRDEP','FLAG_NA_NSF','FLAG_NA_SAV'
              ,'FLAG_NA_ATM','FLAG_NA_CD','FLAG_NA_IRA','FLAG_NA_INV','FLAG_NA_MM','FLAG_NA_MMCRED',
              'FLAG_NA_CC','FLAG_NA_CCPURC','FLAG_NA_SDB','FLAG_NA_INAREA','FLAG_NA_INS', 'FLAG_NA_BRANCH')
drop_vars <- names(flag_sub) %in% drop_flag

flag_sub_sub <- flag_sub[!drop_vars]

# add flags back to original data frame

train <- cbind(train, flag_sub_sub)

# mode for categorical var
calc_mode <- function(x){
  
  # list the distinct / unique values
  distinct_values <- unique(x)
  
  # count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

train$INV <- if_else(is.na(train$INV), calc_mode(train$INV), train$INV)
train$CC <- if_else(is.na(train$CC), calc_mode(train$CC), train$CC)
train$CCPURC <- if_else(is.na(train$CCPURC), calc_mode(train$CCPURC), train$CCPURC)

# median for continuous var
train <- train %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

##### check each variable for separation problems
# create list of categorical variable names
all <- names(train)[c(2,7:8,12,14,18,20,22,24,26:27,29:30,36,38)]

# print table for each column against INS
for(i in all){
  table_temp <- train %>% 
    dplyr::select(INS, all_of(i)) %>% 
    table()
  
  print(table_temp) #MMCRED has quasi-separation
}

# roll up categories
train$MMCRED <- as.character(train$MMCRED)
train$MMCRED[which(train$MMCRED > 2)] <- "3+" # new category for 3+ money market credits
table(train$INS, train$MMCRED)



# Random Forest -----------------------------------------------------------

set.seed(444)

rf <- randomForest( factor( INS ) ~ ., data = train, ntree = 500, 
                    importance = TRUE )

plot(rf)

varImpPlot(rf,
           sort = TRUE,
           n.var = 15)
importance(rf)

##Tune Random Forest -----------
set.seed( 444 )

tuneRF( x = train[ , -37 ], y = factor( train[ , 37 ] ), plot = TRUE, ntreeTry = 500, 
       stepFactor = 0.5 )

set.seed( 444 )
rf <- randomForest( factor( INS ) ~ ., data = train, mtry = 6, ntree = 500, 
                    importance = TRUE )

x <- predict(rf, type = "response")
x <- as.numeric(x)
x <- x-1

InformationValue::plotROC(train$INS, x)

varImpPlot(rf,
           sort = TRUE,
           n.var = 15)
importance(rf, type = 1)

##Interpret variables using partial dependence plots

partialPlot(rf, train, SAVBAL)


# XGBOOST -----------------------------------------------------------------

## Prepare data for XGBOOST -----------------------------------------------

train_x <- model.matrix(INS ~ ., data = train)[,-1]
train_y <- train$INS

##Build XGBOOST Model -----------------------------------------------------

set.seed( 444 )

param <- list(objective = "binary:logistic", eval_metric = "auc", subsample = 0.5)

xgb <- xgboost( data = train_x, label = train_y, nrounds = 100, 
               objective = "binary:logistic", eval_metric = "auc" )

###XGBOOST cross val for best nrounds value
set.seed( 444 )
xgb_cv <- xgb.cv( data = train_x, label = train_y, nrounds = 100, 
                 objective = "binary:logistic", eval_metric = "auc", 
                 nfold = 10 )

#Best nround = 14

##Caret -------------------------------------------------------------------

trcrtl <- trainControl( method = "repeatedcv", number = 10, repeats = 2 )

tune_grid <- expand.grid (nrounds = 14,
                         eta = c( 0.1, 0.15, 0.2, 0.25, 0.3 ),
                         max_depth = c(1:10),
                         gamma = c( 0 ),
                         colsample_bytree = 1,
                         min_child_weight = 1,
                         subsample = c( 0.25, 0.5, 0.75, 1) )
set.seed( 444 )
xgb_caret <- train( x = train_x,
                   y = train_y,
                   tuneGrid = tune_grid,
                   trControl = trcrtl,
                   method = "xgbTree",
                   objective = "binary:logistic", 
                   eval_metric = "auc",
                   verbose = TRUE)
xgb_caret$bestTune
plot(xgb_caret)

##Rerun base xgboost function with best tune

xgb_best <- xgboost( data = train_x, label = train_y, nrounds = 14, 
                    objective = "binary:logistic", eval_metric = "auc", 
                    max_depth = 5, eta = 0.3, gamma = 0, 
                    colsample_bytree = 1, min_child_weight = 1, 
                    subsample = 1 )

xgb.importance(feature_names = colnames(train_x),model = xgb_best)

xgb.ggplot.importance( xgb.importance( feature_names = colnames( train_x ),
                                     model = xgb_best) )

##Add a random variable

set.seed( 444 )
train$random <- rnorm(nrow(train))

train_x <- model.matrix(INS ~ ., data = train)
train_y <- train$INS

set.seed( 444 )
xgb_best <- xgboost( data = train_x, label = train_y, nrounds = 14, 
                     objective = "binary:logistic", eval_metric = "auc", 
                     max_depth = 5, eta = 0.3, gamma = 0, 
                     colsample_bytree = 1, min_child_weight = 1, 
                     subsample = 1 )
xgb.importance(feature_names = colnames(train_x),model = xgb_best)

xgb.ggplot.importance( xgb.importance( feature_names = colnames( train_x ),
                                       model = xgb_best) )

partial(xgb_best, pred.var = "SAVBAL", 
        plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = 'lattice',
        train = train_x, pdp.color = "red")
