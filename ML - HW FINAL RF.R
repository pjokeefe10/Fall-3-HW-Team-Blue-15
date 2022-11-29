# libraries
library(tidyverse)
library(ggplot2)
library(UsingR)
library(car)
library(stats)
library(DescTools)
library(AppliedPredictiveModeling)
library(lmtest)
library(vcdExtra)
library(InformationValue)
library(naniar)
library(dplyr)
library(caret)
library(leaps)
library(mgcv)
library(ROCR)
library(pROC)
library(nnet)
library(NeuralNetTools)
library(iml)
library(e1071)
library(caret)
library(patchwork)
library(glmnet)
library(randomForest)
library(gower)

# read in data
train <- read.csv("https://github.com/pjokeefe10/Fall-3-HW-Team-Blue-15/raw/main/insurance_t.csv")
valid <- read.csv("https://github.com/pjokeefe10/Fall-3-HW-Team-Blue-15/raw/main/insurance_v.csv")

########################################## imputation from phase 1 ###############################################################

########################################## imputation from phase 1 ###############################################################

train$INS <- factor(train$INS)


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

# actual impuration
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

# roll up categories
train$MMCRED <- as.character(train$MMCRED)
train$MMCRED[which(train$MMCRED > 2)] <- "3+" # new category for 3+ money market credits

###################################################### validation impuation prep ################################################

valid$INS <- factor(valid$INS)

# coerce all binary and categorical to factor (16 cols)

#convert all categorical var to factors
col_names <- names(valid)[c(2,7:8,12,14,18,20,22,24,26:27,29:30,36,38)]
valid[,col_names] <- lapply(valid[,col_names] , factor)

# create binary flag col for all variables
flag  = valid %>%
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

valid <- cbind(valid, flag_sub_sub)

# actual imputation

valid$INV <- if_else(is.na(valid$INV), calc_mode(valid$INV), valid$INV)
valid$CC <- if_else(is.na(valid$CC), calc_mode(valid$CC), valid$CC)
valid$CCPURC <- if_else(is.na(valid$CCPURC), calc_mode(valid$CCPURC), valid$CCPURC)

# median for continuous var
valid <- valid %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

# roll up categories
valid$MMCRED <- as.character(valid$MMCRED)
valid$MMCRED[which(valid$MMCRED > 2)] <- "3+" # new category for 3+ money market credits

###################################################### validation impuation prep ################################################

valid$INS <- factor(valid$INS)

# coerce all binary and categorical to factor (16 cols)

#convert all categorical var to factors
col_names <- names(valid)[c(2,7:8,12,14,18,20,22,24,26:27,29:30,36,38)]
valid[,col_names] <- lapply(valid[,col_names] , factor)

# create binary flag col for all variables
flag  = valid %>%
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

valid <- cbind(valid, flag_sub_sub)

# actual imputation

valid$INV <- if_else(is.na(valid$INV), calc_mode(valid$INV), valid$INV)
valid$CC <- if_else(is.na(valid$CC), calc_mode(valid$CC), valid$CC)
valid$CCPURC <- if_else(is.na(valid$CCPURC), calc_mode(valid$CCPURC), valid$CCPURC)

# median for continuous var
valid <- valid %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

# roll up categories
#valid$MMCRED <- as.character(valid$MMCRED)
valid$MMCRED[which(valid$MMCRED > 2)] <- '3+' # new category for 3+ money market credits

train$INS <- factor(train$INS)
train <- train %>%
  mutate(
    FLAG_NA_ACCTAGE = as.factor(FLAG_NA_ACCTAGE),
    FLAG_NA_PHONE= as.factor(FLAG_NA_PHONE),
    FLAG_NA_POS = as.factor(FLAG_NA_POS),
    FLAG_NA_POSAMT = as.factor(FLAG_NA_POSAMT),
    FLAG_NA_INVBAL = as.factor(FLAG_NA_INVBAL),
    FLAG_NA_CCBAL = as.factor(FLAG_NA_CCBAL),
    FLAG_NA_INCOME = as.factor(FLAG_NA_INCOME),
    FLAG_NA_LORES = as.factor(FLAG_NA_LORES),
    FLAG_NA_HMVAL = as.factor(FLAG_NA_HMVAL),
    FLAG_NA_AGE = as.factor(FLAG_NA_AGE),
    FLAG_NA_CRSCORE = as.factor(FLAG_NA_CRSCORE))

valid <- valid %>%
  mutate(
    FLAG_NA_ACCTAGE = as.factor(FLAG_NA_ACCTAGE),
    FLAG_NA_PHONE= as.factor(FLAG_NA_PHONE),
    FLAG_NA_POS = as.factor(FLAG_NA_POS),
    FLAG_NA_POSAMT = as.factor(FLAG_NA_POSAMT),
    FLAG_NA_INVBAL = as.factor(FLAG_NA_INVBAL),
    FLAG_NA_CCBAL = as.factor(FLAG_NA_CCBAL),
    FLAG_NA_INCOME = as.factor(FLAG_NA_INCOME),
    FLAG_NA_LORES = as.factor(FLAG_NA_LORES),
    FLAG_NA_HMVAL = as.factor(FLAG_NA_HMVAL),
    FLAG_NA_AGE = as.factor(FLAG_NA_AGE),
    FLAG_NA_CRSCORE = as.factor(FLAG_NA_CRSCORE))


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

# actual impuration
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

# roll up categories
train$MMCRED <- as.character(train$MMCRED)
train$MMCRED[which(train$MMCRED > 2)] <- '3+' # new category for 3+ money market credits

###################################################### validation impuation prep ################################################

valid$INS <- factor(valid$INS)

# coerce all binary and categorical to factor (16 cols)

#convert all categorical var to factors
col_names <- names(valid)[c(2,7:8,12,14,18,20,22,24,26:27,29:30,36,38)]
valid[,col_names] <- lapply(valid[,col_names] , factor)

# create binary flag col for all variables
flag  = valid %>%
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

valid <- cbind(valid, flag_sub_sub)

# actual imputation

valid$INV <- if_else(is.na(valid$INV), calc_mode(valid$INV), valid$INV)
valid$CC <- if_else(is.na(valid$CC), calc_mode(valid$CC), valid$CC)
valid$CCPURC <- if_else(is.na(valid$CCPURC), calc_mode(valid$CCPURC), valid$CCPURC)

# median for continuous var
valid <- valid %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

# roll up categories
valid$MMCRED <- as.character(valid$MMCRED)
valid$MMCRED[which(valid$MMCRED > 2)] <- '3+' # new category for 3+ money market credits

############ Random Forest #####################################################################################################
set.seed(444)
# rf <- randomForest(INS ~ . - LORES - FLAG_NA_CRSCORE - FLAG_NA_INCOME
#                    - FLAG_NA_ACCTAGE - SDB - NSFAMT - INAREA,
#                    data = train, ntree = 200, mtry = 8, importance = TRUE)
rf <- randomForest(INS ~ .,
                   data = train, ntree = 200, mtry = 8, importance = TRUE)
importance(rf)
#10 folds 
# control <- trainControl(method='cv',
#                         number=10, classProbs= TRUE)
# 
# set.seed(444)
# 
# 
# tunegrid <- expand.grid(.mtry=8)
# rf <- train(make.names(INS)~.,
#                     data=train,
#                     method='rf',
#                     eval_metric='ROC',
#                     tuneGrid=tunegrid,
#                     trControl=control, ntree = 200)


# determine ROC and accuracy on validation
valid_p <- valid
valid_p$p_hat <- predict(rf, newdata=valid_p, type = "prob")[,2]

#ROC curve
pred.rf <- prediction(valid_p$p_hat, valid_p$INS) 
perf.rf <- performance(pred.rf, measure = "tpr", x.measure = "fpr")
plot(perf.rf, lwd = 3, col = "dodgerblue3", main = paste0("Random Forest ROC Plot (AUC = ", round(AUROC(valid_p$INS, valid_p$p_hat), 3),")"), 
     xlab = "False Positive",
     ylab = "True Positive")
abline(a = 0, b = 1, lty = 3)

#create predictions
rf_pred <- predict(rf, valid, type="response")

#confusion matrix
rf_cMatrix <- table(rf_pred, valid$INS)

confusionMatrix(rf_cMatrix)

################ Variable Interest ##############################################################################################
predictor_rf <- Predictor$new(rf, data = train[,-37], y = train$INS, type = "prob")

########### Observation Interest (LIME and Shapely) #############################################################################

# LIME
point <- 732
lime.explain_rf <- LocalModel$new(predictor_rf,
                                  x.interest = train[point,-37],
                                  k = 5)
plot(lime.explain_rf)


# Shapely 
shap <- Shapley$new(predictor_rf,
                    x.interest = train[point,-37])
shap$plot()

###### Global interpretation
# ale_plot <- FeatureEffects$new(predictor_rf, method = "ale")
# ale_plot$plot(c("ACCTAGE"))

pdp_plot <- FeatureEffects$new(predictor_rf, method = "pdp")
pdp_plot$plot(c("ACCTAGE"))
