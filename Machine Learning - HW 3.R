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

# read in data
train <- read.csv("https://github.com/pjokeefe10/Fall-3-HW-Team-Blue-15/raw/main/insurance_t.csv")
valid <- read.csv("https://github.com/pjokeefe10/Fall-3-HW-Team-Blue-15/raw/main/insurance_v.csv")

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

###################################################### run the same thing, but on validation

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

############## Neural Network ###########################################################
# need a dataframe structure
train_df <- as.data.frame(train)
# also standardize continuous vars
train_df <- train_df %>%
  mutate(s_ACCTAGE = scale(ACCTAGE),
         s_DDABAL = scale(DDABAL),
         s_DEP = scale(DEP),
         s_DEPAMT = scale(DEPAMT),
         s_CHECKS = scale(CHECKS),
         s_NSFAMT = scale(NSFAMT),
         s_PHONE = scale(PHONE),
         s_TELLER = scale(TELLER),
         s_SAVBAL = scale(SAVBAL),
         s_ATMAMT = scale(ATMAMT),
         s_POS = scale(POS),
         s_POSAMT = scale(POSAMT),
         s_CDBAL = scale(CDBAL),
         s_IRABAL = scale(IRABAL),
         s_INVBAL = scale(INVBAL),
         s_MMBAL = scale(MMBAL),
         s_CCBAL = scale(CCBAL),
         s_INCOME = scale(INCOME),
         s_LORES = scale(LORES),
         s_HMVAL = scale(HMVAL),
         s_AGE = scale(AGE),
         s_CRSCORE = scale(CRSCORE)
         )

# optimize tuning
tune_grid <- expand.grid(
  .size = c(0:5),
  .decay = c(0,  .025, .05, .1)
)
set.seed(444)

nn_bank_caret <- train(INS ~ s_ACCTAGE +s_DDABAL + s_DEP + s_DEPAMT + s_CHECKS + s_NSFAMT + s_PHONE 
                       + s_TELLER + s_SAVBAL + s_ATMAMT + s_POS + s_POSAMT + s_CDBAL + s_IRABAL + s_INVBAL +
                         s_MMBAL + s_CCBAL + s_INCOME + s_LORES + s_HMVAL + s_AGE + s_CRSCORE +
                         DDA + DIRDEP + NSF + ATM + CD + IRA + INV + MM + CC + CCPURC + SDB + INAREA +
                         BRANCH + FLAG_NA_ACCTAGE + FLAG_NA_PHONE + FLAG_NA_POS  + FLAG_NA_POSAMT + FLAG_NA_INVBAL   
                       + FLAG_NA_CCBAL + FLAG_NA_INCOME + FLAG_NA_LORES + FLAG_NA_HMVAL + FLAG_NA_AGE + FLAG_NA_CRSCORE 
                       , data = train_df,
                       method = "nnet",
                       tuneGrid = tune_grid,
                       trControl = trainControl(method = 'cv', number = 10),
                       trace = FALSE, linout = F)
nn_bank_caret$bestTune

# then use best tune

set.seed(444)
nn_bank <- nnet(INS ~ s_ACCTAGE +s_DDABAL + s_DEP + s_DEPAMT + s_CHECKS + s_NSFAMT + s_PHONE 
                + s_TELLER + s_SAVBAL + s_ATMAMT + s_POS + s_POSAMT + s_CDBAL + s_IRABAL + s_INVBAL +
                  s_MMBAL + s_CCBAL + s_INCOME + s_LORES + s_HMVAL + s_AGE + s_CRSCORE +
                  DDA + DIRDEP + NSF + ATM + CD + IRA + INV + MM + CC + CCPURC + SDB + INAREA +
                  BRANCH + FLAG_NA_ACCTAGE + FLAG_NA_PHONE + FLAG_NA_POS  + FLAG_NA_POSAMT + FLAG_NA_INVBAL   
                + FLAG_NA_CCBAL + FLAG_NA_INCOME + FLAG_NA_LORES + FLAG_NA_HMVAL + FLAG_NA_AGE + FLAG_NA_CRSCORE 
, data = train_df, size = 5, decay = 0, linout = F)

# determine ROC and accuracy


############## Naive Bayes ###############################################################

# tuning parameters
tune_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = c(0, 0.5, 1), adjust = c(TRUE, FALSE)
)

set.seed(444)
nb_bank_caret_nb <- train(INS ~ s_ACCTAGE +s_DDABAL + s_DEP + s_DEPAMT + s_CHECKS + s_NSFAMT + s_PHONE 
                          + s_TELLER + s_SAVBAL + s_ATMAMT + s_POS + s_POSAMT + s_CDBAL + s_IRABAL + s_INVBAL +
                            s_MMBAL + s_CCBAL + s_INCOME + s_LORES + s_HMVAL + s_AGE + s_CRSCORE +
                            DDA + DIRDEP + NSF + ATM + CD + IRA + INV + MM + CC + CCPURC + SDB + INAREA +
                            BRANCH + FLAG_NA_ACCTAGE + FLAG_NA_PHONE + FLAG_NA_POS  + FLAG_NA_POSAMT + FLAG_NA_INVBAL   
                          + FLAG_NA_CCBAL + FLAG_NA_INCOME + FLAG_NA_LORES + FLAG_NA_HMVAL + FLAG_NA_AGE + FLAG_NA_CRSCORE, 
                          data = train_df,
                       method = "nb",
                       tuneGrid = tune_grid,
                       trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                number = 10))

nb_bank_caret_nb$bestTune
# now bulid nb

set.seed(444)
nb_bank <- naiveBayes(INS ~ s_ACCTAGE +s_DDABAL + s_DEP + s_DEPAMT + s_CHECKS + s_NSFAMT + s_PHONE 
                      + s_TELLER + s_SAVBAL + s_ATMAMT + s_POS + s_POSAMT + s_CDBAL + s_IRABAL + s_INVBAL +
                        s_MMBAL + s_CCBAL + s_INCOME + s_LORES + s_HMVAL + s_AGE + s_CRSCORE +
                        DDA + DIRDEP + NSF + ATM + CD + IRA + INV + MM + CC + CCPURC + SDB + INAREA +
                        BRANCH + FLAG_NA_ACCTAGE + FLAG_NA_PHONE + FLAG_NA_POS  + FLAG_NA_POSAMT + FLAG_NA_INVBAL   
                      + FLAG_NA_CCBAL + FLAG_NA_INCOME + FLAG_NA_LORES + FLAG_NA_HMVAL + FLAG_NA_AGE + FLAG_NA_CRSCORE 
                      , data = train_df, laplace = 0, usekernel = FALSE)

# determine ROC and accuracy

########### LIME and Shapely #############################################################################

train_sub <- subset(train_df, select = c(INS, s_ACCTAGE ,s_DDABAL , s_DEP , s_DEPAMT , s_CHECKS , s_NSFAMT , s_PHONE 
                                         , s_TELLER , s_SAVBAL , s_ATMAMT , s_POS , s_POSAMT , s_CDBAL , s_IRABAL , s_INVBAL ,
                                         s_MMBAL , s_CCBAL , s_INCOME , s_LORES , s_HMVAL , s_AGE , s_CRSCORE ,
                                         DDA , DIRDEP , NSF , ATM , CD , IRA , INV , MM , CC , CCPURC , SDB , INAREA ,
                                         BRANCH , FLAG_NA_ACCTAGE , FLAG_NA_PHONE , FLAG_NA_POS  , FLAG_NA_POSAMT , FLAG_NA_INVBAL   
                                         , FLAG_NA_CCBAL , FLAG_NA_INCOME , FLAG_NA_LORES , FLAG_NA_HMVAL , FLAG_NA_AGE ))
# FLAG_NA_CRSCOR was removed?
# need to convert all scaled vars into numeric?

train_sub <- train_sub %>%
  mutate(s_ACCTAGE = as.numeric(s_ACCTAGE),
         s_DDABAL = as.numeric(s_DDABAL),
         s_DEP = as.numeric(s_DEP),
         s_DEPAMT = as.numeric(s_DEPAMT),
         s_CHECKS = as.numeric(s_CHECKS),
         s_NSFAMT = as.numeric(s_NSFAMT),
         s_PHONE = as.numeric(s_PHONE),
         s_TELLER = as.numeric(s_TELLER),
         s_SAVBAL = as.numeric(s_SAVBAL),
         s_ATMAMT = as.numeric(s_ATMAMT),
         s_POS = as.numeric(s_POS),
         s_POSAMT = as.numeric(s_POSAMT),
         s_CDBAL = as.numeric(s_CDBAL),
         s_IRABAL = as.numeric(s_IRABAL),
         s_INVBAL = as.numeric(s_INVBAL),
         s_MMBAL = as.numeric(s_MMBAL),
         s_CCBAL = as.numeric(s_CCBAL),
         s_INCOME = as.numeric(s_INCOME),
         s_LORES = as.numeric(s_LORES),
         s_HMVAL = as.numeric(s_HMVAL),
         s_AGE = as.numeric(s_AGE),
         s_CRSCORE = as.numeric(s_CRSCORE),
         FLAG_NA_ACCTAGE = as.factor(FLAG_NA_ACCTAGE),
         FLAG_NA_PHONE= as.factor(FLAG_NA_PHONE),
         FLAG_NA_POS = as.factor(FLAG_NA_POS),
         FLAG_NA_POSAMT = as.factor(FLAG_NA_POSAMT),
         FLAG_NA_INVBAL = as.factor(FLAG_NA_INVBAL),
         FLAG_NA_CCBAL = as.factor(FLAG_NA_CCBAL),
         FLAG_NA_INCOME = as.factor(FLAG_NA_INCOME),
         FLAG_NA_LORES = as.factor(FLAG_NA_LORES),
         FLAG_NA_HMVAL = as.factor(FLAG_NA_HMVAL),
         FLAG_NA_AGE = as.factor(FLAG_NA_AGE)
  )

nn_pred <- Predictor$new(nn_bank,
                             data = train_sub[,-1],
                             y = train_sub$INS,
                             type = "response")

# LIME
point <- 732
lime.explain <- LocalModel$new(nn_pred,
                               x.interest = train_df[point,-1],
                               k = 5)
plot(lime.explain)

# Shapely
shap <- Shapley$new(nn_pred,
                    x.interest = train_df[point,-1])
shap$plot()