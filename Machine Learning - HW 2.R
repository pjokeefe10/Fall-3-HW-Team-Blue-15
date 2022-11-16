# libraries
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
library(pROC)

# read in data
train <- read.csv("https://github.com/pjokeefe10/Fall-3-HW-Team-Blue-15/raw/main/insurance_t.csv")

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

############################## random forest #########################################################

set.seed(444)

rf <- randomForest(factor(INS) ~ ., data = train, ntree = 200, importance = TRUE, mtry = 6)
plot(rf, main = "Number of Trees Compared to MSE")

varImpPlot(rf, sort = TRUE, main = "Variable Importance")


# tuning

set.seed(444)
tuneRF(x = train[,-37], y = train[,37],
       plot = TRUE, ntreeTry = 500, stepFactor = 0.25)

# variable selection
train2 <- train
train2$random <- rnorm(nrow(train))
set.seed(444)

rf2 <- randomForest(factor(INS) ~ ., data = train2, ntree = 200, importance = TRUE, mtry = 6)
varImpPlot(rf2,
           sort = TRUE,
           n.var = 15,
           main = "Look for Variables Below Random Variable")

# if we use accuracy, then no need to take out any vars
# if we use gini, then left with SAVBAL, BRANCH, and DDABAL

#set.seed(444)
#rf3 <- randomForest(factor(INS) ~ SAVBAL + BRANCH + DDABAL, data = train, ntree = 200, importance = TRUE, mtry = 3)

# original rf still best

############################# XGBoost Model ######################################################

# separate training into x and y 

train_x <- model.matrix(INS ~ ., data = train)[, c( -1)] 
train_y <- as.numeric(train$INS) - 1 # this gets us 0's and 1's. this took way too long to figure out. 


################## need to find optimal nrounds and change that to grid
set.seed(444)
xgb <- xgb.cv(data = train_x, label = train_y, subsample = 0.5, nrounds = 100, eval_metric = "auc", objective = "binary:logistic", nfold = 10)

# nround = 9 at subsample = .5

# variable selection w/ tuning
#train$random <- rnorm(nrow(train))
train_x <- model.matrix(INS ~ ., data = train)[, c( -1)]
train_y <- as.numeric(train$INS) - 1 

tune_grid <- expand.grid(
  nrounds = 9,
  eta = c(0, .25, .5, .75, 1),
  max_depth = c(3:14),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(.8,.85, .9, .95, 1)
)

set.seed(444)

xgb.caret <- train(x = train_x, y = train_y,
                        method = "xgbTree",
                        tuneGrid = tune_grid,
                        trControl = trainControl(method = 'cv', number = 10), eval_metric = "auc", objective = "binary:logistic")

plot(xgb.caret)
# "best" xgb

set.seed(444)
xgb2 <- xgboost(data = train_x, label = train_y, subsample = .9, nrounds = 9, eval_metric = "auc", objective = "binary:logistic",
            max_depth = 4, eta = .5)

xgb.importance(feature_names = colnames(train_x), model = xgb2)

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb2))



# just use SAVBAL and DDABAL? may not do var selection

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb2))

################################## ROC Curves ############################################################
######################## Random Forest
##Accuracy metrics
train_p <- train

#Concordance and discordance

train_p$p_hat <- predict(rf, type = "prob")[,2]

p1 <- train_p$p_hat[train_p$INS == 1]
p0 <- train_p$p_hat[train_p$INS == 0]

coef_discrim <- mean(p1) - mean(p0)

ggplot(train_p, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  labs(x = "Predicted Probability",
       y = "Density",
       fill = "Outcome",
       title = "Discrimination Slope for Random Forest",
       subtitle = paste("Coefficient of Discrimination = ",
                        round(coef_discrim, 3), sep = "")) +
  scale_fill_manual(values = c("#1C86EE", "#FFB52E"),name = "Customer Decision", labels = c("Not Bought", "Bought")) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5) )

#ROC curve
pred.rf <- prediction(train_p$p_hat, factor(train_p$INS)) 
perf.rf <- performance(pred.rf, measure = "tpr", x.measure = "fpr")
plot(perf.rf, lwd = 3, col = "dodgerblue3", main = paste0("Random Forest ROC Plot (AUC = ", round(AUROC(train_p$INS, train_p$p_hat), 3),")"), 
     xlab = "False Positive",
     ylab = "True Positive")
abline(a = 0, b = 1, lty = 3)


####################### XGBoost ###############################################################################################
y_pred <- predict(xgb2, train_x)

#ROC curve
pred.xgb <- prediction(y_pred, factor(train_p$INS)) 
perf.xgb <- performance(pred.xgb, measure = "tpr", x.measure = "fpr")
plot(perf.xgb, lwd = 3, col = "dodgerblue3", main = paste0("XGBoost ROC Plot (AUC = ", round(AUROC(train_p$INS, y_pred), 3),")"), 
     xlab = "False Positive",
     ylab = "True Positive")
abline(a = 0, b = 1, lty = 3)

# we can get a really high AUC by increasing the max_depth parameter
