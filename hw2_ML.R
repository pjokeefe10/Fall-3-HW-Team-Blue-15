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
library(mlbench)
library(xgboost)
library(Ckmeans.1d.dp)
library(pdp)
library(pROC)

#### read in data #####
train <- read.csv("C:/Users/kat4538/Documents/MSA/FALL 3/machine learning/hw 2/insurance_t.csv")

# DETERMINE TYPE OF VARIABLES
str(train)
# var <- sapply(train, n_distinct) 

##### check which variables have missing #####
gg_miss_var(train) # 14 variables
na_count <-sapply(train, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

##### missing value imputation #####
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

##### RANDOM FOREST #####
train$INS <- as.factor(train$INS)
set.seed(444)
rf <- randomForest(INS ~ ., data = train, 
                        ntree = 500, importance = TRUE)

# Plot the change in error across different number of trees
plot(rf, main = "Number of Trees Compared to MSE")

#Look at variable importance
varImpPlot(rf,
           sort = TRUE,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)

# Tune an random forest mtry value
set.seed(444)
tuneRF(x = train[,-1], y = train[,1], 
       plot = TRUE, ntreeTry = 500, stepFactor = 0.5)

set.seed(444)
rf <- randomForest(INS ~ ., data = train, ntree = 200,
                         mtry = 8, importance = TRUE)

varImpPlot(rf,
           sort = TRUE,
           n.var = 14,
           main = "Order of Variables")
importance(rf, type = 1)

# Include a random variable to determine variable selection
train$random <- rnorm(8495)

set.seed(444)
rf <- randomForest(INS ~ ., data = train, 
                   ntree = 200, mtry = 8, importance = TRUE)

varImpPlot(rf,
           sort = TRUE,
           n.var = 20,
           main = "Look for Variables Below Random Variable")
rfimp <- importance(rf)
rfimp <- as.data.frame(rfimp)
write.csv(rfimp, "C:/Users/kat4538/Documents/MSA/FALL 3/machine learning/hw 2/rfimp.csv")

# variable selection w/ accuracy
set.seed(444)
rf <- randomForest(INS ~ . - LORES - FLAG_NA_CRSCORE - FLAG_NA_INCOME
                   - FLAG_NA_ACCTAGE - SDB - NSFAMT - INAREA, 
                   data = train, ntree = 200, mtry = 8, importance = TRUE)

# rf <- randomForest(INS ~ . - FLAG_NA_HMVAL - LORES - NSF - FLAG_NA_CRSCORE - 
#                      FLAG_NA_ACCTAGE - SDB - NSFAMT - FLAG_NA_LORES - INAREA, 
#                    data = train, ntree = 200, mtry = 8, importance = TRUE)

varImpPlot(rf,
           sort = TRUE,
           n.var = 10)
rfimp2 <- importance(rf)
rfimp2 <- as.data.frame(rfimp2)
write.csv(rfimp2, "C:/Users/kat4538/Documents/MSA/FALL 3/machine learning/hw 2/rfimp2.csv")

# rf <- randomForest(INS ~ . - LORES - FLAG_NA_CRSCORE - FLAG_NA_INCOME
#                    - FLAG_NA_ACCTAGE - SDB - NSFAMT - INAREA - DIRDEP
#                    - FLAG_NA_LORES - CRSCORE - FLAG_NA_HMVAL - NSF, 
#                    data = train, ntree = 200, mtry = 8, importance = TRUE)

# variable selection w/ gini
set.seed(444)
rf <- randomForest(INS ~ SAVBAL + DDABAL + BRANCH, data = train, 
                   ntree = 200, importance = TRUE)

varImpPlot(rf,
           sort = TRUE,
           n.var = 3)
importance(rf)

# ROC Curve
train_p <- train
train_p$p_hat <- predict(rf, type = "prob")[,2]

p1 <- train_p$p_hat[train_p$INS == 1]
p0 <- train_p$p_hat[train_p$INS == 0]

#ROC curve
pred.rf <- prediction(train_p$p_hat, factor(train_p$INS)) 
perf.rf <- performance(pred.rf, measure = "tpr", x.measure = "fpr")
plot(perf.rf, lwd = 3, col = "dodgerblue3", main = paste0("Random Forest ROC Plot (AUC = ", round(AUROC(train_p$INS, train_p$p_hat), 3),")"), 
     xlab = "False Positive",
     ylab = "True Positive")
abline(a = 0, b = 1, lty = 3)

# coefficient of discrimination
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

##### XGBOOST #####
# Prepare data for XGBoost function - similar to what we did for glmnet
train_x <- model.matrix(INS ~ ., data = train)[, -1]
train_y <- as.numeric(train$INS)-1

# Build XGBoost model
set.seed(444)
xgb <- xgboost(data = train_x, label = train_y, 
               subsample = 0.5, nrounds = 100, 
               objective = "binary:logistic", 
               eval_metric = "auc")

# Tuning an XGBoost nrounds parameter - 11 iterations had the greatest auroc!
xgbcv <- xgb.cv(data = train_x, label = train_y, 
                subsample = 0.5, nrounds = 100, 
                nfold = 10, objective = "binary:logistic", 
                eval_metric = "auc")
tune <- xgbcv$evaluation_log

# Tuning through caret
tune_grid <- expand.grid(
  nrounds = 11,
  eta = c(0.1, 0.15, 0.2, 0.25, 0.3),
  max_depth = c(1:10),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.25, 0.5, 0.75, 1)
)

set.seed(444)
xgb.caret <- train(x = train_x, y = train_y,
                        method = "xgbTree", eval_metric = "auc",
                        tuneGrid = tune_grid, objective = "binary:logistic",
                        trControl = trainControl(method = 'cv', number = 10))

plot(xgb.caret)

# Variable importance
xgb <- xgboost(data = train_x, label = train_y, 
               subsample = 1, nrounds = 11, eta = 0.50, 
               max_depth = 4, objective = "binary:logistic", 
               eval_metric = "auc")

xgb.importance(feature_names = colnames(train_x), model = xgb)

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb))

# Include a random variable to determine variable selection
train$random <- rnorm(8495)

set.seed(444)
xgb <- xgboost(data = train_x, label = train_y, 
               subsample = 1, nrounds = 11, eta = 0.50, 
               max_depth = 4, objective = "binary:logistic", 
               eval_metric = "auc")

xgb.importance(feature_names = colnames(train_x), model = xgb)

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb))

# variable selection
train_x <- model.matrix(INS ~ SAVBAL + DDABAL + CDBAL + 
                          DDA + MM, data = train)[, -1]
train_y <- as.numeric(train$INS)-1

set.seed(444)
xgb <- xgboost(data = train_x, label = train_y, 
               subsample = 1, nrounds = 11, eta = 0.50, 
               max_depth = 4, objective = "binary:logistic", 
               eval_metric = "auc")

xgb.importance(feature_names = colnames(train_x), model = xgb)
xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb))

# ROC curve
y_pred <- predict(xgb, train_x)

#ROC curve
pred.xgb <- prediction(y_pred, factor(train_p$INS)) 
perf.xgb <- performance(pred.xgb, measure = "tpr", x.measure = "fpr")
plot(perf.xgb, lwd = 3, col = "dodgerblue3", main = paste0("XGBoost ROC Plot (AUC = ", round(AUROC(train_p$INS, y_pred), 3),")"), 
     xlab = "False Positive",
     ylab = "True Positive")
abline(a = 0, b = 1, lty = 3)
