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
library(mlbench)


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

##### HW 2 #####
# Random Forest model
rf.bank <- randomForest(INS ~ ., data = train, ntree = 500, importance = TRUE)

# Plot the change in error across different number of trees
plot(rf.bank, main = "Number of Trees Compared to MSE")

#Look at variable importance
varImpPlot(rf.bank,
           sort = TRUE,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf.bank)

# Tune an random forest mtry value
set.seed(12345)
tuneRF(x = train[,-1], y = train[,1], 
       plot = TRUE, ntreeTry = 500, stepFactor = 0.5)

set.seed(12345)
rf.bank <- randomForest(INS ~ ., data = train, ntree = 500, mtry = 4, importance = TRUE)

varImpPlot(rf.bank,
           sort = TRUE,
           n.var = 14,
           main = "Order of Variables")
importance(rf.bank, type = 1)

# Interpret some of the variables using partial dependence plots
# NEED TO UPDATE
#partialPlot(rf.bank, train, AGE)
#partialPlot(rf.bank, train, LORES)
#partialPlot(rf.bank, train, INCOME)
#partialPlot(rf.bank, train, HMVAL)

# Include a random variable to determine variable selection
train$random <- rnorm(2051)

set.seed(12345)
rf.bank <- randomForest(INS ~ ., data = train, ntree = 500, mtry = 4, importance = TRUE)

varImpPlot(rf.bank,
           sort = TRUE,
           n.var = 15,
           main = "Look for Variables Below Random Variable")
importance(rf.bank)

#ROC curve
# pred.rf2 <- prediction(fitted(rf.bank), factor(rf.bank.p$INS)) #need to check
# perf.rf2 <- performance(pred.rf.bank, measure = "tpr", x.measure = "fpr")
# plot(perf.rf2, lwd = 3, col = "dodgerblue3", main = paste0("Random Forest ROC Plot (AUC = ", round(AUROC(gam2.p$INS, gam2.p$p_hat), 3),")"), 
#      xlab = "True Positive",
#      ylab = "False Positive")
# abline(a = 0, b = 1, lty = 3)


# XGBoost model

# Prepare data for XGBoost function - similar to what we did for glmnet
train_x <- model.matrix(INS ~ ., data = train)[, -1]
train_y <- train$INS

# Build XGBoost model
set.seed(12345)
xgb.bank <- xgboost(data = train_x, label = train_y, subsample = 0.5, nrounds = 100)

# Tuning an XGBoost nrounds parameter - 24 was lowest!
xgbcv.bank <- xgb.cv(data = train_x, label = train_y, subsample = 0.5, nrounds = 100, nfold = 10)

# Tuning through caret
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
xgb.bank.caret <- train(x = train_x, y = train_y,
                        method = "xgbTree",
                        tuneGrid = tune_grid,
                        trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                 number = 10))

plot(xgb.bank.caret)

# Variable importance
xgb.bank <- xgboost(data = train_x, label = train_y, subsample = 1, nrounds = 24, eta = 0.25, max_depth = 5)

xgb.importance(feature_names = colnames(train_x), model = xgb.bank)

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb.bank))

# Include a random variable to determine variable selection
training$random <- rnorm(2051)

train_x <- model.matrix(INS ~ ., data = train)[, -1]
train_y <- train$INS

set.seed(12345)
xgb.bank <- xgboost(data = train_x, label = train_y, subsample = 1, nrounds = 24, eta = 0.25, max_depth = 5, objective = "reg:linear")

xgb.importance(feature_names = colnames(train_x), model = xgb.bank)

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb.bank))

# Interpret some of the variables using partial dependence plots
xgb.bank <- xgboost(data = train_x, label = train_y, subsample = 1, nrounds = 24, eta = 0.25, max_depth = 5, objective = "reg:linear")
# 
# partial(xgb.bank, pred.var = "Year_Built", 
#         plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "lattice", 
#         train = train_x, pdp.color = "red")
# partial(xgb.bank, pred.var = "Garage_Area", 
#         plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "lattice", 
#         train = train_x)

#ROC curve
# pred.boost <- prediction(fitted(xgb.bank), factor(xgb.bank.p$INS)) 
# perf.boost <- performance(pred.boost, measure = "tpr", x.measure = "fpr")
# plot(perf.gam2, lwd = 3, col = "dodgerblue3", main = paste0("XGBoost ROC Plot (AUC = ", round(AUROC(gam2.p$INS, gam2.p$p_hat), 3),")"), 
#      xlab = "True Positive",
#      ylab = "False Positive")
# abline(a = 0, b = 1, lty = 3)


##### HW 2 END #####


##### HW 1 #####
##### MARS modeling #####
# Earth function
set.seed(444)
mars <- earth(INS ~ ., data = train, glm=list(family=binomial), 
              nfold = 10, pmethod=c("cv"), trace = 0.5)
summary(mars)

# Variable importance metric
evimp(mars)

##Accuracy metrics
train_p <- train

#Concordance and discordance
Concordance(train_p$INS, predict(mars, type = "response"))

somersD(train_p$INS, predict(mars, type = "response"))

train_p$p_hat <- predict(mars, type = "response")[,1]

p1 <- train_p$p_hat[train_p$INS == 1]
p0 <- train_p$p_hat[train_p$INS == 0]

coef_discrim <- mean(p1) - mean(p0)

ggplot(train_p, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  labs(x = "Predicted Probability",
       y = "Density",
       fill = "Outcome",
       title = "Discrimination Slope for MARS Model",
       subtitle = paste("Coefficient of Discrimination = ",
                        round(coef_discrim, 3), sep = "")) +
  scale_fill_manual(values = c("#1C86EE", "#FFB52E"),name = "Customer Decision", labels = c("Not Bought", "Bought")) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5) )

#ROC curve
pred.mars <- prediction(fitted(mars), factor(train_p$INS)) 
perf.mars <- performance(pred.mars, measure = "tpr", x.measure = "fpr")
plot(perf.mars, lwd = 3, col = "dodgerblue3", main = paste0("MARS ROC Plot (AUC = ", round(AUROC(train_p$INS, train_p$p_hat), 3),")"), 
     xlab = "True Positive",
     ylab = "False Positive")
abline(a = 0, b = 1, lty = 3)


# KS stat
KS <- max(perf.mars@y.values[[1]] - perf.mars@x.values[[1]])
cutoffAtKS <- unlist(perf.mars@alpha.values)[which.max(perf.mars@y.values[[1]] - perf.mars@x.values[[1]])]
print(c(KS, cutoffAtKS)) #KS Statistic

plot(x = unlist(perf.mars@alpha.values), y = (1-unlist(perf.mars@y.values)),
     type = "l", main = "MARS K-S Plot (EDF)",
     xlab = 'Cut-off',
     ylab = "Proportion",
     col = "red")
lines(x = unlist(perf.mars@alpha.values), y = (1-unlist(perf.mars@x.values)), col = "blue")


train_p$prediction <- factor(ifelse(train_p$p_hat >= 0.3236774, 1, 0))

confusionMatrix(train_p$prediction, train_p$INS)



##### GAM modeling #####
# all variables
gam <- mgcv::gam(INS ~ s(ACCTAGE)	+ factor(DDA)	+ s(DDABAL) +	s(DEP) + 
                   s(DEPAMT) +	s(CHECKS)	+ factor(DIRDEP) +	factor(NSF) +
                   s(NSFAMT) +	s(PHONE) + s(TELLER)	+ factor(SAV) + 
                   s(SAVBAL) + 	factor(ATM) + s(ATMAMT)	+ s(POS) +
                   s(POSAMT) + 	factor(CD) + 	s(CDBAL) + 	factor(IRA) +
                   s(IRABAL) + 	factor(INV)	+ s(INVBAL) + factor(MM) + 
                   s(MMBAL)	+ factor(MMCRED) + 	factor(CC) + 	s(CCBAL) +
                   factor(CCPURC) + factor(SDB) + s(INCOME) + s(LORES) +
                   s(HMVAL) +	s(AGE) + s(CRSCORE) +	factor(INAREA) +
                   factor(BRANCH) + factor(FLAG_NA_ACCTAGE) + 	factor(FLAG_NA_PHONE) +
                   factor(FLAG_NA_POS) +	factor(FLAG_NA_POSAMT) + 	factor(FLAG_NA_INVBAL) +	factor(FLAG_NA_CCBAL) +
                   factor(FLAG_NA_INCOME)	+ factor(FLAG_NA_LORES)	+ factor(FLAG_NA_HMVAL)	+ factor(FLAG_NA_AGE) + factor(FLAG_NA_CRSCORE)			
                 , family = binomial, select = TRUE, data = train)
summary(gam)

# remaining variables after selection
gam2 <- mgcv::gam(INS ~ s(ACCTAGE)	+ factor(DDA)	+ s(DDABAL) +	s(DEP) + 
                    s(DEPAMT) +	s(CHECKS)	+ factor(DIRDEP) +	factor(NSF) + s(PHONE) + s(TELLER)	+ factor(SAV) + 
                    s(SAVBAL) + 	factor(ATM) + s(ATMAMT)	+ factor(CD) + 	s(CDBAL) + 	factor(IRA) +
                    s(IRABAL) + 	factor(INV) + factor(MM) + 
                    s(MMBAL)	+ factor(MMCRED) + 	factor(CC) + 	s(CCBAL) +
                    factor(CCPURC) + factor(SDB) + s(CRSCORE) +	factor(INAREA) +
                    factor(BRANCH) + factor(FLAG_NA_ACCTAGE) + 	factor(FLAG_NA_PHONE) +
                    factor(FLAG_NA_POS) +	factor(FLAG_NA_POSAMT) + 	factor(FLAG_NA_INVBAL) +	factor(FLAG_NA_CCBAL) +
                    factor(FLAG_NA_INCOME)	+ factor(FLAG_NA_LORES)	+ factor(FLAG_NA_HMVAL)	+ factor(FLAG_NA_AGE) + factor(FLAG_NA_CRSCORE)			
                  , family = binomial, select = TRUE, data = train)
summary(gam2)



#Accuracy metrics
gam2.p <- train

#Concordance and discordance
Concordance(gam2.p$INS, predict(gam2, type = "response"))

somersD(gam2.p$INS, predict(gam2, type = "response"))

gam2.p$p_hat <- predict(gam2, type = "response")

p1 <- gam2.p$p_hat[gam2.p$INS == 1]
p0 <- gam2.p$p_hat[gam2.p$INS == 0]

coef_discrim <- mean(p1) - mean(p0)

ggplot(gam2.p, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  labs(x = "Predicted Probability",
       y = "Density",
       fill = "Outcome",
       title = "Discrimination Slope for GAM", 
       subtitle = paste("Coefficient of Discrimination = ", round(coef_discrim, 3), sep = "")) +
  scale_fill_manual(values = c("#1C86EE", "#FFB52E"),name = "Customer Decision", labels = c("Not Bought", "Bought")) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5) )

#ROC curve
pred.gam2 <- prediction(fitted(gam2), factor(gam2.p$INS)) 
perf.gam2 <- performance(pred.gam2, measure = "tpr", x.measure = "fpr")
plot(perf.gam2, lwd = 3, col = "dodgerblue3", main = paste0("ROC Plot (AUC = ", round(AUROC(gam2.p$INS, gam2.p$p_hat), 3),")"), 
     xlab = "True Positive",
     ylab = "False Positive")
abline(a = 0, b = 1, lty = 3)


# KS stat
KS <- max(perf.gam2@y.values[[1]] - perf.gam2@x.values[[1]])
cutoffAtKS <- unlist(perf.gam2@alpha.values)[which.max(perf.gam2@y.values[[1]] - perf.gam2@x.values[[1]])]
print(c(KS, cutoffAtKS)) #KS Statistic

plot(x = unlist(perf.gam2@alpha.values), y = (1-unlist(perf.gam2@y.values)),
     type = "l", main = "GAM K-S Plot (EDF)",
     xlab = 'Cut-off',
     ylab = "Proportion",
     col = "red")
lines(x = unlist(perf.gam2@alpha.values), y = (1-unlist(perf.gam2@x.values)), col = "blue")

## K-S Statistic of 0.3236774


gam2.p$prediction <- factor(ifelse(gam2.p$p_hat >= 0.3236774, 1, 0))

confusionMatrix(gam2.p$prediction, gam2.p$INS)
