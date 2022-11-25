############ HW 3 MACHINE LEARNING ###############
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
library(nnet)
library(NeuralNetTools)
library(reshape2)
library(e1071)
library(pdp)
library(ALEPlot)
library(lime)
library(iml)

#### read in data #####
train <- read.csv("C:/Users/kat4538/Documents/MSA/FALL 3/machine learning/hw 3/insurance_t.csv")
valid <- read.csv("C:/Users/kat4538/Documents/MSA/FALL 3/machine learning/hw 3/insurance_v.csv")

# DETERMINE TYPE OF VARIABLES
str(train)
# var <- sapply(train, n_distinct) 

##### check which variables have missing #####
gg_miss_var(train) # 14 variables
na_count <-sapply(train, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

##### missing value imputation for TRAINING DATA #####
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

##### NNET #####
# standardizing continuous variables
train2 <- train %>% mutate(across(where(is.numeric), scale))
valid2 <- valid %>% mutate(across(where(is.numeric), scale))

# build model
train2$INS <- as.factor(train2$INS)
valid2$INS <- as.factor(valid2$INS)

set.seed(444)
nn <- nnet(INS ~ ., data = train2, size = 5)

# plot network
plotnet(nn)

# tuning parameters
tune_grid <- expand.grid(
  .size = c(3, 4, 5, 6, 7),
  .decay = c(0, 0.5, 1)
)
set.seed(444)
nn.caret <- train(INS~., data = train2,
                  method = "nnet", # Neural network using the nnet package
                  tuneGrid = tune_grid,
                  trControl = trainControl(method = 'cv', number = 10),
                  trace = FALSE)

nn.caret$bestTune

# final nnet
set.seed(444)
nn <- nnet(INS ~ ., data = train2,
            size = 3, decay = 1)

plotnet(nn)

# Hinton Diagram
nn_weights <- matrix(data = nn$wts[1:217], ncol = 3, nrow = 72)
rownames(nn_weights) <- c("bias", nn$coefnames)
colnames(nn_weights) <- c("h1", "h2", "h3")

ggplot(melt(nn_weights), aes(x=Var1, y=Var2, size=abs(value), 
                             color=as.factor(sign(value)))) +
  geom_point(shape = 15) +
  scale_size_area(max_size = 40) +
  labs(x = "", y = "", title = "Hinton Diagram of NN Weights") +
  theme_bw()

##### NAIVE BAYES #####
set.seed(444)
train$INS <- as.factor(train$INS)
nb <- naiveBayes(INS ~ ., data = train, 
                 laplace = 0.1, usekernel = TRUE)


# Optimize laplace and kernel - CARET ONLY ABLE TO TUNE CLASSIFICATION PROBLEMS FOR NAIVE BAYES
# tune_grid <- expand.grid(
#   usekernel = c(TRUE),
#   fL = c(0, 0.5, 1),
#   adjust=c(0,0.5,1.0))
# 
# set.seed(444)
# nb.caret <- train(x, y, data = train,
#                        method = "nb", 
#                        tuneGrid = tune_grid,
#                        trControl = trainControl(method = 'cv', number = 10))
# nb.caret$bestTune
# 
# nb <- naiveBayes(INS ~ ., data = train, 
#                  fL = 0.5, usekernel = TRUE, adjust = 0)

##### missing value imputation for VALIDATION DATA #####
valid$INS <- as.factor(valid$INS)

#convert all categorical var to factors
col_names <- names(valid)[c(2,7:8,12,14,18,20,22,24,26:27,29:30,36,38)]
valid[,col_names] <- lapply(valid[,col_names] , factor)

# create binary flag col for all variables
flag  = valid %>%
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

valid <- cbind(valid, flag_sub_sub)

# mode for categorical var
calc_mode <- function(x){
  
  # list the distinct / unique values
  distinct_values <- unique(x)
  
  # count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

valid$INV <- if_else(is.na(valid$INV), calc_mode(valid$INV), valid$INV)
valid$CC <- if_else(is.na(valid$CC), calc_mode(valid$CC), valid$CC)
valid$CCPURC <- if_else(is.na(valid$CCPURC), calc_mode(valid$CCPURC), valid$CCPURC)

# median for continuous var
valid <- valid %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

##### check each variable for separation problems #####
# create list of categorical variable names
all <- names(valid)[c(2,7:8,12,14,18,20,22,24,26:27,29:30,36,38)]

# print table for each column against INS
for(i in all){
  table_temp <- valid %>% 
    dplyr::select(INS, i) %>% 
    table()
  
  print(table_temp) #MMCRED has quasi-separation
}

# roll up categories
valid$MMCRED <- as.character(valid$MMCRED)
valid$MMCRED[which(valid$MMCRED > 2)] <- "3+" # new category for 3+ money market credits
table(valid$INS, valid$MMCRED)

#### ROC CURVE ON VALIDATION DATA ####
# NNET
valid_p <- valid2
valid_p$p_hat <- predict(nn, newdata=valid_p, type = "raw")[,1]
p1nn <- valid_p$p_hat[valid_p$INS == 1]
p0nn <- valid_p$p_hat[valid_p$INS == 0]

#ROC curve
pred.nn <- prediction(valid_p$p_hat, factor(valid_p$INS)) 
perf.nn <- performance(pred.nn, measure = "tpr", x.measure = "fpr")
plot(perf.nn, lwd = 3, col = "dodgerblue3", main = paste0("Neural Net ROC Plot (AUC = ", round(AUROC(valid_p$INS, valid_p$p_hat), 3),")"), 
     xlab = "False Positive",
     ylab = "True Positive")
abline(a = 0, b = 1, lty = 3)

# coefficient of discrimination
coef_discrim <- mean(p1nn) - mean(p1nn)
ggplot(valid_p, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  labs(x = "Predicted Probability",
       y = "Density",
       fill = "Outcome",
       title = "Discrimination Slope for Naive Bayes",
       subtitle = paste("Coefficient of Discrimination = ",
                        round(coef_discrim, 3), sep = "")) +
  scale_fill_manual(values = c("#1C86EE", "#FFB52E"),name = "Customer Decision", labels = c("Not Bought", "Bought")) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5) )


# NAIVE BAYES
val_p <- valid
val_p$p_hat <- predict(nb, type = "raw", newdata = val_p)[,1]
p1nb <- val_p$p_hat[val_p$INS == 1]
p0nb <- val_p$p_hat[val_p$INS == 0]

pred.nn <- prediction(val_p$p_hat, factor(val_p$INS)) 
perf.nn <- performance(pred.nn, measure = "tpr", x.measure = "fpr")
plot(perf.nn, lwd = 3, col = "dodgerblue3", main = paste0("Naive Bayes ROC Plot (AUC = ", round(AUROC(val_p$INS, val_p$p_hat), 3),")"), 
     xlab = "False Positive",
     ylab = "True Positive")
abline(a = 0, b = 1, lty = 3)


# coefficient of discrimination
coef_discrim <- mean(p1nb) - mean(p1nb)
ggplot(val_p, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  labs(x = "Predicted Probability",
       y = "Density",
       fill = "Outcome",
       title = "Discrimination Slope for Naive Bayes",
       subtitle = paste("Coefficient of Discrimination = ",
                        round(coef_discrim, 3), sep = "")) +
  scale_fill_manual(values = c("#1C86EE", "#FFB52E"),name = "Customer Decision", labels = c("Not Bought", "Bought")) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5) )
