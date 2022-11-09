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
library(ROCR)

#### read in data #####
train <- read.csv("C:/Users/kat4538/Documents/MSA/FALL 3/machine learning/hw 1/insurance_t.csv")

# DETERMINE TYPE OF VARIABLES
# str(train)
# var <- sapply(train, n_distinct) 
# write.csv(var, "C:/Users/kat4538/Documents/MSA/FALL 3/machine learning/hw 1/var.csv")

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

##### MARS modeling #####
# Earth function
set.seed(444)
mars <- earth(INS ~ ., data = train, glm=list(family=binomial), 
              nfold = 10, pmethod=c("cv"))
summary(mars)

# variable importance metric
evimp(mars)

# ROC Curve for MARS
train_p <- train
train_p$p_hat <- predict(mars, type = "response")
p1 <- train_p$p_hat[train_p$Bonus == 1]
p0 <- train_p$p_hat[train_p$Bonus == 0] 

InformationValue::plotROC(train_p$INS, train_p$p_hat)

# KS stat
pred <- prediction(fitted(mars), factor(train$INS)) 
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
KS <- max(perf@y.values[[1]] - perf@x.values[[1]])
cutoffAtKS <- unlist(perf@alpha.values)[which.max(perf@y.values[[1]] - perf@x.values[[1]])]
print(c(KS, cutoffAtKS)) #KS Statistic

plot(x = unlist(perf@alpha.values), y = (1-unlist(perf@y.values)),
     type = "l", main = "K-S Plot (EDF)",
     xlab = 'Cut-off',
     ylab = "Proportion",
     col = "red")
lines(x = unlist(perf@alpha.values), y = (1-unlist(perf@x.values)), col = "blue")

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

# ROC curve for GAM
train_p2 <- train
train_p2$p_hat <- predict(gam, type = "response")
p1 <- train_p2$p_hat[train_p2$Bonus == 1]
p0 <- train_p2$p_hat[train_p2$Bonus == 0] 

InformationValue::plotROC(train_p2$INS, train_p2$p_hat)

# remaining variables after selection
gam2 <- mgcv::gam(INS ~ s(ACCTAGE)	+ factor(DDA)	+ s(DDABAL) +	s(DEP) + 
                   s(DEPAMT) +	s(CHECKS)	+ factor(DIRDEP) +	factor(NSF) +
                   s(PHONE) + s(TELLER)	+ factor(SAV) + 
                   s(SAVBAL) + 	factor(ATM) + s(ATMAMT)	+ 
                   factor(CD) + 	s(CDBAL) + 	factor(IRA) +
                   s(IRABAL) + 	factor(INV) + factor(MM) + 
                   s(MMBAL)	+ factor(MMCRED) + 	factor(CC) + 	s(CCBAL) +
                   factor(CCPURC) + factor(SDB) + s(CRSCORE) +	factor(INAREA) +
                   factor(BRANCH) + factor(FLAG_NA_ACCTAGE) + 	factor(FLAG_NA_PHONE) +
                   factor(FLAG_NA_POS) +	factor(FLAG_NA_POSAMT) + 	factor(FLAG_NA_INVBAL) +	factor(FLAG_NA_CCBAL) +
                   factor(FLAG_NA_INCOME)	+ factor(FLAG_NA_LORES)	+ factor(FLAG_NA_HMVAL)	+ factor(FLAG_NA_AGE) + factor(FLAG_NA_CRSCORE)			
                 , family = binomial, data = train)
summary(gam2)

# ROC curve for GAM w/ selected variables
train_p3 <- train
train_p3$p_hat <- predict(gam, type = "response")
p1 <- train_p3$p_hat[train_p3$Bonus == 1]
p0 <- train_p3$p_hat[train_p3$Bonus == 0] 

InformationValue::plotROC(train_p3$INS, train_p3$p_hat)
