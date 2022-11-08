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
train <- read.csv("https://github.com/pjokeefe10/Fall-3-HW-Team-Blue-15/raw/main/insurance_t.csv")

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

train_p$p_hat <- predict(mars, type = "response")

p1 <- train_p$p_hat[train_p$INS == 1]
p0 <- train_p$p_hat[train_p$INS == 0]

coef_discrim <- mean(p1) - mean(p0)

ggplot(train_p, aes(p_hat, fill = INS)) +
  geom_density(alpha = 0.7) +
  labs(x = "Predicted Probability",
       y = "Density",
       fill = "Outcome",
       title = "Discrimination Slope") +
  scale_fill_manual(values = c("#1C86EE", "#FFB52E"),name = "Customer Decision", labels = c("Not Bought", "Bought")) +
  theme(plot.title = element_text(hjust = 0.5))

#ROC curve
pred.mars <- prediction(fitted(mars), factor(train_p$INS)) 
perf.mars <- performance(pred.mars, measure = "tpr", x.measure = "fpr")
plot(perf.mars, lwd = 3, col = "dodgerblue3", main = paste0("ROC Plot (AUC = ", round(AUROC(train_p$INS, train_p$p_hat), 3),")"), 
     xlab = "True Positive",
     ylab = "False Positive")
abline(a = 0, b = 1, lty = 3)


# KS stat
KS <- max(perf.mars@y.values[[1]] - perf.mars@x.values[[1]])
cutoffAtKS <- unlist(perf.mars@alpha.values)[which.max(perf.mars@y.values[[1]] - perf.mars@x.values[[1]])]
print(c(KS, cutoffAtKS)) #KS Statistic

plot(x = unlist(perf.mars@alpha.values), y = (1-unlist(perf.mars@y.values)),
     type = "l", main = "K-S Plot (EDF)",
     xlab = 'Cut-off',
     ylab = "Proportion",
     col = "red")
lines(x = unlist(perf.mars@alpha.values), y = (1-unlist(perf.mars@x.values)), col = "blue")


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

ggplot(gam2.p, aes(p_hat, fill = INS)) +
  geom_density(alpha = 0.7) +
  labs(x = "Predicted Probability",
       y = "Density",
       fill = "Outcome",
       title = "Discrimination Slope") +
  scale_fill_manual(values = c("#1C86EE", "#FFB52E"),name = "Customer Decision", labels = c("Not Bought", "Bought")) +
  theme(plot.title = element_text(hjust = 0.5))

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
     type = "l", main = "K-S Plot (EDF)",
     xlab = 'Cut-off',
     ylab = "Proportion",
     col = "red")
lines(x = unlist(perf.gam2@alpha.values), y = (1-unlist(perf.gam2@x.values)), col = "blue")

## K-S Statistic of 0.3037318
