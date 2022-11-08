# Needed Libraries for Analysis 
library(tidyverse)
library(caret)
library(leaps)
library(glmnet)
library(dplyr)
library(earth)
library(InformationValue)

df <-read.csv("C:\\Users\\Justin\\OneDrive - North Carolina State University\\Documents\\NC State\\IAA R\\Data\\insurance_t.csv")

# types of data
str(df)


# find number of NA ()
colSums(is.na(df))

# coerce all binary and categorical to factor (16 cols)

cols <- c('DDA','DIRDEP','NSF','SAV','ATM','CD','IRA','INV','MM','MMCRED','CC','CCPURC','SDB','INAREA','INS', 'BRANCH')


df[cols] <- lapply(df[cols], factor)  

# create binary flag col for all variables
flag  = df %>%
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

df <- cbind(df, flag_sub_sub)

###impute data ####################################################################################################################

# continuous imputation (median)

df <- df %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

# verify no left over NA's for continuous vars
colSums(is.na(df))

# categorical imputation (NA category)

df$INV <- addNA(df$INV)
df$CC <- addNA(df$CC)
df$CCPURC <- addNA(df$CCPURC)

# verify no left over NA's categorical vars
colSums(is.na(df))

# note: no need for variable selection

# MARS
mars <- earth(INS ~ ., data = df, glm = list(family=binomial))
summary(mars)

evimp(mars)

# ROC Curve MARS
df$p_hat <- predict(mars, type = "response")
p1 <- df$p_hat[df$Bonus == 1]
p0 <- df$p_hat[df$Bonus == 0] 


InformationValue::plotROC(df$INS, df$p_hat)

# GAM
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
                 , family = binomial, data = df)

summary(gam)
