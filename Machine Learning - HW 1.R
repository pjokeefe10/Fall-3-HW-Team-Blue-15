# Needed Libraries for Analysis 
library(tidyverse)
library(caret)
library(leaps)
library(glmnet)
library(dplyr)

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

# GAM
