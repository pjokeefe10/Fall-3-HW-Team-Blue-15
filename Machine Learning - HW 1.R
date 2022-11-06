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


# add flags back to original data frame

df <- cbind(df, flag_sub)

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

