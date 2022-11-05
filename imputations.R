library(tidyverse)
library(caret)
library(earth)
library(skimr)

set.seed(25335)

train_1 <- read_csv("insurance_t.csv")

x <- createDataPartition(train_1$INS, p = 0.7, list = TRUE)$Resample1
train <- train_1[x, ]
test <- train_1[-x, ]


colSums(is.na(train))



train %>% 
  count(INV)
median(train$INVBAL, na.rm = TRUE)

train$INV[is.na(train$INV)] <-  0

train$INVBAL[is.na(train$INVBAL)] = 0


train %>% 
  count(POS)
median(train$POSAMT, na.rm = TRUE)

train$POS[is.na(train$POS)] <-  0

train$POSAMT[is.na(train$POSAMT)] = 0

train %>% 
  count(CC)



train$CC[is.na(train$CC)] <- 0

median(train$CCBAL, na.rm = TRUE)

train$CCBAL[is.na(train$CCBAL)] <- 0


train %>% 
  count(CCPURC)

train$CCPURC[is.na(train$CCPURC)] <- 0


train %>% count(PHONE)

train$PHONE[is.na(train$PHONE)] = 0

median(train$ACCTAGE, na.rm = TRUE)

train$ACCTAGE[is.na(train$ACCTAGE)]= 4

train %>% count(INCOME)

median(train$INCOME, na.rm = TRUE)

train$INCOME[is.na(train$INCOME)] = 35

train %>% count(LORES)

median(train$LORES, na.rm = TRUE)

train$LORES[is.na(train$LORES)] = 6.5

train %>% count(HMVAL)

median(train$HMVAL, na.rm = TRUE)

train$HMVAL[is.na(train$HMVAL)] <- 107

median(train$AGE, na.rm = TRUE)

train$AGE[is.na(train$AGE)] <- 48

train %>% count(CRSCORE)

median(train$CRSCORE, na.rm = TRUE)

train$CRSCORE[is.na(train$CRSCORE)] <- 666

write_csv(train, "train_imputed.csv")
write_csv(test, "test_nonimputed.csv")

