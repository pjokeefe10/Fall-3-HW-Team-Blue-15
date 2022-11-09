library(tidyverse)
library(caret)
library(leaps)
library(glmnet)
library(earth)
library(mgcv)
library(ROCR)
library(InformationValue)

#Load in the train and test created in imputations.R
train <- read_csv("train_imputed.csv")

#Check for missing values
is.na(train) %>% colSums()

#Subset data for variables chosen from the logistic regression hw
train <- train %>% 
  select(DDA, NSF, IRA, INV, MM, CC, DDABAL, CHECKS, TELLER, SAVBAL,
         ATMAMT, CDBAL, BRANCH, INS)


#Get the variable names in a vector
col_names <- colnames(train)

#Use the vector to see how many unique values are in each predictor
for ( i in seq_len( 14 ) ) {
  print(col_names[i])
  print(unique(train[[i]]) %>% length())
}

#Check for separation concerns
for ( i in seq_len( 14 ) ) {
  print( col_names[ i ] )
  print( table(train[[ i ]], train$INS) )
} 


#Continuous variables: DDBAL, Checks this was binned in the binned dataset
#Teller, was binned in binned dataset, SAVBAL, ATMAMT, CDBAL

train$INS <- as.factor(train$INS)
train$DDA <- as.factor(train$DDA)
train$NSF <- as.factor(train$NSF)
train$IRA <- as.factor(train$IRA)
train$INV <- as.factor(train$INV)
train$MM <- as.factor(train$MM)
train$BRANCH <- as.factor(train$BRANCH)
train$CC <- as.factor(train$CC)

set.seed(123)

hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

step <- train(x = subset(train, select = -INS),
              y = train$INS,
              method = "earth",
              glm = list(family = binomial(link = "logit")))
summary(step)

train$phat <- predict(step, type = "prob")
p1 <- train$phat$`1`
p0 <- train$phat$`0`



step2 <- train(x = subset(train, select = -INS),
              y = train$INS,
              method = "earth",
              tuneGrid = hyper_grid,
              trControl = trainControl(method = "cv", number = 10),
              glm = list(family = binomial(link = "logit")))

summary(step2)



train$phat <- predict(step2, type = "response")
p1 <- df

train_phat <- train
train_phat$phat <- predict.train(step2, type = 'prob')
plotROC(train_phat$INS, train_phat$phat$`1`)

mars <- earth(INS ~ ., data = train, 
              glm = list(family = binomial(link = "logit")))
summary(mars)

train$phat <- predict(mars, type = "response")
InformationValue::plotROC(train$INS, train$phat)



plotd(mars)
plot.earth.models(mars)
evimp(mars)


spline_model <- mgcv::gam(INS ~ DDA + NSF + IRA + INV + MM + CC + s(DDABAL) +
  s(CHECKS) + s(TELLER) + s(SAVBAL) + s(ATMAMT) + s(CDBAL) + BRANCH, 
  method = "REML", data = train, family = binomial(link = "logit"))

summary(spline_model)

spline_model_penalty <- mgcv::gam(INS ~ DDA + NSF + IRA + INV + MM + CC + s(DDABAL) +
      s(CHECKS) + s(TELLER) + s(SAVBAL) + s(ATMAMT) + s(CDBAL) + BRANCH, 
     method = "REML", data = train, select = TRUE, family = binomial(link = "logit"))

summary(spline_model_penalty) 


spline_post_penalty <- mgcv::gam(INS ~ DDA + NSF + IRA + INV + MM + CC + 
                               s(DDABAL) + s(CHECKS) + s(TELLER) + s(SAVBAL) + 
                                 s(ATMAMT) + s(CDBAL) + BRANCH,
                               method = "REML", data = train, 
                               family = binomial(link = "logit"))
summary(spline_post_penalty)
