cor(train[, c('ACCTAGE', 'DDABAL', 'DEP','DEPAMT', 'CHECKS', 'NSFAMT','PHONE', 
          'TELLER', 'SAVBAL', 'ATMAMT', 'POS', 'POSAMT', 'CDBAL', 'IRABAL',
          'INVBAL', 'MMBAL', 'CCBAL', 'INCOME', 'LORES', 'HMVAL', 'AGE', 
          'CRSCORE')])

library("mgcv")
## simulate data with concurvity...
set.seed(8)
n<- 200
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
  (10 * x)^3 * (1 - x)^10
t <- sort(runif(n)) ## first covariate
## make covariate x a smooth function of t + noise...
x <- f2(t) + rnorm(n)*3
## simulate response dependent on t and x...
y <- sin(4*pi*t) + exp(x/20) + rnorm(n)*.3

## fit model...
b <- gam(y ~ s(t,k=15) + s(x,k=15), method="REML")

x2 <- seq_len(n) + rnorm(n)*3
b2 <- update(b, . ~ . + x2)

concurvity(b)
concurvity(b2)

cor(t, x2)

concurvity(update(b, . ~ . + s(x2)))

train$INS <- as.factor(train$INS)
train$MMCRED <- as.factor(train$MMCRED)

x <- model.matrix(INS ~ ., data = train)[, -1]

y <- as.numeric(train$INS)
y <- ifelse(y == 1, 0, 1)
y <- as.factor(y)
model <- caret::train(x,
                      y,
             method = 'glmnet',
             family = "binomial",
             tuneGrid = expand.grid(.alpha = seq(0,1, by = 0.05),
                                    .lambda = seq(0.01,4, by = 0.05)),
             trControl = trainControl(method = 'cv', number = 10))
model

cv.fit <- cv.glmnet(x, y, family = 'binomial', type.measure = 'auc')
cv.fit
coef(cv.fit, s = "lambda.1se") 

plot(cv.fit)
print(cv.fit)

cv.fit.class <- cv.glmnet(x, y, family = 'binomial', type.measure = 'class')
cv.fit.class
plot(cv.fit.class)

coef(cv.fit.class)

train_cv <- train %>% 
  select(DDA, DDABAL, DEP, CHECKS, PHONE, TELLER, SAV, SAVBAL, ATM, ATMAMT,
         CD, CDBAL, IRA, INV, MM, CC, CCPURC, BRANCH, FLAG_NA_PHONE, 
         FLAG_NA_POS, FLAG_NA_POSAMT, FLAG_NA_INVBAL, FLAG_NA_CCBAL, INS)



earth.test <- earth(INS ~ ., data = train_cv,
                    glm = list(family = 'binomial'))
earth.test
varImp(earth.test)


