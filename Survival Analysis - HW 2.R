# libraries
library(survival)
library(foreign)
library(ggplot2)
library(survminer)
library(rms)
library(flexsurv)
library(dplyr)
library(ciTools)
library(here)
library(visreg)
library(cmprsk)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gmodels)

# read in data
hurricane <- read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

# flip survival variable
hurricane$survive <- ifelse(hurricane$survive, 0, 1)
hurricane$flood <- ifelse(hurricane$reason == 1, 1, 0) # create target variable for flood failures
hurricane <- hurricane[-c(9:56)] # drop h1-h48 variables 

# aft

# log normal dist
hurr.aft.ln <- survreg(Surv(hour, flood) ~ backup + age + bridgecrane
                       + servo + gear + trashrack + slope + elevation, 
                       data = hurricane, dist = 'lognormal')
summary(hurr.aft.ln)

# weibull
hurr.aft.w <- survreg(Surv(hour, flood) ~ backup + age + bridgecrane
                      + servo + gear + trashrack + slope + elevation, 
                      data = hurricane, dist = 'weibull')
summary(hurr.aft.w) 

# exp
hurr.aft.e <- survreg(Surv(hour, flood) ~ backup + age + bridgecrane
                      + servo + gear + trashrack + slope + elevation, 
                      data = hurricane, dist = 'exp')
summary(hurr.aft.e)

######## checking distributions w/ plots ############ 
# weibull
hurr.w <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane
                      + servo + gear + trashrack + slope + elevation, 
                      data = hurricane, dist = 'weibull')
plot(hurr.w, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Weibull Distribution")

# exp
hurr.e <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane
                      + servo + gear + trashrack + slope + elevation, 
                      data = hurricane, dist = 'exp')
plot(hurr.e, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Exponential Distribution")

# gamma
hurr.g <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane
                      + servo + gear + trashrack + slope + elevation, 
                      data = hurricane, dist = 'gamma')
plot(hurr.g, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Gamma Distribution")

####### goodness-of-fit tests ########
like.e <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane
                      + servo + gear + trashrack + slope + elevation, 
                      data = hurricane, dist = "exp")$loglik
like.w <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane
                      + servo + gear + trashrack + slope + elevation, 
                      data = hurricane, dist = 'weibull')$loglik

like.g <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane
                      + servo + gear + trashrack + slope + elevation, 
                      data = hurricane, dist = 'gamma')$loglik

# check pvalues
pval.e.g = pchisq((-2*(like.e-like.g)), 2,lower.tail=F)
pval.w.g = pchisq((-2*(like.w-like.g)), 1,lower.tail=F)
pval.e.w = pchisq((-2*(like.e-like.w)), 1,lower.tail=F)

Tests = c('Exp vs. Gam', 'Wei vs. Gam', 'Exp vs. Wei')
P_values = c(pval.e.g, pval.w.g, pval.e.w)
cbind(Tests, P_values)

####### proceed w/ weibull for manual backward variable selection ######
#exclude elevation
hurr.aft.w1 <- survreg(Surv(hour, flood) ~ backup + age + bridgecrane
                       + servo + gear + trashrack + slope, 
                       data = hurricane, dist = 'weibull') 
summary(hurr.aft.w1) 

#exclude age
hurr.aft.w2 <- survreg(Surv(hour, flood) ~ backup + bridgecrane
                       + servo + gear + trashrack + slope, 
                       data = hurricane, dist = 'weibull') 
summary(hurr.aft.w2)

#exclude bridgecrane
hurr.aft.w3 <- survreg(Surv(hour, flood) ~ backup +
                         servo + gear + trashrack + slope, 
                       data = hurricane, dist = 'weibull') 
summary(hurr.aft.w3)

#exclude gear
hurr.aft.w4 <- survreg(Surv(hour, flood) ~ backup +
                         servo + trashrack + slope, 
                       data = hurricane, dist = 'weibull') 
summary(hurr.aft.w4)

#exclude trashrack
hurr.aft.w5 <- survreg(Surv(hour, flood) ~ backup +
                         servo + slope, 
                       data = hurricane, dist = 'weibull') 
summary(hurr.aft.w5)

# final model has backup + servo + slope

####### analyze pumps that failed due to flood failure #########
#flood <- hurricane %>% filter(flood == 1)
#flood <- flood[c("backup", "servo", "slope", "hour", "survive")] # subset to relevant vars


# backup

# actual time
survprob.actual = 1 - psurvreg(hurricane$hour,
                               mean = predict(hurr.aft.w5, type = "lp"),
                               scale = hurr.aft.w5$scale, distribution = hurr.aft.w5$dist)

# new time
new_time = qsurvreg(1 - survprob.actual,
                    mean = predict(hurr.aft.w5, type = "lp") +
                      coef(hurr.aft.w5)['backup'],
                    scale = hurr.aft.w5$scale,
                    distribution = hurr.aft.w5$dist)

#calc new time
hurricane$new_time = new_time
hurricane$diff = hurricane$new_time - hurricane$hour
impact.fin=data.frame(hurricane$hour, hurricane$new_time,
                      hurricane$diff,hurricane$flood,hurricane$backup)
colnames(impact.fin)=c("O.Week","N.Week","Diff","flood","Backup")
impact.fin2=subset(impact.fin,flood==1 & Backup==0)
#head(impact.fin2)

### servo

# actual time
survprob.actual = 1 - psurvreg(hurricane$hour,
                               mean = predict(hurr.aft.w5, type = "lp"),
                               scale = hurr.aft.w5$scale, distribution = hurr.aft.w5$dist)

# new time
new_time = qsurvreg(1 - survprob.actual,
                    mean = predict(hurr.aft.w5, type = "lp") +
                      coef(hurr.aft.w5)['servo'],
                    scale = hurr.aft.w5$scale,
                    distribution = hurr.aft.w5$dist)

hurricane$new_time = new_time
hurricane$diff = hurricane$new_time - hurricane$hour
impact.fin=data.frame(hurricane$hour, hurricane$new_time,
                      hurricane$diff,hurricane$flood,hurricane$servo)
colnames(impact.fin)=c("O.Week","N.Week","Diff","flood","servo")
impact.fin2=subset(impact.fin,flood==1 & servo==0)
impact.fin2


# backup - 100K -> 1.15 mil for all
# servo - 150k -> 1.725 mil for all (all )
