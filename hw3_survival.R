library(tidyverse)
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
library(data.table)

hurricane <- read_csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

# ===============================

#'Read dataset
#'Censor non motor failure 
#'flip survival failure code
#'get index of 12 consecutive
#'

#Read in the dataset
hurricane <- read_csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

#DEAL WITH MISMATCH IN h1:h48 VALUES AND HOUR COLUMN
##' Assuming that the hour column for time of event is accurate
##' We modify the h cols to match the hour column value by having
##' numeric values up until event hour and replace values after with NA's.
##' In cases where event time is 48, we leave h48 with its numeric value if
##' such a value exists. When NA's exist before event time, we replace those
##' values with 0. We think that this should have a minimal effect on the 
##' analysis as these cases are few.

#ID column
hurricane$ID <- seq(1:770)

na_ids <- hurricane$ID
na_hour <- hurricane$hour

hurricane_t <- hurricane
hurricane_t[ is.na( hurricane_t ) ] <- 0

for ( i in seq_along( na_ids ) ){
  if ( na_hour[[i]] == 48 ){
    
  }else{
    x <- na_hour[[i]] + 9
    hurricane_t[ i , x:56 ] <- NA
  }
}

#Rename transformed df 
hurricane <- hurricane_t

# flip survival variable
hurricane$survive <- ifelse(hurricane$survive == 1, 0, 1)

#removing events where pump fails for reasons other than motor failure
# hurricane <- hurricane[hurricane$reason == 2, ]  

# create target variable for motor 
hurricane$motor <- ifelse(hurricane$reason == 2, 1, 0)

#Function to find 12 1s consecutively - index position
#need to make a global flag over the entire pump 
htime <- paste0("h", seq(1:48)) 
h_time <- str_remove(htime, "h")

#Make copy of hurricane
hurricane_12time <- hurricane

#Change h1:h48 col names to integer names
names(hurricane_12time)[9:56] <- h_time
names(hurricane_12time)

#Pivot Hurricane so h1:48 vars are under Hour, values to motor_on
hur_piv <- pivot_longer(hurricane_12time, cols = 9:56, names_to = "Hour", values_to = "motor_on")
hur_piv$Hour <- sapply(hur_piv$Hour, as.numeric) #make values numeric

thirteen <- rep(1,13)

motor_12_split <- split( hur_piv$motor_on, ceiling( seq_along( hur_piv$motor_on ) / 48 ) )

motor_12 <- frollapply( motor_12_split, length(thirteen), identical, thirteen )


first_12hours <- list()
for ( i in seq_along( motor_12 ) ) {
  holder <- unlist( motor_12[[i]] )
  first_12hours[[i]] <-  match( 1, holder )
}

#Create flag
for ( i in seq_along( first_12hours ) ){
  # prepost_12hour <- first_12hours[!is.na(first_12hours)];
  hurricane_12time$prepost_12hour <- ifelse(!is.na(first_12hours), 1, 0)
}  
#Change NA's into 48 in first_12hours, this is the new time variable
first_12vec <- unlist(first_12hours)

first_12vec[is.na(first_12vec)] <- 48

hurricane_12time$time_to_12 <- first_12vec

#create a copy
motor_12_test <- motor_12
#Replace NAs in Motor_12 with 0s
for ( i in seq_along( motor_12 ) ){
  motor_12_test[[i]][ is.na(motor_12_test[[i]] ) ] <- 0
}


names(motor_12_test) <- seq(1:770)

x <- as.data.frame(motor_12_test)
colnames(x) <- as.character( seq(1:770) )
piv_time_at12 <- pivot_longer(x, cols = everything(), names_to = "pump", values_to = "time_to_12")

piv_time_at12$pump <- as.numeric(piv_time_at12$pump)

piv_time_at12 %>% arrange(pump) -> piv_time_at12

hur_piv$Time_at12 <- piv_time_at12$time_to_12
hur_piv$tstart <- hur_piv$Hour - 1

hur_piv <- hur_piv %>% 
  relocate(tstart, .before = Hour)

hur_piv_fin <- hur_piv %>% na.omit()

#hour to fail
hour_to_failure <- list()

for ( i in seq_along( hurricane_12time$hour ) ){
  if ( hurricane_12time$survive[[i]] == 0 ){
    hour_to_failure[[i]] <- rep(0, 48) 
  }else if ( hurricane_12time$reason[[i]] == 2 ){
    x <- rep(0,hurricane_12time$hour[[i]]-1)
    hour_to_failure[[i]] <- c(x, 1)
  }else{
    x <- rep(0,hurricane_12time$hour[[i]])
    hour_to_failure[[i]] <- x
  }
}

un_htf <- unlist(hour_to_failure)

hur_piv_fin <- hur_piv %>% na.omit()

hur_piv_fin$motor <- un_htf

write_csv(hur_piv_fin, "counting_fin.csv")

counting_fin <- read_csv("https://github.com/pjokeefe10/Fall-3-HW-Team-Blue-15/blob/meghana/counting_fin.csv?raw=true")
lapply(counting_fin, unique)

## VARIABLE SELECTION ##
full.model <- coxph(Surv( tstart, Hour, motor ) ~  factor(backup) + age +
                      factor(bridgecrane) + factor(servo) + factor(gear) + slope + factor(elevation) + 
                      factor(Time_at12) + factor(motor_on), data = counting_fin)
empty.model <- coxph(Surv( tstart, Hour, motor ) ~ 1, data = counting_fin)
alpha.f=0.03 #set pvalue

#forward
# for.model <- step(empty.model, 
#                   scope = list(lower=formula(empty.model), 
#                                upper=formula(full.model)), 
#                   direction = "forward", k = qchisq(alpha.f, 1, lower.tail = FALSE))
# 
# #stepwise
# step.model <- step(empty.model, 
#                    scope = list(lower=formula(empty.model), 
#                                 upper=formula(full.model)), 
#                    direction = "both", k = qchisq(alpha.f, 1, lower.tail = FALSE))

#backward
back.model <- step(full.model, direction = "backward",
                   k = qchisq(alpha.f, 1, lower.tail = FALSE))
summary(back.model)

## BUILD MODELS ##
#model w/ variables from backward var selection
# cox_1 <- coxph(Surv(tstart, Hour, motor) ~ age + factor(servo) + slope,
#                data = counting_fin)
cox_2 <- coxph(Surv(tstart, Hour, motor) ~ age + slope,
               data = counting_fin)
summary(cox_2)
summary(cox_2)[7][1]

## ASSUMPTIONS ##
# linearity
visreg(cox_2, "slope", xlab = "slope", ylab = "partial residuals",gg = TRUE, band = FALSE) +  
  geom_smooth(col = "red", fill = "red") + 
  theme_bw() 

visreg(cox_2, "age", xlab = "age", ylab = "partial residuals",gg = TRUE, band = FALSE) +  
  geom_smooth(col = "red", fill = "red") + 
  theme_bw() 

# PH
pump.ph.zph <- cox.zph(cox_2, transform = "identity")
pump.ph.zph

ggcoxzph(pump.ph.zph)
