# Survival Homework 2, no pivot test

# Author: Sam Weiner & Meghana Subramaniam
# Version: 2022-11-16

# Packages
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
#Parameters

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
hurricane$survive <- ifelse(hurricane$survive, 0, 1)

#removing events where pump fails for reasons other than motor failure
# hurricane <- hurricane[hurricane$reason == 2, ]  

hurricane$motor <- ifelse(hurricane$reason == 2, 1, 0) # create target variable for motor 



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
for ( i in seq_along(first_12hours ) ){
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

write_csv(hur_piv_fin, "counting_fin.csv")

#Replace the h1:48 cols with values from motor_12_test
for ( i in seq_along( motor_12_test ) ){
  
  hurricane_12time[i, 9:56]
}


#Use survSplit to make count data frame

count_12 <- tmerge(data1 = hurricane_12time, data2 = hurricane_12time, 
                   id=ID, tstop = hour)
count_12 <- tmerge(data1 = count_12, data2 = hurricane_12time, id=ID, 
                   consec_12 = tdc(time_to_12))
summary(count_12)

cox_1 <- coxph( Surv( tstart, tstop, consec_12 ) ~  factor(backup) + age + 
                  factor(bridgecrane) + factor(servo) + slope + elevation, 
                cluster = ID, data = count_12)
summary(cox_1)


#Use survSplit to make count data frame
#Data frame has one column: consec_12 to indicate 12 hours consecutively,
#Failure indicates if the failure occured for that observation in the tstart to
#tstop time window

count_12test <- tmerge(data1 = hurricane_12time, data2 = hurricane_12time, 
                       id=ID, tstop = hour)
count_12test <- tmerge(data1 = count_12, data2 = hurricane_12time, id=ID, failure = event(hour),
                       consec_12 = tdc(time_to_12))
summary(count_12test)
attr(count_12test, "tcount")

cox_1 <- coxph( Surv( tstart, tstop, failure ) ~  factor(backup) + age + 
                  factor(bridgecrane) + factor(servo) + slope + elevation + 
                  factor(consec_12), cluster = ID, data = count_12test)
summary(cox_1)

