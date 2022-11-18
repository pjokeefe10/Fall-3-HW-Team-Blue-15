# Survival Homework 2

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
hurricane <- hurricane[hurricane$reason == 2, ]  

hurricane$motor <- ifelse(hurricane$reason == 2, 1, 0) # create target variable for motor 

#Remove "h" from names of hurricane cols to prepare for pivoting
htime <- paste0("h", seq(1:48)) 
h_time <- str_remove(htime, "h")

#Change h1:h48 col names to integer names
names(hurricane)[9:56] <- h_time
names(hurricane)

#Pivot Hurricane so h1:48 vars are under Hour, values to motor_on
hur_piv <- pivot_longer(hurricane, cols = 9:56, names_to = "Hour", values_to = "motor_on")
hur_piv$Hour <- sapply(hur_piv$Hour, as.numeric) #make values numeric

#Use survSplit to get start and stop columns, to append to hur_piv
sp <- survSplit( Surv( hour, motor ) ~ ID, cut = c(1:48), data = hurricane, episode = "ep")

hur_piv$Stop <- sp$hour[1:5376] + 1

# hur_piv$Hour <- hur_piv$Hour -1
# hur_piv$Stop <- hur_piv$Stop - 1

hur_piv %>%
  relocate(Stop, .after = Hour) -> hur_piv


#Function to find 12 1s consecutively - index position
#need to make a global flag over the entire pump 
twelve <- rep(1,12)

motor_12_split <- split( hur_piv$motor_on, ceiling( seq_along( hur_piv$motor_on ) / 48 ) )

motor_12 <- frollapply( motor_12_split, length(twelve), identical, twelve )


first_12hours <- list()
for ( i in seq_along( motor_12 ) ) {
  holder <- unlist( motor_12[[i]] )
  first_12hours[[i]] <-  match( 1, holder )
}

#Create flag
for ( i in seq_along(first_12hours ) ){
  # prepost_12hour <- first_12hours[!is.na(first_12hours)];
  hurricane$prepost_12hour <- ifelse(!is.na(first_12hours), 1, 0)
}


#Pivot AGAIN hur_piv to get h1:h48 cols into short format called hour, values to motor
hur_piv_final <- pivot_longer(hurricane, cols = 9:56, names_to = "Hour", values_to = "motor_on")
hur_piv_final$Hour <- sapply(hur_piv$Hour, as.numeric) #make values numeric

# prepost_12hour <- hur_piv #pivoted dataframe - 36k
# #df$upset <- ifelse(df$score_diff<0 & df$seed_diff>5, 1, 0)
# hur_piv2 <- hurricane
# 
# for ( i in seq_along(first_12hours ) ){
#   hur_piv$prepost_12hour <- ifelse(!is.na(first_12hours), 1, 0)
# }

#Add the Stop col to final piv table
hur_piv_final$Stop <- sp$hour[1:5376] + 1
 
# hur_piv_final$Hour <- hur_piv_final$Hour -1
# hur_piv_final$Stop <- hur_piv_final$Stop - 1

hur_piv_final %>%
  relocate(Stop, .after = Hour) -> hur_piv_final

# sp <- survSplit( Surv( hour, motor ) ~ motor_on, cut = 1:48, data = hur_piv, id = "id" )


# 
# write_csv(hur_sel, "counting_hurricane.csv")


# hur_sel <- read_csv("https://raw.githubusercontent.com/pjokeefe10/Fall-3-HW-Team-Blue-15/Sam/counting_hurricane.csv")

lapply(hur_piv_final, unique) #trashrack only had one value, removed

#1

cox.hurr <- coxph( Surv( Hour, Stop, motor ) ~ factor(backup) + age + 
                     factor(bridgecrane) + factor(servo) + slope + elevation + 
                     factor(prepost_12hour), data = hur_piv_final)
summary(cox.hurr)
#Issue with the above: The prepost_12hour has the same value for every pump 
# regardless of time interval. This means that the start stop columns till have
# the same information for every hour and so cannot delineate a difference 
# between pre 12 and post 12 hazards in one pump. 

#THE VARIABLE LOOKS INTO THE FUTURE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#Without the start and stop time
cox.hurr_wo <- coxph( Surv( hour, motor ) ~ factor(backup) + age + 
                     factor(bridgecrane) + factor(servo) + slope + elevation + 
                     factor(prepost_12hour), data = hur_piv_final)
summary(cox.hurr_wo)

#Using the regular Hurricane Data
cox.hurr_reg <- coxph( Surv( hour, motor ) ~ factor(backup) + age + 
                     factor(bridgecrane) + factor(servo) + slope + elevation + 
                     factor(prepost_12hour), data = hurricane)
summary(cox.hurr_reg)
plot(cox.zph(cox.hurr_reg)[7])

#2
cox.hurr <- coxph( Surv( Hour, Stop, motor ) ~ backup + age + bridgecrane + servo
                   + slope + elevation + motor_on, data = hur_piv_final)
summary(cox.hurr)



# Miscellaneous -----------------------------------------------------------

prepost_12hour <- hur_sel #pivoted dataframe - 36k
#df$upset <- ifelse(df$score_diff<0 & df$seed_diff>5, 1, 0)

for ( i in seq_along(first_12hours ) ){
 # prepost_12hour <- first_12hours[!is.na(first_12hours)];
  hur_piv_final$prepost_12hour <- ifelse(!is.na(first_12hours), 1, 0)
}

first_12_index <- unlist(first_12hours)

#Replaces each individual index for one pump into index that aligns with hur_sel 
firsn_12_test <- list()
for ( i in seq_along( first_12hours ) ){
  firsn_12_test[[i]] <- ( 48 * (i - 1) + first_12hours[[i]] )
}
#Unlist the first_12_test
first_12_vec <- unlist(firsn_12_test)

# sp <- survSplit( Surv( hour, reason ) ~ 1, cut = first_12_vec, data = hurricane)


for ( i in seq_along( first_12_vec ) ){
  if ( !is.na( first_12_vec[[i]] ) ){
    
  }else{
    first_12_vec[[i]] <- 48 * (i-1)
  }
}

first_vec_12_12 <- unlist(motor_12)
first_vec_12_12[is.na( first_vec_12_12 )] <- 0

prepost_12hour$motor_on <- first_vec_12_12

sp <- survSplit( Surv( Hour, motor_on ) ~ 1, cut = 0:48, 
                 data = prepost_12hour, id = "id", zero = 0)

prepost_12hour %>% 
  group_by(ID) %>% 
  mutate( motor_on2 = ifelse( cumany( motor_on == 1), 0, motor_on ) ) %>% 
  View()

prepost_12hour %>% cumany( motor_on == 1 )