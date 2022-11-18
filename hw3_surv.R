# Survival Homework 2

# Author: Sam Weiner
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

#ID column
hurricane$ID <- seq(1:770)

# flip survival variable
hurricane$survive <- ifelse(hurricane$survive, 0, 1)
hurricane$motor <- ifelse(hurricane$reason == 2, 1, 0) # create target variable for motor 

#Remove "h" from names of hurricane cols to prepare for pivoting
htime <- paste0("h", seq(1:48)) 
h_time <- str_remove(htime, "h")

names(hurricane)[9:56] <- h_time
names(hurricane)


sp <- survSplit( Surv( hour, reason ) ~ 1, cut = c(seq(1:48)), data = hurricane)
# 
# sp_motor <- sp %>% 
#   filter(reason == 1)
# 
#Pivot hurricane to get h1:h48 cols into long format called hour, values to motor
hur_piv <- pivot_longer(hurricane, cols = 9:56, names_to = "Hour", values_to = "motor_on")
hur_piv$Hour <- sapply(hur_piv$Hour, as.numeric) #make values numeric
# 
# sp <- survSplit( Surv( hour, motor ) ~ motor_on, cut = 1:48, data = hur_piv, id = "id" )
# 
# sp_motor <- sp %>% 
#   filter(motor == 1)
# 
# vet <- survival::veteran
# fit1 <- coxph(Surv(time, status) ~ karno + age + trt, veteran)
# plot(cox.zph(fit1)[1])
# 
# vet2 <- survSplit(Surv(time, status) ~., veteran,
#                   cut=c(60, 120), episode ="timegroup")
# 
# hur_piv$Stop <- sp$hour[1:36960] + 1
# 
# hur_piv$Hour <- hur_piv$Hour -1
# hur_piv$Stop <- hur_piv$Stop - 1
# 
# hur_piv %>% 
#   relocate(Stop, .after = Hour) -> hur_piv
# 
# hur_piv %>% 
#   filter(motor == 1) %>% 
#   View()
# 
# hurricane %>% 
#   select(9:62) %>% 
#   filter(motor == 1) %>% 
#   View()
# 
# addict.cp <- survSplit( addicts, cut = addicts$survt[addicts$status == 1], end = 'survt', 
#                        event = "status", start = 'start' )
# addicts$survt[addicts$status == 1]
# 
# hur_sel <- hur_piv %>% 
#   select(!c(reason, reason2, survive))
# 
# write_csv(hur_sel, "counting_hurricane.csv")
# 
# 
# p <- read_csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/recid_long.csv")
# 
# piv <- hur_piv %>% 
#   na.omit()
# 
# piv %>% 
#   filter(ID == 320) %>% 
#   View()

hur_sel <- read_csv("counting_hurricane.csv")

hur_naomit <- hur_sel %>% na.omit()

#1

cox.hurr <- coxph( Surv( Hour, Stop, motor ) ~ factor(backup) + age + 
                     factor(bridgecrane) + factor(servo) + factor(trashrack)
                   + slope + elevation + factor(motor_on), data = hur_sel)
summary(cox.hurr)

#2
cox.hurr <- coxph( Surv( Hour, Stop, motor ) ~ backup + age + bridgecrane + servo
                   + slope + elevation + motor_on, data = hur_sel)
summary(cox.hurr)

hold <- hur_sel %>% 
  group_by(ID) %>% 
  summarise(motor_12 = c( rep( 1, 12 ) ) %in% motor_on)
str(hold)

#Function to find 12 1s consecutively
twelve <- rep(1,12)

motor_12_split <- split( hur_sel$motor_on, ceiling( seq_along( hur_sel$motor_on ) / 48 ) )

motor_12 <- frollapply( motor_12_split, length(twelve), identical, twelve )

hur_sel %>% 
  filter(ID == 769) %>% 
  View()

first_12hours <- list()
for ( i in seq_along( motor_12 ) ) {
  holder <- unlist( motor_12[[i]] )
  first_12hours[[i]] <-  match( 1, holder )
}


first_12hours[[2]]


prepost_12hour <- hur_sel

for ( i in seq_along(first_12hours ) ){
  
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
