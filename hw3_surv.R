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

sp_motor <- sp %>% 
  filter(reason == 1)

#Pivot hurricane to get h1:h48 cols into long format called hour, values to motor
hur_piv <- pivot_longer(hurricane, cols = 9:56, names_to = "Hour", values_to = "motor_on")
hur_piv$Hour <- sapply(hur_piv$Hour, as.numeric) #make values numeric

sp <- survSplit( Surv( hour, motor ) ~ motor_on, cut = 1:48, data = hur_piv, id = "id" )

sp_motor <- sp %>% 
  filter(motor == 1)

vet <- survival::veteran
fit1 <- coxph(Surv(time, status) ~ karno + age + trt, veteran)
plot(cox.zph(fit1)[1])

vet2 <- survSplit(Surv(time, status) ~., veteran,
                  cut=c(60, 120), episode ="timegroup")

hur_piv$Stop <- sp$hour[1:36960] + 1

hur_piv %>% 
  relocate(Stop, .after = Hour) -> hur_piv

hur_piv %>% 
  filter(motor == 1) %>% 
  View()

hurricane %>% 
  select(9:62) %>% 
  filter(motor == 1) %>% 
  View()

addict.cp <- survSplit( addicts, cut = addicts$survt[addicts$status == 1], end = 'survt', 
                       event = "status", start = 'start' )
addicts$survt[addicts$status == 1]

write_csv(hur_piv, "counting_hurricane.csv")
