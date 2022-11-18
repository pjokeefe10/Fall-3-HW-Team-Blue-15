# Testing Assumptions and Variable Selection

# Author: Sam Weiner and Meghana Subramaniam
# Version: 2022-11-18

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
hurricane <- read_csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

# ===============================

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


c(0,0,0,0,0,0) + c(1,1,1,1)

