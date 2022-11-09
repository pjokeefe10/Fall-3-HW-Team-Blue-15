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


hurricane <- read.csv("https://github.com/sjsimmo2/Survival/raw/master/hurricane.csv")
head(hurricane)

hurricane <- data.frame(lapply(hurricane, function(x) as.factor(x)))

hurricane$hour <- as.numeric(hurricane$hour)
hurricane$age <- as.numeric(hurricane$age)
hurricane$slope <- as.numeric(hurricane$slope)
hurricane$elevation <- as.numeric(hurricane$elevation)

str(hurricane)


describe(hurricane)

hurricane_missing <- hurricane[rowSums(is.na(hurricane)) > 0,]

hurricane$censor <- ifelse(hurricane$survive == 1, 0, 1)


#Survival Analysis Object
hurricane.surv = Surv(time=hurricane$hour,event=hurricane$censor)

hurricane.km=survfit(Surv(time=hour,event=censor)~1, data = hurricane)
summary(hurricane.km)

plot(hurricane.km, main = "Survival Function", xlab = "Tenure", ylab = "Survival Probability")

#Hazard function
h = hurricane.km$n.event/hurricane.km$n.risk
index.h=rep(0,length=(max(hurricane$hour)+1)) #Need to add 0
index.h[(hurricane.km$time)+1]=h #Because of 0
haz.plot=data.frame(cbind(seq(0,max(hurricane$hour)), index.h))
colnames(haz.plot)=c("Time","Hazard")
ggplot(haz.plot,aes(x=Time,y=Hazard))+geom_line()

#Cumulative hazard
ggsurvplot(hurricane.km, data = hurricane, fun = "cumhaz", conf.int = TRUE,  palette = "blue", xlab = "Week",
           ylab = "Cumulative Hazard", legend = "none")

ggsurvplot(
  hurricane.km,
  data = hurricane,
  size = 1,                 
  palette = "blue",
  conf.int = TRUE,              
  risk.table = TRUE,    
  risk.table.height = 0.25, 
  ggtheme = theme_bw()      
)


#Stratified Survival analysis
reason.surv = survfit(Surv(time=hour,event=censor)~reason, data = hurricane)
ggsurvplot(reason.surv, data=hurricane, palette = c("blue","orange","red","purple","yellow"),conf.int = T)


#Test differences
survdiff(Surv(time=hour,event=censor)~reason, data = hurricane, rho = 0)

survdiff(Surv(time=hour,event=censor)~reason, data = hurricane, rho = 1)


#Hazard function
h = hurricane.km$n.event/hurricane.km$n.risk
index.h=rep(0,length=(max(hurricane$hour)+1)) #Need to add 0
index.h[(hurricane.km$time)+1]=h #Because of 0
haz.plot=data.frame(cbind(seq(0,max(hurricane$hour)), index.h))
colnames(haz.plot)=c("Time","Hazard")
ggplot(haz.plot,aes(x=Time,y=Hazard))+geom_line()

#Cumulative hazard
ggsurvplot(reason.surv, data = hurricane, fun = "cumhaz", conf.int = TRUE,  palette = c("blue","orange","red","purple","yellow"), xlab = "Week",
           ylab = "Cumulative Hazard", legend = "none")

ggsurvplot(
  reason.surv,
  data = hurricane,
  size = 1,                 
  palette =
    c("blue","orange","red","purple","yellow"),
  conf.int = TRUE,          
  pval = TRUE,              
  risk.table = TRUE,
  legend.labs =
    c("0", "1", "2", "3", "4"),    
  risk.table.height = 0.25, 
  ggtheme = theme_bw()      
)
