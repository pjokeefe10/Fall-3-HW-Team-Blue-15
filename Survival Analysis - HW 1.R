# libraries
library(tidyverse)
library(ggplot2)
library(survival)
library(survminer)
library(dplyr)
library(reshape2)

# read in data

df <- read.csv('https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv')

# flip survival variable
df$survive <- ifelse(df$survive, 0, 1)

# Give the percentage of pumps within each failure type and percentage of pumps that did not fail.
df %>% group_by(reason) %>% summarise(Percentage=n()/nrow(.))

df %>% group_by(survive) %>% summarise(Percentage=n()/nrow(.))

# Give the average time until failure for each failure type.  Are means a good measure for length of survival?  Discuss why or why not.

df %>% 
  group_by(reason, survive) %>% 
  summarize(value = mean(hour, na.rm = T)) %>% 
  pivot_wider(names_from = reason)


# Create and upload the survival probability across time for pumps broken down by failure type overlaid into one graph.
hurr_km =survfit(Surv(time = hour, event = survive)~ reason, data = df)

ggsurvplot(hurr_km,data=df,palette = c("blue","orange", "red", "purple", "yellow"),conf.int =F,
           legend.title = "Reason for Failure", legend.labs = c("No Failure", "Flood", "Motor", "Surge", "Jammed"), xlab='Hour')

# Create and upload the graph of conditional failure probabilities across time for pumps broken down by failure type 
# overlaid into one graph.

# perhaps separate into failures
df_0 <- df %>% filter(df$reason == 0)
df_1 <- df %>% filter(df$reason == 1)
df_2 <- df %>% filter(df$reason == 2)
df_3 <- df %>% filter(df$reason == 3)
df_4 <- df %>% filter(df$reason == 4)

hurr_km_0 =survfit(Surv(time = hour, event = survive)~ reason, data = df_0)
hurr_km_1 =survfit(Surv(time = hour, event = survive)~ reason, data = df_1)
hurr_km_2 =survfit(Surv(time = hour, event = survive)~ reason, data = df_2)
hurr_km_3 =survfit(Surv(time = hour, event = survive)~ reason, data = df_3)
hurr_km_4 =survfit(Surv(time = hour, event = survive)~ reason, data = df_4)

h_0= hurr_km_0$n.event/hurr_km_0$n.risk
h_1= hurr_km_1$n.event/hurr_km_1$n.risk
h_2= hurr_km_2$n.event/hurr_km_2$n.risk
h_3= hurr_km_3$n.event/hurr_km_3$n.risk
h_4= hurr_km_4$n.event/hurr_km_4$n.risk



# need to fill all with 0 to 48
h_0_rep <- h_0
h_0_rep[(length(h_0)):49]=0
h_1_rep <- h_1
h_1_rep[(length(h_1)):49]=0
h_2_rep <- h_2
h_2_rep[(length(h_2)):49]=0
h_3_rep <- h_3
h_3_rep[(length(h_3)):49]=0
h_4_rep <- h_4
h_4_rep[(length(h_4)):49]=0

index.h=rep(0,length=(max(df$hour)+1)) #Need to add 0


haz.plot=data.frame(cbind(seq(0,max(df$hour)), h_0_rep))
haz.plot$haz_1 <- h_1_rep
haz.plot$haz_2 <- h_2_rep
haz.plot$haz_3 <- h_3_rep
haz.plot$haz_4 <- h_4_rep
colnames(haz.plot)=c("Time","No Failure", "Flood", "Motor", "Surge", "Jammed")


data_long <- melt(haz.plot, id = "Time")
data_long <- data_long %>% rename(Reason = variable)
ggplot(data_long, aes(x = Time, y = value, color = Reason)) +  geom_line()+ ylab("Conditional Failure Probability") + 
  ggtitle("Conditional Failure Probabilities Across Time by Failure Type") 


# Provide a statistical test to see if the major types of failure have similar survival probabilities across time 
# (include null and alternative hypotheses, test statistic, p-value and conclusion).

survdiff(Surv(hour, survive) ~ reason, data=df, rho=0)
