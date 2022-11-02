library(tidyverse)
library(openintro)


glimpse(kobe_basket)


kobe_streak <- calc_streak(kobe_basket$shot)

ggplot(data = kobe_streak, aes(x = length)) +
  geom_bar()


coin_outcomes <- c("heads", "tails")

sample(coin_outcomes, size = 1, replace = TRUE)

sim_fair_coin <- sample(coin_outcomes, size = 100, replace = TRUE)

table(sim_fair_coin)
