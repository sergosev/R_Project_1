library(tidyr)
library(dplyr)
library(ggplot2)
library(DescTools)

data <- read.csv("day.csv", header = T)
sapply(data, FUN = function(x) {
  table(is.na(x))[2]
})

str(data)
data$season <- as.factor(data$season)
data$yr <- as.factor(data$yr)
data$holiday <- as.factor(data$holiday)
data$weekday <- as.factor(data$weekday)
data$workingday <- as.factor(data$workingday)
data$weathersit <- as.factor(data$weathersit)
data$mnth <- as.factor(data$mnth)

#Month effect
grouped_means <- data %>% select(c(mnth, yr, casual, registered)) %>%
  group_by(across(c(yr, mnth))) %>%
  summarise(mean_casual = mean(casual, na.rm=T),
            mean_reg = mean(registered, na.rm=T))

ggplot(data = grouped_means, aes(x = mnth, y = mean_casual))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(~ yr)


ggplot(data = grouped_means, aes(x = mnth, y = mean_reg))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(~ yr)
