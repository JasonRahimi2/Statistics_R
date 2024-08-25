library(rio)
library(ggpubr)
library(ggplot2)
library(tidyverse)
Dataset = import("OECD_Dataset.csv")
view(Dataset)

#Descriptive statistics for OECD member countries
OECD <- subset(Dataset, LOCATION == "OECD")
mean(OECD$Value)
sd(OECD$Value)
median(OECD$Value)
OECD_rate <- OECD$Value
mode_OECD <- function(OECD_rate) {
  uniquev <- unique(OECD_rate)
  uniquev[which.max(tabulate(match(OECD_rate, uniquev)))]
}
mode_OECD(OECD_rate)

#Unemployment rate plot for OECD member countries
OECD_time_factor <- factor(OECD$TIME)
plot(OECD_time_factor, OECD$Value, xlab = "Date", ylab = "Unemployment Rate (%)")
title("Unemployment Rate Over Time in OECD Member Countries")
lines(OECD_time_factor, OECD$Value)

#Boxplot for unemployment rate in OECD member countries
ggboxplot(OECD$Value, xlab = "OECD", ylab = "Unemployment Rate (%)", title="Unemployment Rate in OECD Member Countries")

#Descriptive statistics for Japan
JPN <- subset(Dataset, LOCATION == "JPN")
mean(JPN$Value)
sd(JPN$Value)
median(JPN$Value)
JPN_rate <- JPN$Value
mode_JPN <- function(JPN_rate) {
  uniquev <- unique(JPN_rate)
  uniquev[which.max(tabulate(match(JPN_rate, uniquev)))]
}
mode_JPN(JPN_rate)

#Unemployment rate plot for Japan
JPN_time_factor <- factor(JPN$TIME)
plot(JPN_time_factor, JPN$Value, xlab = "Date", ylab = "Unemployment Rate (%)")
title("Unemployment Rate Over Time in Japan")
lines(JPN_time_factor, JPN$Value)

#Boxplot for unemployment rate in Japan
ggboxplot(JPN$Value, xlab = "Japan", ylab = "Unemployment Rate (%)", title = "Unemployment Rate in Japan")

#Descriptive statistics for France
FRA <- subset(Dataset, LOCATION == "FRA")
mean(FRA$Value)
sd(FRA$Value)
median(FRA$Value)
FRA_rate <- FRA$Value
mode_FRA <- function(FRA_rate) {
  uniquev <- unique(FRA_rate)
  uniquev[which.max(tabulate(match(FRA_rate, uniquev)))]
}
mode_FRA(FRA_rate)

#Unemployment rate plot for France
FRA_time_factor <- factor(FRA$TIME)
plot(FRA_time_factor, FRA$Value, xlab = "Date", ylab = "Unemployment Rate (%)")
title("Unemployment Rate Over Time in France")
lines(FRA_time_factor, FRA$Value)

#Boxplot for unemployment rate in France
ggboxplot(FRA$Value, xlab = "France", ylab = "Unemployment Rate (%)", title = "Unemployment Rate in France")

#Checking unemployment rates for countries and groups in the dataset
boxplot(Value ~ LOCATION, data = Dataset, xlab = "Country/Group", ylab = "Unemployment Rate (%)")
title("Unemployment Rate in Each OECD Member Country and Groups")

#Unemployment rate over time for all countries and groups in the dataset
time_factor <- factor(Dataset$TIME)
plot(time_factor, Dataset$Value, xlab = "Date", ylab = "Unemployment Rate (%)")
title("Unemployment Rate in Each OECD Member Country and Groups Over Time")