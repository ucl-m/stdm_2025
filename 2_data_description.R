# -------------------
# Prerequisites:
#    - Functionality in '1_get_data.R' must be run in order to download and format data

# Functionality:
#
# (1) Describes and visualises the collision data
# (2) Exploratory spatio-temporal data analysis
#
# Instructions for use:
# -> Install R packages as necessary
# -> Set working directory
# -------------------

library(sf)
library(ggplot2)

# USER SET VARIABLES:
w_dir <- "C:/Users/Boomerang/OneDrive - University College London/Documents/CEGE0042 - Spatial-Temporal Data Analysis (STDM)/Coursework/final/"

# Set working directory
setwd(w_dir)

# Load data
data = read.csv("gla_collisions_sf.csv")

# Summarise data
summarise_stats19 = function(x) {
  data.frame(row.names = 1:length(x),
             name = substr(names(x), 1, 19),
             class = sapply(x, function(v) class(v)[1]),
             n_unique = sapply(x, function(v) length(unique(v))),
             first_label = sapply(x, function(v) substr(unique(v)[1], 1, 16)),
             most_common_value = sapply(x, function(v) 
               substr(names(sort(table(v), decreasing = TRUE)[1]), 1, 16)[1])
  )
}

knitr::kable(summarise_stats19(data), 
             caption = "Summary of formatted crash data.")



# extract the month from the 'date' column
data$month <- format(as.Date(data$date, format="%d/%m/%Y"),"%B")
unique(data$month)

# daily mean and std deviation (aggregate by day)
class(data)
daily_agg = aggregate(data[c("date")],
                      by = list(data$date),
                      FUN = length)

daily_agg

mu = mean(daily_agg$date)
mu

sdev = sd(daily_agg$date)
sdev

# Histogram with mean
hist(daily_agg$date, xlab = "Daily total accidents", main = "Histogram of daily total accidents")
abline(v=mu, col="red")

qqnorm(daily_agg$date)
qqline(daily_agg$date, col="red")

# hourly mean and std deviation (convert to decimal time)
mu_time = mean(daily_agg$date)
mu_time

sdev_time = sd(daily_agg$date)
sdev_time

hist(data$hour_decimal)
abline(v=mean(data$hour_decimal), col="red")

qqnorm(data$hour_decimal)
qqline(data$hour_decimal, col="red")