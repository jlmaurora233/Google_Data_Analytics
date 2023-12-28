library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# read in data
daily_activity <- read.csv('dailyActivity_merged.csv')
daily_calories <- read.csv('dailyCalories_merged.csv')
daily_intensities <- read.csv('dailyIntensities_merged.csv')
daily_steps <- read.csv('dailySteps_merged.csv')
hourly_calories <- read.csv('hourlyCalories_merged.csv')
hourly_intensities <- read.csv('hourlyIntensities_merged.csv')
hourly_steps <- read.csv('hourlySteps_merged.csv')

glimpse(daily_activity)
glimpse(daily_calories)
glimpse(daily_intensities)
glimpse(daily_steps)
glimpse(hourly_calories)
glimpse(hourly_intensities)
glimpse(hourly_steps)

# convert date to date objects
