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

# remove NA or duplicates in the datasets, if any
daily_activity <- daily_activity %>% 
  drop_na() %>% distinct()
daily_calories <- daily_calories %>% 
  drop_na() %>% distinct()
daily_intensities <- daily_intensities %>% 
  drop_na() %>% distinct()
daily_steps <- daily_steps %>% 
  drop_na() %>% distinct()
hourly_calories <- hourly_calories %>% 
  drop_na() %>% distinct()
hourly_intensities <- hourly_intensities %>% 
  drop_na() %>% distinct()
hourly_steps <- hourly_steps %>% 
  drop_na() %>% distinct()

# process the date column (convert dtype & sort by date)
daily_activity <- daily_activity %>% 
  mutate(ActivityDate = as.Date(mdy(ActivityDate))) %>%
  arrange(ActivityDate) 
daily_calories <- daily_calories %>% 
  mutate(ActivityDay = as.Date(mdy(ActivityDay))) %>%
  arrange(ActivityDay)
daily_intensities <- daily_intensities %>% 
  mutate(ActivityDay = as.Date(mdy(ActivityDay))) %>%
  arrange(ActivityDay)
daily_steps <- daily_steps %>% 
  mutate(ActivityDay = as.Date(mdy(ActivityDay))) %>%
  arrange(ActivityDay)

# process the ActivityHour column (convert dtype & separate date and time)
hourly_calories <- hourly_calories %>% 
  mutate(
    ActivityHour = as_datetime(ActivityHour, format = "%m/%d/%Y %I:%M:%S %p"),
    Date = as.Date(ActivityHour),
    Time = format(ActivityHour, format = "%H:%M:%S")
  )
hourly_intensities <- hourly_intensities %>% 
  mutate(
    ActivityHour = as_datetime(ActivityHour, format = "%m/%d/%Y %I:%M:%S %p"),
    Date = as.Date(ActivityHour),
    Time = format(ActivityHour, format = "%H:%M:%S")
  )
hourly_steps <- hourly_steps %>% 
  mutate(
    ActivityHour = as_datetime(ActivityHour, format = "%m/%d/%Y %I:%M:%S %p"),
    Date = as.Date(ActivityHour),
    Time = format(ActivityHour, format = "%H:%M:%S")
  )

# Analysis: can group by Id and make some plots
# look into detail of each dataset
daily_activity_grouped <- daily_activity %>% group_by(Id)

ggplot(daily_activity_grouped, aes(x = ActivityDate, y = TotalSteps)) + geom_line() +
  facet_wrap(~ Id)
# It looks like not everyone is exercising daily, so let's go head and find out
# how many of the survey participants are skipping days
record_counts <- daily_activity_grouped %>% summarise(Count = n())
# data from 4-12-2016 to 5-12-2016 is included in the dataset
complete_data_threshold <- 31
half_threshold <- 15
# find the number of participants who do not have complete records
count_below_threshold1 <- sum(record_counts$Count < complete_data_threshold)
count_below_threshold2 <- sum(record_counts$Count < half_threshold)
# 12 of participants didn't complete the exercise daily and one of them didn't complete half of the month
# suggestion: add new functions to the product to encourage people to do exercise daily, such as reminders and click-in bonus

cor_matrix_activity <- daily_activity %>% 
  select(TotalDistance,TotalSteps,VeryActiveDistance, VeryActiveMinutes, ModeratelyActiveDistance, FairlyActiveMinutes, Calories) %>% 
  mutate_all(as.numeric) %>% cor()
heatmap(cor_matrix_activity, 
        col = colorRampPalette(c("white", "blue"))(20),
        margins = c(5, 5))
# We can see that VeryActiveMinutes, TotalDistance, and TotalSteps are the three factors and influence Calories the most.

# Go to daily_calories
daily_calories_grouped <- daily_calories %>% group_by(Id)
ggplot(daily_calories_grouped, aes(x = ActivityDay, y = Calories)) + geom_line() +
  facet_wrap(~ Id)
# The majority of user's daily calories maintain at a stable level and decreases in the last few days of the reporting period
# suggestion: some rewarding mechanism can be added to the smart devices to increase user usage during that tough time

# Go to daily_intensities
summary(daily_intensities)
# the VeryActiveMinutes' mean is 21.16, which isn't long while it contributes a lot to Calories. So, stand up and be active to burn your calories!

# Now examine the hourly_ datasets
hourly_calories_grouped <- hourly_calories %>% group_by(Time) %>% 
  summarise(average_calories = mean(Calories))
ggplot(data = hourly_calories_grouped) +
  geom_col(mapping = aes(x = Time, y = average_calories, fill = average_calories)) +
  scale_fill_gradient(low = "blue", high = "orange") +
  labs(title = "Average Hourly Calories In A Typical Day", x = "Time", y = "Calories") +
  theme(axis.text.x = element_text(angle = 90))

hourly_intensities_grouped <- hourly_intensities %>% group_by(Time) %>% 
  summarise(average_intensity = mean(TotalIntensity))
ggplot(data = hourly_intensities_grouped) +
  geom_col(mapping = aes(x = Time, y = average_intensity, fill = average_intensity)) +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Average Hourly Intensities In A Typical Day", x = "Time", y = "Intensities") +
  theme(axis.text.x = element_text(angle = 90))

hourly_steps_grouped <- hourly_steps %>% group_by(Time) %>% 
  summarise(average_step = mean(StepTotal))
ggplot(data = hourly_steps_grouped) +
  geom_col(mapping = aes(x = Time, y = average_step, fill = average_step)) +
  scale_fill_gradient(low = "grey", high = "pink") + 
  labs(title = "Average Hourly Steps In A Typical Day", x = "Time", y = "Steps") +
  theme(axis.text.x = element_text(angle = 90))
