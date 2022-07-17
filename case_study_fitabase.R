## Install Packages


install.packages("tidyverse")
install.packages("lubridate")
install.packages("shiny")
install.packages("skimr")
install.packages("janitor")

library(tidyverse)
library(lubridate)
library(shiny)
library(skimr)
library(janitor)
library(ggplot2)


## Upload all daily activity data (Activity, Sleep, and Weight), then 
## Normalize "Date" column
## Create unique vector by combining "Id" and "Date"
## Combine into one data set


hr_gps_activity <- read_csv("Data Analyst/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged_NEED.csv") %>% 
  rename(Date=ActivityDate) %>% 
  unite(col='id_date', c("Id", "Date"), sep="_",remove=FALSE)
colnames(hr_gps_activity)
View(hr_gps_activity)

daily_sleep_activity <- read_csv("Data Analyst/Fitabase Data 4.12.16-5.12.16/sleepDay_merged_NEED.csv") %>% 
  rename(Date=SleepDay) %>% 
  unite(col='id_date', c("Id", "Date"), sep="_",remove=FALSE)
colnames(daily_sleep_activity)
View(daily_sleep_activity)

daily_weight_activity <- read_csv("Data Analyst/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged_NEED.csv") %>% 
  unite(col='id_date', c("Id", "Date"), sep="_",remove=FALSE)
colnames(daily_weight_activity)
View(daily_weight_activity)

daily_activity_hr_sleep <- full_join(hr_gps_activity, daily_sleep_activity, by="id_date")
colnames(daily_activity_hr_sleep)
View(daily_activity_hr_sleep)

daily_activity <- full_join(daily_activity_hr_sleep, daily_weight_activity, by="id_date") %>% 
  select(-"Id.y",-"Date.y",-"Id",-"Date") %>% 
  rename(id=Id.x, date=Date.x) %>% 
  clean_names()
daily_activity[is.na(daily_activity)] <-0
colnames(daily_activity)
View(daily_activity)


## Create User Summary


user_summary <- daily_activity %>%
  group_by(id) %>% 
  summarize(total_days = sum(date >= 1), start_date = min(date), end_date = max(date),  
            zero_step_days = sum(total_steps == 0), 
            total_active_days = total_days - zero_step_days, average_steps = mean(total_steps),
            percent_days_active = (1 - zero_step_days / total_days)*100, 
            days_with_sleep = sum(total_sleep_records > 0),
            percent_days_slept = (days_with_sleep/total_days)*100,
            days_weighed = sum(weight_kg > 0), 
            percent_days_weighed = (days_weighed / total_days)*100)
colnames(user_summary)
View(user_summary)

usage_summary <- user_summary %>% 
  summarize(number_of_users = sum(id>0), min_date = min(start_date), max_date = max(end_date),
            min_active_days = min(total_days), max_active_days = max(total_days), 
            total_days = sum(total_days), total_inactive_days = sum(zero_step_days),
            total_active_days = total_days - total_inactive_days, 
            toal_sleep_days = sum(days_with_sleep), total_weight_days = sum(days_weighed)) %>% 
  unite(date_range, c("min_date", "max_date"), sep = " - ") %>% 
  unite(days_used_range, c("min_active_days", "max_active_days"),sep = " - ")
View(usage_summary)

sleep_comparison <- user_summary %>% mutate(user = c(1:33)) %>%  select(-"id")
ggplot(sleep_comparison, aes(x=user, y=days_with_sleep, fill=user)) +
  geom_bar(stat="identity")

weight_comparison <- user_summary %>% mutate(user = c(1:33)) %>%  select(-"id")
ggplot(weight_comparison, aes(x=user, y=days_weighed, fill=user)) +
  geom_bar(stat="identity")

coeff <- 400
user_summary_4 <- user_summary %>% mutate(user = c(1:33)) %>%  select(-"id")
user_summary_4[is.na(user_summary_4)]<-0
ggplot(user_summary_4, aes(x=user, fill=user)) +
  geom_bar(aes(y=total_active_days), stat="identity")+
  geom_line(aes(y=average_steps / coeff, color="yellow"))+
  scale_y_continuous(name="Total Active Days", sec.axis = sec_axis(~.*coeff, name="Average Steps (line graph)"))

## Create activity summary


user_step_summary <- daily_activity %>%
  group_by(id) %>% 
  summarize(average_steps = mean(total_steps), max_steps = max(total_steps), min_steps = min(total_steps),
            start_date = min(date), end_date = max(date), inactive_days = sum(total_steps == 0), 
            total_days = sum(date >= 1))
colnames(user_step_summary)
View(user_step_summary)

