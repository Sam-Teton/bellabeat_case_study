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

used_databases <- c("FitBit_Fitness_Tracker_Data", "dailyActivity_merged.csv", "sleepDay_merged.csv", "weightLogInfo_merged.csv")
data_source <- c("https://www.kaggle.com/datasets/arashnic/fitbit", "FitBit_Fitness_Tracker_Data", "FitBit_Fitness_Tracker_Data", "FitBit_Fitness_Tracker_Data")
file_type <- c("URL", ".csv", ".csv", ".csv")
author <- c("Mobius", "Mobius", "Mobius", "Mobius")
last_year_updated <- c(2020, 2020, 2020, 2020)

Data_Source_Summary <- data_frame(used_databases, data_source, file_type, author, last_year_updated)
head(Data_Source_Summary)

## Upload all daily activity data (Activity, Sleep, and Weight), then 
## Normalize "Date" column
## Create unique vector by combining "Id" and "Date"
## Combine into one data set

install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(janitor)

activity_file <- read_csv("Data Analyst/Coursera_case_study/FitBit_Data_1/dailyActivity_merged_NEED.csv") %>% 
  rename(Date = ActivityDate) %>% 
  unite(col='id_date', c("Id", "Date"), sep="_",remove=FALSE) %>%  clean_names()
sleep_activity <- read_csv("Data Analyst/Coursera_case_study/FitBit_Data_1/sleepDay_merged_NEED.csv") %>% 
  rename(Date = SleepDay) %>% 
  unite(col='id_date', c("Id", "Date"), sep="_",remove=FALSE) %>%  clean_names()
weight_activity <- read_csv("Data Analyst/Coursera_case_study/FitBit_Data_1/weightLogInfo_merged_NEED.csv") %>% 
  unite(col='id_date', c("Id", "Date"), sep="_",remove=FALSE) %>%  clean_names()
sleep_and_activity <- full_join(activity_file, sleep_activity, by="id_date") %>% 
  select(-"id.y", -"date.y")
daily_activity <- full_join(sleep_and_activity, weight_activity, by="id_date") %>% 
  select(-"id", -"date", -"id_date") %>%  rename(id = "id.x", date = "date.x") %>%  
  clean_names() 

View(daily_activity)


## Create User Summary

daily_activity[is.na(daily_activity)] <- 0
user_summary <- daily_activity  %>% 
  group_by(id) %>% 
  summarize(total_days = sum(date >= 1), start_date = min(date), end_date = max(date),  
            zero_step_days = sum(total_steps == 0), 
            zero_calories_days = sum(calories == 0),
            all_sedentary_days = sum(sedentary_minutes == 1440),
            total_active_days = total_days - zero_step_days, average_steps = mean(total_steps),
            percent_days_active = (1 - zero_step_days / total_days)*100, 
            days_with_sleep = sum(total_sleep_records >= 1),
            percent_days_slept = (days_with_sleep/total_days)*100,
            days_weighed = sum(weight_kg >= 1), 
            percent_days_weighed = (days_weighed / total_days)*100)
tibble(user_summary) %>% select(-"start_date", -"end_date", -"zero_step_days", 
                                -"percent_days_active",-"percent_days_slept", 
                                -"percent_days_weighed")

usage_summary <- user_summary %>% 
  summarize(number_of_users = sum(id>0), min_date = min(start_date), max_date = max(end_date),
            min_active_days = min(total_days), max_active_days = max(total_days), 
            total_days = sum(total_days), zero_step_days = sum(zero_step_days),
            zero_calorie_days = sum(zero_calories_days), 
            total_sedentary_days = sum(all_sedentary_days),
            total_sleep_days = sum(days_with_sleep), total_weight_days = sum(days_weighed)) %>% 
  unite(date_range, c("min_date", "max_date"), sep = " - ") %>% 
  unite(days_used_range, c("min_active_days", "max_active_days"),sep = " - ")
tibble(gather(usage_summary))


summary(daily_activity)
library(scales)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
step_intergity <- daily_activity %>% mutate(step_level = if_else(total_steps > 10000, "more_than_10k_steps",
                                                                 if_else(total_steps >= 5000 & total_steps <= 10000, "from_5k_to_10k_steps",
                                                                         if_else(total_steps >= 1 & total_steps < 5000, "from_1_to_5k_steps", "zero_step_days")))) 

short_step <- step_intergity %>% group_by(step_level) %>% 
  summarize(days=sum(id>0))


ggplot(short_step, aes(x="", y=days, fill=step_level))+
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) + blank_theme +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = c(740, 500, 160, 0), 
                label = percent(days/871)), size=4) +
  labs(title="Distrubtion of Daily Step Level", 
       subtitle= "Out of 871 daily entries, the step average = 8,285") +
  theme(plot.title = element_text(hjust = 0.5))

date_step <- step_intergity %>% group_by(date) %>% summarize (total_users=sum(id>0), total_step=sum(total_steps), avg_user_daily_steps=(total_step/total_users))

ggplot(step_intergity, aes(x=date, y=total_steps, group=id))+
  geom_line(stat="identity") +
  theme(axis.text.x = element_blank())+
  facet_wrap(~id) +
  labs(title="Individual Daily Step Count Trend") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(date_step, aes(x=date, y=avg_user_daily_steps, group=date))+
  geom_bar(stat="identity", fill="light blue")+
  labs(title="Average Daily Step Count Trend") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=90))

n <- daily_activity %>% filter(lightly_active_minutes>0) %>% 
  summarize(mean(lightly_active_minutes))
head(n)
minute_intergity <- daily_activity %>% 
  summarize(very_active_days = sum(very_active_minutes > "37.5"),
            fairly_active_days = sum(very_active_minutes <= "37.5" & 
                                       fairly_active_minutes > "23"),
            lightly_active_days = sum(very_active_minutes <= "37.5" & 
                                        fairly_active_minutes <= "23"& 
                                        lightly_active_minutes > "212"),
            less_than_average_activity = (sum(very_active_minutes <= "37.5" & 
                                                fairly_active_minutes <= "23"& 
                                                lightly_active_minutes <= "212")  
                                          - sum(total_steps ==0)), 
            no_activity_days = sum(total_steps == 0), expected_total=sum(id>0), total= 
              (very_active_days + fairly_active_days + lightly_active_days + less_than_average_activity + no_activity_days))
View(minute_intergity)

minute_intergity <- daily_activity %>%  
  mutate(activity_level = if_else(very_active_minutes > "40", "very_active_day", 
                                  if_else(very_active_minutes <= "40" & fairly_active_minutes > "23", 
                                          "fairly_active_day", 
                                          if_else(very_active_minutes <= "40" & fairly_active_minutes <= "23" & 
                                                    lightly_active_minutes > "212", "lightly_active_days", 
                                                  if_else(very_active_minutes <= "40" & fairly_active_minutes <= "23" & 
                                                            lightly_active_minutes <= "212" & sedentary_minutes != "1440",                                                          "less_than_average_activity", "no_activity_days")))))



ggplot(minute_intergity, aes(x="", y= activity_level, fill=activity_level))+
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) 

calories_intergity <- daily_activity %>% 
  summarize(more_than_3k_calories = sum(calories > 3000), 
            from_2k_to_3k_calories = sum(calories >= 2000 & calories <= 3000),
            from_1_to_2k_calories = sum(calories >= 1 & calories < 2000),
            zero_calories_days = sum(calories == 0), expected_total=sum(id>0), total= 
              more_than_3k_calories + from_2k_to_3k_calories + from_1_to_2k_calories + zero_calories_days)
View(sleep_intergity)

daily_activity[is.na(daily_activity)] <- 0
sleep_intergity <- daily_activity %>% 
  summarize(more_than_9_hours = sum((total_minutes_asleep)/60 > "9"), 
            from_7_to_9_hours = sum((total_minutes_asleep)/60 > "7" & 
                                      (total_minutes_asleep)/60 <= "9"),
            from_5_to_7_hours = sum((total_minutes_asleep)/60 > "5" & 
                                      (total_minutes_asleep)/60 <= "7"),
            from_1_to_5_hours = sum((total_minutes_asleep)/60 > "0" & 
                                      (total_minutes_asleep)/60 <= "5"),
            no_sleep_days = sum(total_minutes_asleep == 0), 
            expected_total=sum(id>0), total= 
              more_than_9_hours + from_7_to_9_hours + from_5_to_7_hours + 
              from_1_to_5_hours + no_sleep_days)
View(sleep_intergity)

daily_activity[is.na(daily_activity)] <- 0
weight_intergity <- daily_activity %>% 
  summarize(days_with_weight = sum(weight_kg>0),
            no_weight_days = sum(weight_kg == 0), 
            expected_total=sum(id>0), total= 
              days_with_weight + no_weight_days)
View(weight_intergity)

daily_activity <- daily_activity %>% filter(calories>0)
v <- daily_activity %>% filter(calories == 0)
View(v)

v <- daily_activity %>% filter(total_steps == 0)
View(v)

## Create activity summary

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

user_step_summary <- daily_activity %>%
  group_by(id) %>% 
  summarize(average_steps = mean(total_steps), max_steps = max(total_steps), min_steps = min(total_steps),
            start_date = min(date), end_date = max(date), inactive_days = sum(total_steps == 0), 
            total_days = sum(date >= 1))
colnames(user_step_summary)
View(user_step_summary)


sed_day <- daily_activity %>% filter(Date < "2016-05-12") %>% 
  group_by(day) %>% summarize(sed_min=sum(sedentary_minutes), users=sum(id>0))
ggplot(sed_day, aes(x=day, y=(sed_min/users)))+
  geom_bar(stat="identity", fill="deeppink4") +
  labs(title="Average Sedentary Minutes per User per Day") + 
  labs(subtitle= "Users are least sedentary on the weekends") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


date_cal <- calories_intergity %>% filter(Date != "2016-05-12") %>% group_by(Date) %>% summarize (total_users=sum(id>0), total_cal=sum(calories), avg_user_daily_cal=(total_cal/total_users))
ggplot(date_cal, aes(x=Date, y=avg_user_daily_cal))+
  geom_bar(stat="identity", fill="chocolate3")+
  labs(title="Average Daily Calorie Burn") +
  labs(subtitle = "Daily calories burned are fairly consistent") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method="loess", formula=y~x, color="black")+
  theme(plot.subtitle = element_text(hjust = 0.5))



sleep_days <- daily_activity %>% group_by(day) %>% summarize(days=sum(total_sleep_records>0))
ggplot(sleep_days, aes(x="", y=days, fill=day))+
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) + blank_theme +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.text.x = element_blank()) +
  labs(title="Distrubtion of Sleep Records by Day") + 
  labs(subtitle= "Sleep records are fairly evenly distributed by day of the week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))




#RMD CODE STARTS HERE
##### *Install Packages* 
install.packages("tidyverse",repos = "http://cran.us.r-project.org")
library(tidyverse)
install.packages("janitor",repos = "http://cran.us.r-project.org")
library(janitor)


##### *Upload Datasets, Normalize Naming, Clean Formatting, and Combine into one Master Dataframe*
dailyActivity_merged_NEED.csv <- "C:/Users/sjmin/OneDrive/Documents/Data Analyst/Coursera_case_study/FitBit_Data_1/dailyActivity_merged_NEED.csv"
sleepDay_merged_NEED.csv <- "C:/Users/sjmin/OneDrive/Documents/Data Analyst/Coursera_case_study/FitBit_Data_1/sleepDay_merged_NEED.csv"
weightLogInfo_merged_NEED.csv <- "C:/Users/sjmin/OneDrive/Documents/Data Analyst/Coursera_case_study/FitBit_Data_1/weightLogInfo_merged_NEED.csv"
activity_file <- read_csv(dailyActivity_merged_NEED.csv) %>% rename(Date = ActivityDate) %>% 
  unite(col='id_date', c("Id", "Date"), sep="_",remove=FALSE) %>%  clean_names()
sleep_activity <- read_csv(sleepDay_merged_NEED.csv) %>% rename(Date = SleepDay) %>% 
  unite(col='id_date', c("Id", "Date"), sep="_",remove=FALSE) %>%  clean_names()
weight_activity <- read_csv(weightLogInfo_merged_NEED.csv) %>% 
  unite(col='id_date', c("Id", "Date"), sep="_",remove=FALSE) %>%  clean_names()
sleep_and_activity <- full_join(activity_file, sleep_activity, by="id_date") %>% select(-"id.y", -"date.y")
daily_activity <- full_join(sleep_and_activity, weight_activity, by="id_date") %>% select(-"id", -"date", -"id_date") %>% rename(id = "id.x", date = "date.x") %>% clean_names()%>% mutate(Date=as.Date(date, "%m/%d/%Y")) %>% mutate(day=format(Date, format="%A")) %>% select(-"date") %>% relocate(Date, .after=id)
daily_activity$day <- factor(daily_activity$day, levels =c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))




##### *Identify Abnormalities, Check Data Integrity, Remove Invalid Entries*  
daily_activity <- daily_activity %>% filter(calories>0, total_steps>0 | sedentary_minutes<1440)
daily_activity[is.na(daily_activity)] <- 0
user_summary <- daily_activity  %>% 
  group_by(id) %>% 
  summarize(total_days = sum(Date >= 1), start_date = min(Date), end_date = max(Date),  
            zero_step_days = sum(total_steps == 0), 
            zero_calories_days = sum(calories == 0),
            all_sedentary_days = sum(sedentary_minutes == 1440),
            total_active_days = total_days - (zero_step_days+zero_calories_days+
                                                all_sedentary_days), 
            average_steps = mean(total_steps), very_to_fair_active_min = 
              mean((very_active_minutes+fairly_active_minutes)),
            percent_days_active = (1 - zero_step_days / total_days)*100, 
            days_with_sleep = sum(total_sleep_records >= 1),
            percent_days_slept = (days_with_sleep/total_days)*100,
            days_weighed = sum(weight_kg >= 1), 
            percent_days_weighed = (days_weighed / total_days)*100,
            step_count = sum(total_steps), tot_very_to_fair_active_min = 
              sum((very_active_minutes+fairly_active_minutes)), 
            total_miles = sum(total_distance))
usage_summary <- user_summary %>% 
  summarize(number_of_users = sum(id>0), min_date = min(start_date), max_date = max(end_date),
            min_active_days = min(total_days), max_active_days = max(total_days), 
            total_days = sum(total_days), zero_step_days = sum(zero_step_days),
            zero_calorie_days = sum(zero_calories_days), 
            complete_sedentary_days = sum(all_sedentary_days),
            total_sleep_days = sum(days_with_sleep), total_weight_days = sum(days_weighed)) %>% 
  unite(date_range, c("min_date", "max_date"), sep = " - ") %>% 
  unite(days_used_range, c("min_active_days", "max_active_days"),sep = " - ")
tibble(gather(usage_summary))








##### *Daily Step Analysis* 
library(scales)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
step_intergity <- daily_activity %>% mutate(step_level = if_else(total_steps > 10000, "more_than_10k_steps",
                                                                 if_else(total_steps >= 5000, "from_5k_to_10k_steps",
                                                                         if_else(total_steps >= 1, "from_1_to_5k_steps", "zero_step_days")))) 

short_step <- step_intergity %>% group_by(step_level) %>% 
  summarize(days=sum(id>0))
short_step$step_level <- factor(short_step$step_level, levels =c("zero_step_days", "from_1_to_5k_steps", "from_5k_to_10k_steps", "more_than_10k_steps"))
ggplot(short_step, aes(x="", y=days, fill=step_level))+
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) + blank_theme +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = c(740, 500, 160, 0), 
                label = percent(days/871)), size=4) +
  labs(title="Distrubtion of Daily Step Count", 
       subtitle= "Out of 871 daily entries, the step count averages = 8,285") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

date_step <- step_intergity %>% filter(Date != "2016-05-12") %>% group_by(Date) %>% summarize (total_users=sum(id>0), total_step=sum(total_steps), avg_user_daily_steps=(total_step/total_users))
ggplot(date_step, aes(x=Date, y=avg_user_daily_steps))+
  geom_bar(stat="identity", fill="chocolate3")+
  labs(title="Average Daily Step Count") +
  labs(subtitle = "Daily step count trended up for the first two week, but then leveled out") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method="loess", formula=y~x, color="black")+
  theme(plot.subtitle = element_text(hjust = 0.5))

day_step <- step_intergity %>% group_by(day) %>% summarize (total_users=sum(id>0), total_mile=sum(total_distance)+sum(logged_activities_distance), avg_miles=(total_mile/total_users))
ggplot(day_step, aes(x=day, y=avg_miles, fill=day))+
  geom_bar(stat="identity", fill="darkolivegreen4")+
  labs(title="Average Miles by Day") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="The least distance is walked on Fridays and Sundays") +
  theme(plot.subtitle = element_text(hjust = 0.5))


d_step <- step_intergity %>% mutate(over_10k = if_else(total_steps>10000, 1, 0)) %>%
  group_by(day) %>% summarize (total_users=sum(id>0), total_step_10000=sum(over_10k), percent_over_10k = (total_step_10000/total_users)*100)
ggplot(d_step, aes(x=day, y=percent_over_10k, fill=day))+
  geom_bar(stat="identity", fill="darkslateblue")+
  labs(title="Percent of Users With 10,000+ Steps by Day") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="Tuesday is the most likely day to walk 10,000+ steps") +
  theme(plot.subtitle = element_text(hjust = 0.5))






##### *Activity Analysis* 
act_min <- daily_activity  %>% 
  summarize(high_active_min=sum(very_active_minutes), fair_active_min=sum(fairly_active_minutes), low_active_min=sum(lightly_active_minutes)) %>% 
  gather(key="activity_type", value="min")
act_min$activity_type <- factor(act_min$activity_type, levels =c("low_active_min", "fair_active_min", "high_active_min"))
ggplot(act_min, aes(x="", y= min, fill= activity_type))+
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) + blank_theme +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = c(10000, 28000, 150000), 
                label = percent(min/sum(min))), size=4) +
  labs(title="Distrubtion of Active Minutes") + 
  labs(subtitle= "The vast majority of active minutes are low intensity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

act_min_date <- daily_activity %>% filter(Date < "2016-05-12") %>% group_by(Date) %>% 
  summarize(high_min=sum(very_active_minutes), fair_min=sum(fairly_active_minutes), 
            low_min=sum(lightly_active_minutes), users=sum(id>0))
ggplot(act_min_date, aes(x= Date, y= (high_min/users)))+
  geom_bar(stat="identity", fill="chocolate3") +
  labs(title="High Intensity Active Minutes per User") + 
  labs(subtitle= "The amount of high intensity activity minutes per day trends down throughout the month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_smooth(method=lm, formula=y~x, color="black")

high_day <- daily_activity %>% filter(Date < "2016-05-12") %>% 
  mutate(act= if_else((very_active_minutes)>0, 1, 0))%>% 
  group_by(day) %>% 
  summarize(act_day=sum(act), na_days=sum(act==0), users=sum(id>0))
ggplot(high_day, aes(x=day, y=(act_day/users)*100))+
  geom_bar(stat="identity", fill="deepskyblue4") +
  labs(title="Percent Users With High Intensity Activity by Day") + 
  labs(subtitle= "The chance of high intensity activities decreases throughout the week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

act_min_date <- daily_activity %>% filter(Date < "2016-05-12") %>% group_by(day) %>% 
  summarize(high_min=sum(very_active_minutes), fair_min=sum(fairly_active_minutes), 
            low_min=sum(lightly_active_minutes), users=sum(id>0))
ggplot(act_min_date, aes(x= day, y= (high_min/users)))+
  geom_bar(stat="identity", fill="darkolivegreen4") +
  labs(title="High Intensity Active Minutes per User by Day") + 
  labs(subtitle= "The amount of high intensity activity minutes per user per day trends down throughout the week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

na_day <- daily_activity %>% filter(Date < "2016-05-12") %>% 
  mutate(act= if_else((very_active_minutes + fairly_active_minutes)>0, 1, 0)) %>% 
  group_by(day) %>% 
  summarize(act_day=sum(act), na_days=sum(act==0), users=sum(id>0))
ggplot(na_day, aes(x=day, y=((na_days/users)*100)))+
  geom_bar(stat="identity", fill="darkslateblue") +
  labs(title="Percent Users With No High or Fair Intensity Activity Minutes") + 
  labs(subtitle= "There is a lower chance of high intensity activity Friday, Saturday, and Sunday") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))






##### *Calorie Burn Analysis* 
calories_intergity <- daily_activity %>% mutate(calorie_level = 
                                                  if_else(calories >= 3000, "over_3k_calories",
                                                          if_else(calories>= 2500, "from_2.5k_to_3k_calories",
                                                                  if_else(calories >= 2000, "from_2k_to_2.5k_calories" ,"from_1_to_2k_calories"))))

short_cal <- calories_intergity %>% group_by(calorie_level) %>% 
  summarize(days=sum(id>0))
short_cal$calorie_level <- factor(short_cal$calorie_level, levels =c("from_1_to_2k_calories", "from_2k_to_2.5k_calories", "from_2.5k_to_3k_calories", "over_3k_calories"))
ggplot(short_cal, aes(x="", y=days, fill=calorie_level))+
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) + blank_theme +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = c(730, 470, 270, 90), 
                label = percent(days/871)), size=4) +
  labs(title="Distrubtion of Daily Calorie Burn") + 
  labs(subtitle= "Out of 871 daily entries, the calorie burn average = 2,357") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

day_cal <- calories_intergity %>% group_by(day) %>% summarize (total_users=sum(id>0), total_cal=sum(calories), avg_cal=(total_cal/total_users))
ggplot(day_cal, aes(x=day, y=avg_cal, fill=day))+
  geom_bar(stat="identity", fill="darkolivegreen4")+
  labs(title="Average Calorie Burn by Day") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="Daily calories burned are fairly consistent") +
  theme(plot.subtitle = element_text(hjust = 0.5))

d_cal <- calories_intergity %>% mutate(over_2.5k = if_else(calories>2500, 1, 0)) %>%
  group_by(day) %>% summarize (total_users=sum(id>0), total_2.5k =sum(over_2.5k), percent_over_2.5k = (total_2.5k/total_users)*100)
ggplot(d_cal, aes(x=day, y=percent_over_2.5k, fill=day))+
  geom_bar(stat="identity", fill="darkslateblue")+
  labs(title="Percent of Users With 2,500+ Calories Burned by Day") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="Tuesdays have the highest percent of users with 2,500+ calories burned") +
  theme(plot.subtitle = element_text(hjust = 0.5))










##### *Sleep Analysis* 
daily_activity[is.na(daily_activity)] <- 0
sleep_intergity <- daily_activity %>% filter(total_minutes_asleep>0) %>% mutate(sleep_hours = 
                                                                                  if_else((total_minutes_asleep)/60 > 9, "more_than_9_hours", 
                                                                                          if_else((total_minutes_asleep)/60 > 7, "from_7_to_9_hours",
                                                                                                  if_else((total_minutes_asleep)/60 > 5, "from_5_to_7_hours", "from_1_to_5_hours"))))
short_sleep <- sleep_intergity %>% group_by(sleep_hours) %>% 
  summarize(days=sum(id>0))
short_sleep$sleep_hours <- factor(short_sleep$sleep_hours, levels =c("from_1_to_5_hours", "from_5_to_7_hours", "from_7_to_9_hours", "more_than_9_hours"))
ggplot(short_sleep, aes(x="", y=days, fill=sleep_hours))+
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) + blank_theme +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = c(385, 280, 130, 25), 
                label = percent(days/413)), size=4) +
  labs(title="Distrubtion of Average Sleep Length") + 
  labs(subtitle= "Out of 413 entries, the average sleep lasts 7 hours") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

date_sleep <- sleep_intergity %>% filter(Date != "2016-05-12") %>% group_by(Date) %>% summarize(total_users=sum(id>0), total_sleep=(sum(total_minutes_asleep)/60), avg_user_daily_sleep=(total_sleep/total_users))
ggplot(date_sleep, aes(x=Date, y=avg_user_daily_sleep))+
  geom_bar(stat="identity", fill="chocolate3")+
  labs(title="Average Daily Sleep Length") +
  labs(subtitle = "Daily sleep length average varies between 6-8 hours") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method="loess", formula=y~x, color="black")+
  theme(plot.subtitle = element_text(hjust = 0.5))

day_sleep <- sleep_intergity %>% group_by(day) %>% summarize (total_users=sum(id>0), total_sleep=(sum(total_minutes_asleep)/60), avg_sleep=(total_sleep/total_users))
ggplot(day_sleep, aes(x=day, y=avg_sleep, fill=day))+
  geom_bar(stat="identity", fill="darkolivegreen4")+
  labs(title="Average Sleep Length by Day") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="Users average the most sleep on Sundays") +
  theme(plot.subtitle = element_text(hjust = 0.5))

d_sleep <- sleep_intergity %>% mutate(over_8h = if_else(total_minutes_asleep>480, 1, 0)) %>%
  group_by(day) %>% summarize (total_users=sum(id>0), total_8h =sum(over_8h), percent_over_8h = (total_8h/total_users)*100)
ggplot(d_sleep, aes(x=day, y=percent_over_8h, fill=day))+
  geom_bar(stat="identity", fill="darkslateblue")+
  labs(title="Percent of Users With 8+ Hours of Sleep by Day") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="Weekend entries had the highest chance of recording more than 8 hours of sleep") +
  theme(plot.subtitle = element_text(hjust = 0.5))









# Extra analysis

s <- daily_activity %>% filter(total_minutes_asleep>0)
ggplot(s, aes(x= total_steps, y= (total_minutes_asleep/60))) +
  geom_point(color="darkred") +
  geom_smooth(method="lm", formula="y~x", color="black")+
  labs(title="Steps vs Sleep") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="Days with higher steps resulted in less sleep") +
  theme(plot.subtitle = element_text(hjust = 0.5))


st <- daily_activity %>% filter(total_steps>0)
ggplot(st, aes(x= total_steps, y= calories)) +
  geom_point(color="darkslateblue") +
  geom_smooth(method="lm", formula="y~x", color="black")+
  xlim(0,20000)+
  labs(title="Steps vs Calories") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="There is a positive relationship between steps and calories burned") +
  theme(plot.subtitle = element_text(hjust = 0.5))

a <- daily_activity 
ggplot(a, aes(x= (very_active_minutes+fairly_active_minutes), y= calories)) +
  geom_point(color="darkolivegreen4") +
  geom_smooth(method="lm", formula="y~x", color="black")+
  xlim(0,300)+
  labs(title="High to Moderate Level Activity vs Calories") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="Performing at a high to moderate level burns more calories than low level activites") +
  theme(plot.subtitle = element_text(hjust = 0.5))

a <- daily_activity 
ggplot(a, aes(x= (lightly_active_minutes), y= calories)) +
  geom_point(color="chocolate3") +
  geom_smooth(method="lm", formula="y~x", color="black")+
  xlim(0,300)+
  labs(title="Low Level Activity vs Calories") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="Low level activities burn calories, but at a lower rate") +
  theme(plot.subtitle = element_text(hjust = 0.5))

ggplot(daily_activity, aes(x= (very_active_minutes+fairly_active_minutes), y=total_steps)) +
  geom_point(color="darkolivegreen4")+
  geom_smooth(method="lm", formula="y~x", color="black")+
  xlim(0,300)+
  labs(title="High to Moderate Level Activity vs Steps") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="High to moderate activity increases steps faster than low level activity") +
  theme(plot.subtitle = element_text(hjust = 0.5))

ggplot(daily_activity, aes(x=lightly_active_minutes, y=total_steps)) +
  geom_point(color="chocolate3")+
  geom_smooth(method="lm", formula="y~x", color="black") +
  xlim(0,300)+
  labs(title="Low Level Activity vs Steps") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(subtitle="Low level activites increase steps slowly") +
  theme(plot.subtitle = element_text(hjust = 0.5))


