
# Install and load packages
install.packages('tidyverse')
install.packages('skimr')
install.packages('janitor')
library(tidyverse)
library(skimr)
library(janitor)


# read csv and make a dataset of daily activity. take a look what information it contains
dailyActivity_merged <- read_csv("archive/dailyActivity_merged.csv")
dailyCalories_merged <- read_csv("archive/dailyCalories_merged.csv")
dailyIntensities_merged <- read_csv("archive/dailyIntensities_merged.csv")
dailySteps_merged <- read_csv("archive/dailySteps_merged.csv")

skim_without_charts(dailyActivity_merged)
head(dailyActivity_merged)
skim_without_charts(dailyCalories_merged)
skim_without_charts(dailyIntensities_merged)
skim_without_charts(dailySteps_merged)
# It looks like the activity dataset contains the other three data set.
# steps, distance, minutes in categories, calories, Id
# I would like to know relationships between steps and distance 
dis_step <-dailyActivity_merged %>% 
  group_by(Id) %>% 
  summarise(TotalSteps_by_Id = sum(TotalSteps), TotalDistance_by_Id = sum(TotalDistance),TotalCalories_by_Id = sum(Calories))
dis_step
ggplot(dis_step) +
  geom_point(mapping = aes(x = TotalSteps_by_Id, y = TotalDistance_by_Id))

#It looks like positive relationship and do step and distance related to calories also?

ggplot(dis_step) +
  geom_point(mapping = aes(x = TotalSteps_by_Id, y = TotalCalories_by_Id))

ggplot(dis_step) +
  geom_point(mapping = aes(x = TotalDistance_by_Id, y = TotalCalories_by_Id))

# It looks like positive relationship, but some Id has high calories burns with low distance and steps.
# my hyphothesis here, is daily intensities might effect higher calorie burn.
cal_min <- dailyActivity_merged %>% 
  group_by(Id) %>% 
  summarise(total_very_active_minutes = sum(VeryActiveMinutes),total_fairly_active_minutes = sum(FairlyActiveMinutes), total_lightlyactive_minutes = sum(LightlyActiveMinutes), total_sedentary_minutes = sum(SedentaryMinutes), total_calories = sum(Calories)) 
cal_min
# now i think of it, i do not need to use ID for calories and intensity comparison.

clean_names(dailyActivity_merged)

ggplot(dailyActivity_merged)+
  geom_smooth(mapping = aes(x = VeryActiveMinutes, y = Calories))

ggplot(dailyActivity_merged)+
  geom_smooth(mapping = aes(x = FairlyActiveMinutes, y = Calories))

ggplot(dailyActivity_merged)+
  geom_smooth(mapping = aes(x = LightlyActiveMinutes, y = Calories))

ggplot(dailyActivity_merged)+
  geom_smooth(mapping = aes(x = SedentaryMinutes, y = Calories))

# i can not see a clear relationship intensity and calories but it tends to show longher very active minute raise calory burnings.

# now i bring more data into R

heartrate_seconds_merged <- read_csv("archive/heartrate_seconds_merged.csv")
hourlyCalories_merged <- read_csv("archive/hourlyCalories_merged.csv")
hourlyIntensities_merged <- read_csv("archive/hourlyIntensities_merged.csv")
hourlySteps_merged <- read_csv("archive/hourlySteps_merged.csv")
sleepDay_merged <- read_csv("archive/sleepDay_merged.csv")
weightLogInfo_merged <- read_csv("archive/weightLogInfo_merged.csv")

# Id~workout hour, sleep hours ~ date of workout, Weight~calories
# compare intensity hour and calories by Id. make a new data frame.
View(hourlyIntensities_merged)
View(hourlyCalories_merged)
hourly_cal_Int_merged <- hourlyCalories_merged %>% mutate(hourlyIntensities_merged)
View(hourly_cal_Int_merged)
head(hourly_cal_Int_merged)
ggplot(hourly_cal_Int_merged)+
  geom_smooth(mapping = aes(x=TotalIntensity, y = Calories))
# It show positive relationship which tell high intensity bring high calories. this means that if we can keep users stay on high intensity workout which burn more calories.
# weight lose and calories
View(weightLogInfo_merged)
# find wight changes

weight_max_min <-weightLogInfo_merged %>% 
  group_by(Id) %>% 
  summarise(max_weight = max(WeightKg), min_weight = min(WeightKg))

weight_change <-weight_max_min %>% 
  mutate("weight_change" = max_weight-min_weight)

total_calories_by_Id <- dailyActivity_merged %>% 
  group_by(Id) %>% 
  summarise(total_calories = sum(Calories))
View(total_calories_by_Id)
Weight_change_calories <- weight_change %>% 
  mutate(total_calories_by_Id)

# now i realized that not all of users weighted themselves and i cannot deliver relationship of calories and change change.


# sleep minutes ~ Calories burns

calories_sleep <- dailyActivity_merged %>% 
  mutate("sleep_time" = sleepDay_merged)

total_calories_by_Id  
head(total_calories_by_Id)
total_sleephour_by_Id <- sleepDay_merged %>% 
  group_by(Id) %>% 
  summarise(sum(TotalMinutesAsleep))
View(total_sleephour_by_Id)
# again, the size is different.if there is a way to mix these into one by matching Id, that would be great.

install.packages('rmarkdown')
library(rmarkdown)
