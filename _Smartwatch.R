library("dplyr")
library("janitor")
library("skimr")
library("here")
library("tidyverse")
library("patchwork")
library("ggthemes")


################################################################################
######################## DATA INTEGRATION ######################################
################################################################################

mydata = read_csv("dailyActivity_merged.csv")
sleep_data = read_csv("sleepDay_merged.csv")
cal = read_csv("hourlyCalories_merged.csv")
heart_rates = read_csv("heartrate_seconds_merged.csv")
steps = read_csv("hourlySteps_merged.csv")
int = read_csv("hourlyIntensities_merged.csv")

# activity
mydata$ActivityDate=as.POSIXct(mydata$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
# sleep
sleep_data$SleepDay=as.POSIXct(sleep_data$SleepDay, format="%m/%d/%Y", tz=Sys.timezone())

mydata = mydata %>% 
  rename(date = ActivityDate)

sleep_data = sleep_data %>% 
  rename(date = SleepDay)

merged_data = merge(mydata, sleep_data, by = c("Id", "date"))

################################################################################
############ DATA QUALITY ASSESSMENT & DATA CLEANING ###########################
################################################################################

View(merged_data)
ncol(merged_data)
head(merged_data)
skim_without_charts(merged_data)
glimpse(merged_data)
str(merged_data)
dim(merged_data)

# how many participants / how many unique IDs
n_distinct(merged_data$Id)

# cheking for null values
is.null(merged_data)
sum(is.na(merged_data))

# checking for duplicates
sum(duplicated(merged_data))

# deleting duplicates
merged_data = merged_data %>% 
  distinct() %>% 
  drop_na()

# changing column names to lowercase
colnames(merged_data)
merged_data <- rename_with(merged_data, tolower)
colnames(merged_data)

################################################################################ 
########################## DATA TRANSFORMATION #################################
################################################################################ 

# sum total number of activity
merged_data = merged_data %>% 
  mutate(totalactivity = veryactiveminutes + lightlyactiveminutes + fairlyactiveminutes, 
         sleep_hours = round(totalminutesasleep/60, 1))

# categorize users activity per day and their sleep
merged_data <- merged_data %>%
  mutate(user_activity_day = case_when(
    .$totalsteps < 5000 ~ "Sedentary",
    .$totalsteps >= 5000 & .$totalsteps < 7499 ~ "Low active",
    .$totalsteps >= 7500 & .$totalsteps < 9999 ~ "Somewhat active",
    .$totalsteps >= 10000 & .$totalsteps < 12499 ~ "Active",
    .$totalsteps >= 12500 ~ "Very active"
  ), sleepquality = case_when(
    .$totalminutesasleep/60 >= 6 & .$totalminutesasleep/60 <= 8 ~ "Sufficient",
    .$totalminutesasleep/60 < 6 ~ "Insufficient",
    .$totalminutesasleep/60 > 8 ~ "Oversleeping"
  ))

# adding a new column -> weekday
merged_data <- merged_data %>% 
  mutate(weekday = weekdays(date))

merged_data$weekday <- ordered(merged_data$weekday, 
                                levels = c("Monday", "Tuesday", 
                                           "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# how long it takes for each individual to fall asleep (meaning the difference between total minutes in bed and actual sleep minutes)
merged_data = merged_data %>% 
  mutate(minutes_till_sleep = totaltimeinbed - totalminutesasleep)


################################################################################
############## DATA EXPLORATION AND DESCRIPTIVE STATISTICS #####################
################################################################################

merged_data %>% 
  select(totalsteps, totaldistance, veryactiveminutes, fairlyactiveminutes, lightlyactiveminutes, 
         sedentaryminutes, calories, totalminutesasleep, totaltimeinbed, sleep_hours, minutes_till_sleep) %>% 
  summary()


################## Steps #######################################################

total_steps_summary = merged_data %>%
  group_by(id) %>% 
  summarize(mean_total_steps = mean(totalsteps), max(totalsteps), min(totalsteps)) %>% 
  arrange(mean_total_steps)
total_steps_summary

# steps vs total distance
ggplot(data=merged_data) + geom_smooth(mapping = aes(x = totalsteps, y = totaldistance)) + 
  geom_point(mapping = aes(x = totalsteps, y = totaldistance), color = "darkblue") + 
  labs(title = "Steps Vs Total Distance") +xlab("total steps") + ylab("total distance")

# steps vs very active minutes
s0 = ggplot(data=merged_data) + geom_smooth(mapping = aes(x = totalsteps, y = veryactiveminutes)) + 
  geom_point(mapping = aes(x = totalsteps, y = veryactiveminutes), color = "darkblue") + 
  labs(title = "Steps Vs Very Active Minutes") +xlab("total steps") + ylab("very actvie minutes")

# steps vs fairly active minutes
s1 = ggplot(data=merged_data) + geom_smooth(mapping = aes(x = totalsteps, y = fairlyactiveminutes)) + 
  geom_point(mapping = aes(x = totalsteps, y = fairlyactiveminutes), color = "darkblue") + 
  labs(title = "Steps Vs Fairly Active Minutes") +xlab("total steps") + ylab("Fairly actvie minutes")

# steps vs lightly active minutes
s2 = ggplot(data=merged_data) + geom_smooth(mapping = aes(x = totalsteps, y = lightlyactiveminutes)) + 
  geom_point(mapping = aes(x = totalsteps, y = lightlyactiveminutes), color = "darkblue") + 
  labs(title = "Steps Vs Ligthly Active Minutes") +xlab("total steps") + ylab("ligthly actvie minutes")

# steps vs sedentary minutes
s3 = ggplot(data=merged_data) + geom_smooth(mapping = aes(x = totalsteps, y = sedentaryminutes)) + 
  geom_point(mapping = aes(x = totalsteps, y = sedentaryminutes), color = "darkblue") + 
  labs(title = "Steps Vs Sedentary Minutes") +xlab("total steps") + ylab("sedentary minutes")

# steps vs calories
s4 = ggplot(data=merged_data) + geom_smooth(mapping = aes(x = totalsteps, y = calories)) + 
  geom_point(mapping = aes(x = totalsteps, y = calories), color = "darkblue") + 
  labs(title = "Steps Vs Calories") +xlab("total steps") + ylab("calories")

# steps vs total activity
s5 = ggplot(data=merged_data) + geom_smooth(mapping = aes(x = totalsteps, y = totalactivity)) + 
  geom_point(mapping = aes(x = totalsteps, y = totalactivity), color = "darkblue") + 
  labs(title = "Steps Vs Total Activity") +xlab("total steps") + ylab("total activity")

s0 + s1 +s2 + s3 +s4 + s5 

################### Calories ###################################################

# very active minutes vs calories
p1 = ggplot(data=merged_data) + geom_smooth(mapping = aes(x = veryactiveminutes, y = calories)) + 
  geom_point(mapping = aes(x = veryactiveminutes, y = calories), color = "darkblue") + 
  labs(title = "Very active minutes Vs calories") +xlab("very active minutes") + ylab("calories")

# fairlyactiveminutes vs calories
p2 = ggplot(data=merged_data) + geom_smooth(mapping = aes(x = fairlyactiveminutes, y = calories)) + 
  geom_point(mapping = aes(x = fairlyactiveminutes, y = calories), color = "darkblue") + 
  labs(title = "Fairly active minutes Vs calories") +xlab("fairly active minutes") + ylab("calories")

# lightlyactiveminutes vs calories
p3 = ggplot(data=merged_data) + geom_smooth(mapping = aes(x = lightlyactiveminutes, y = calories)) + 
  geom_point(mapping = aes(x = lightlyactiveminutes, y = calories), color = "darkblue") + 
  labs(title = "Ligthly active minutes Vs calories") +xlab("Ligthly active minutes") + ylab("calories")

# sedentaryminutes vs calories
p4 = ggplot(data=merged_data) + geom_smooth(mapping = aes(x = sedentaryminutes, y = calories)) + 
  geom_point(mapping = aes(x = sedentaryminutes, y = calories), color = "darkblue") + 
  labs(title = "Sedentary minutes Vs calories") +xlab("sedentary minutes") + ylab("calories")


cal$ActivityHour=as.POSIXct(cal$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
cal = cal %>% 
  separate(ActivityHour, into = c("Date", "Time"), sep = " ")

pl1 = cal %>% 
  group_by(Time) %>% 
  summarise(mean_calories = mean(Calories)) %>% 
  ggplot() + geom_col(mapping = aes(x = Time, y = mean_calories), fill = "darkblue") + 
  theme(axis.text.x=element_text(angle=45))

pl1

(p1 | p2 | p3 | p4)/
  pl1


################### Sleep ######################################################

### sleep quality vs user's activity
a1 <- ggplot(data = merged_data) +
  geom_bar(mapping = aes(x = user_activity_day, fill = as.factor(user_activity_day))) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(title = "User Activity Distribution")


a2 = ggplot(data = merged_data) + 
  geom_col(mapping = aes(x = sleepquality, y = user_activity_day, fill = sleepquality)) +
  facet_grid(~user_activity_day) +
  labs(title = "User's activity vs Sleep Quality") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_blank()
  ) +
  scale_fill_viridis_d()

a1+a2

### sleep quality vs weekday

ggplot(data = merged_data) + geom_col(mapping = aes(x = sleepquality, y = weekday, fill = sleepquality)) +
  facet_grid(~weekday) + labs(title = "User's activity vs Sleep Quality") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_fill_viridis_d()


somn = merged_data %>% 
  group_by(id) %>% 
  summarise(mean_sleep_hours = mean(sleep_hours))

ggplot(data = somn) + geom_col(mapping = aes(x = as.character(id), y = mean_sleep_hours, 
                                             fill = mean_sleep_hours)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  geom_hline(yintercept = (mean(somn$mean_sleep_hours))) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# sleep hours vs total steps
xx = ggplot(data = merged_data) + geom_point(mapping = aes(x = sleep_hours, y = totalsteps), color = "darkblue") + 
  geom_smooth(mapping = aes(x = sleep_hours, y = totalsteps))
# sleep hours vs calories
xy = ggplot(data = merged_data) + geom_point(mapping = aes(x = sleep_hours, y = calories), color = "darkblue") + 
  geom_smooth(mapping = aes(x = sleep_hours, y = calories))
# sleep hours vs total activity
xz = ggplot(data = merged_data) + geom_point(mapping = aes(x = sleep_hours, y = totalactivity), color = "darkblue") + 
  geom_smooth(mapping = aes(x = sleep_hours, y = totalactivity))

xx + xy + xz


######################### Weekly ###############################################
#### week summary
week_summary <- merged_data %>% 
  group_by(weekday) %>% 
  summarise(mean_sleep = mean(totalminutesasleep), mean_steeps = mean(totalsteps), 
            mean_calories = mean(calories), mean_activity = mean(totalactivity))
week_summary

b1 = ggplot(data = merged_data) + 
  geom_col(mapping = aes(x = user_activity_day, y = weekday, fill = user_activity_day)) +
  facet_grid(~weekday) +
  labs(title = "User's activity vs Weekday") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_blank()
  ) +
  scale_fill_viridis_d()

b2 = merged_data%>%
  ggplot(aes(weekday,fill=user_activity_day)) +
  geom_bar(stat = "count", alpha = 0.6, position="fill", show.legend = T) +
  labs(title= "Fig. 7: User activity by weekday", subtitle= "...by level of activity", x ="Day of week", y= "Percentage of users") +
  scale_fill_viridis_d()


b1/
b2

plot_one = ggplot(data = week_summary) + geom_col(mapping = aes(x = weekday, y = mean_sleep), fill = "#006699") + 
  theme(axis.text.x = element_text(angle=45, vjust = 0.5))
plot_two = ggplot(data = week_summary) + geom_col(mapping = aes(x = weekday, y = mean_steeps), fill = "#85e0e0") + 
  theme(axis.text.x = element_text(angle=45, vjust = 0.5))
plot_three = ggplot(data = week_summary) + geom_col(mapping = aes(x = weekday, y = mean_calories), fill = "cyan3") + 
  theme(axis.text.x = element_text(angle=45, vjust = 0.5))

plot_one + plot_two +plot_three 

######################### Daily ################################################

steps$ActivityHour=as.POSIXct(steps$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
steps = steps %>% 
  separate(ActivityHour, into = c("Date", "Time"), sep = " ")

int$ActivityHour=as.POSIXct(int$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
int = int %>% 
  separate(ActivityHour, into = c("Date", "Time"), sep = " ")

pl2 = int %>% 
  group_by(Time) %>% 
  summarise(mean_intensity = mean(TotalIntensity)) %>% 
  ggplot() + geom_col(mapping = aes(x = Time, y = mean_intensity), fill = "#FF6666") + 
  theme(axis.text.x=element_text(angle=45))
pl2

pl3 = steps %>% 
  group_by(Time) %>% 
  summarise(mean_steps = mean(StepTotal)) %>% 
  ggplot() + geom_col(mapping = aes(x = Time, y = mean_steps), fill = "#FF6666") + 
  theme(axis.text.x=element_text(angle=45))


(pl1 | pl3) / 
  pl2

######################### Users ################################################

# average number of calories per user + mean activity
merged_data %>% 
  group_by(id) %>% 
  summarise(mean_calories = mean(calories), sd(calories), mean_activity = mean(totalactivity), 
            sd_activity = sd(totalactivity)) %>% 
  arrange(mean_calories)


cor(merged_data$totalsteps, merged_data$calories)

## 147 occurrences of people not having any very active exercise

length(which(merged_data$veryactivedistance == 0.00))

## 135 occurrences of people not having any moderately active exercise
length(which(merged_data$moderatelyactivedistance == 0.00))

## 0 occurrences of people not having any light active exercise
length(which(merged_data$lightactivedistance == 0.00))


#visualizing the distribution of users by hours slept daily 
merged_data %>%
  ggplot(aes(x=sleep_hours, fill=..count..)) +
  geom_histogram(alpha=.8, binwidth=.25) +
  scale_fill_gradient(low="green", high="red") +
  labs(title="Fig. 4: Distribution of users...", subtitle= "...by Total Hours of Sleep", x ="Total Hours of Sleep", y= "Count of users") +
  geom_vline(xintercept=mean(merged_data$sleep_hours), color="black", size=1) +
  annotate("text", x=mean(merged_data$sleep_hours)-0.5, y=10, label="average = 6.96 h", color="black", size=5, angle=90)
summary(merged_data$sleep_hours)

#visualizing the distribution of users by steps taken daily
merged_data %>%
  ggplot(aes(x=totalsteps, fill=..count..)) +
  geom_histogram(alpha=.8, binwidth=1000) +
  scale_fill_gradient(low="grey", high="blue") +
  labs(title="Fig. 5: Distribution of users...", subtitle= "...by Total Daily Steps", x ="Total Steps per day", y= "Count of users") +
  geom_vline(xintercept=mean(merged_data$totalsteps), color="black", size=1) +
  annotate("text", x=mean(merged_data$totalsteps)-1400, y=35, label="average = 8515 steps", color="black", size=5, angle=90)
summary(merged_data$totalsteps)

#visualizing the distribution of users by calories burned daily
merged_data %>%
  ggplot(aes(x=calories, fill=..count..)) +
  geom_histogram(alpha=.8, binwidth=200) +
  scale_fill_gradient(low="grey", high="darkorange") +
  labs(title="Fig. 6: Distribution of users...", subtitle= "...by Calories burned daily", x ="Total Calories per day", y= "Count of users") +
  geom_vline(xintercept=mean(merged_data$calories), color="black", size=1) +
  annotate("text", x=mean(merged_data$calories)+180, y=110, label="average = 2389 kcal", color="black", size=5, angle=0)
summary(merged_data$calories)


################# Hear rate ####################################################

View(heart_rates)

mean(heart_rates$Value)
max(heart_rates$Value)
min(heart_rates$Value)

n_distinct(heart_rates$Id)

heart_rates$Time=as.POSIXct(heart_rates$Time, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())

new_heart = heart_rates[c(1:2000), c(1,2,3)]
View(new_heart)
#de facut "the most and least active hours of the day" cred ca merge barplot
ggplot(data = new_heart) + geom_line(mapping = aes(x = Time, y = Value, color = "red")) +
  labs(title = "Heart rate")


####################################################################################
########################## 
cal = read_csv("hourlyCalories_merged.csv")

cal$ActivityHour=as.POSIXct(cal$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
cal = cal %>% 
  separate(ActivityHour, into = c("Date", "Time"), sep = " ")

pl1 = cal %>% 
  group_by(Time) %>% 
  summarise(mean_calories = mean(Calories)) %>% 
  ggplot() + geom_col(mapping = aes(x = Time, y = mean_calories), fill = "darkblue") + 
  theme(axis.text.x=element_text(angle=45))




#######################################################################################
#######################################################################################
#######################################################################################




