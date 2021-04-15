install.packages("ggplot2") 
install.packages("dplR")
install.packages("tidyr")
install.packages("ggthemes")
install.packages("lubridate")
install.packages("DT")
install.packages("scales") 

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(lubridate)
library(DT)
library(scales)

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
          
ap_data = read.csv(file.choose())
may_data = read.csv(file.choose())                    
june_data = read.csv(file.choose())       
july_data = read.csv(file.choose())
aug_data = read.csv(file.choose())
sep_data = read.csv(file.choose())

# binding all the data from april to september
data_14 = rbind(ap_data,may_data,june_data,july_data,aug_data,sep_data)
str(data_14)

# formatting data according to date and time
data_14$Date.Time = as.POSIXct(data_14$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data_14$Time = format(as.POSIXct(data_14$Date.Time , format = "%m%d%Y%H:%M:%S"), format = "%H:%M:%S")

data_14$Date.Time = ymd_hms(data_14$Date.Time)

#Create factors for Date 
data_14$day = factor(day(data_14$Date.Time))
data_14$month = factor(month(data_14$Date.Time, label = TRUE))
data_14$year = factor(year(data_14$Date.Time))
data_14$dayofweek = factor(wday(data_14$Date.Time, label = TRUE))

# create factors for Time
data_14$hour = factor(hour(hms(data_14$Time)))
data_14$minute = factor(minute(hms(data_14$Time)))
data_14$second = factor(second(hms(data_14$Time)))

#plotting trips by hours in day 
hr_data = data_14 %>% 
  group_by(hour) %>%
  dplyr::summarize(Total =n())
datatable(hr_data)

ggplot(hr_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

month_hour <- data_14 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour , aes(hour,Total, fill = month)) + 
  geom_bar(stat = "identity") +
  ggtitle("Trips Every Hour and Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#plotting data by trips every day of month

day_group = data_14 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

ggplot(day_group , aes(day, Total)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
   ggtitle("Trips Every Day") +
    theme(legend.position = "none") +
     scale_y_continuous(labels = comma)

day_month_group = data_14 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

#No of trips taken in every month of year
month_group <- data_14 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)

ggplot(month_group , aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


month_weekday = data_14 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


day_month_group = data_14 %>%
  group_by(month,day) %>%
  dplyr::summarize(Total =n())

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar(stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

#visualize the number of trips that are taking place each month of the year
month_group = data_14 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n())

ggplot(month_group, aes(month, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

month_weekday <- data_14 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

ggplot(data_14 ,aes(Base)) +
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips By Base")

ggplot(data_14 , aes(Base, fill = month)) +
         geom_bar(position ="dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Months") +
  scale_fill_manual(values = colors)

ggplot(data_14 , aes(Base, fill = dayofweek)) +
  geom_bar(position ="dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayOfWeek") +
  scale_fill_manual(values = colors)

#Creating Heat Map Visualization of Day , Hour and month

day_and_hour = data_14 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total=n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day,hour,fill=Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat map of day and hour")

ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

month_base =data_14 %>%
  group_by(Base , month) %>%
  dplyr::summarize(Total=n())

dayofweek_bases =data_14 %>%
  group_by(Base , dayofweek) %>%
  dplyr::summarize(Total=n())

ggplot(month_base , aes(Base,month,fill=Total)) +
  geom_tile(color="white") +
  ggtitle("Heat map by Month and Bases")

ggplot(dayofweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")

#creating map visualization of rides in new york
#In the final section, we will visualize the rides in New York city by creating a geo-plot that will help us to visualize the rides 
#during 2014 (Apr - Sep) and by the bases in the same period.

min_lat = 40.5774
max_lat = 40.9176
min_long = -74.15
max_long = -73.7004

ggplot(data_14 , aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits = c(min_long, max_long))+
  scale_y_continuous(limits = c(min_lat, max_lat))+
  theme_map()+
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

