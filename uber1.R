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

colors = c(""#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0"")
           
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
           datatable(day_group))


