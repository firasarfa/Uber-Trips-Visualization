
##Load Libraries
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyverse)
library(tidyr)
library(DT)
library(scales)
library(lubridate)
library(knitr)
library(rmarkdown)

  
##Read Data for each month 

april <- read.csv("C:\\Users\\Firas\\Documents\\R Projects\\Uber\\uber-raw-data-apr14.csv")
august <- read.csv("C:\\Users\\Firas\\Documents\\R Projects\\Uber\\uber-raw-data-aug14.csv")
july <- read.csv("C:\\Users\\Firas\\Documents\\R Projects\\Uber\\uber-raw-data-jul14.csv")
june <- read.csv("C:\\Users\\Firas\\Documents\\R Projects\\Uber\\uber-raw-data-jun14.csv")
may <- read.csv("C:\\Users\\Firas\\Documents\\R Projects\\Uber\\uber-raw-data-may14.csv")
september <- read.csv("C:\\Users\\Firas\\Documents\\R Projects\\Uber\\uber-raw-data-sep14.csv")
all_data <- rbind(april,may,june,july,august,september)

message("the data size is "  ,dim(all_data))

#####

all_data$Date.Time <- as.POSIXct(all_data$Date.Time, format="%m/%d/%Y %H:%M:%S")
all_data$Time <- format(as.POSIXct(all_data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
all_data$Date.Time <- ymd_hms(all_data$Date.Time)
all_data$day <- factor(day(all_data$Date.Time))
all_data$month <- factor(month(all_data$Date.Time, label=TRUE))
all_data$year <- factor(year(all_data$Date.Time))
all_data$dayofweek <- factor(wday(all_data$Date.Time, label=TRUE))
all_data$second = factor(second(hms(all_data$Time)))
all_data$minute = factor(minute(hms(all_data$Time)))
all_data$hour = factor(hour(hms(all_data$Time)))
print(all_data)



hourly_data <- all_data %>% 
  group_by(hour) %>% 
  dplyr::summarize(Total = n())

ggplot(hourly_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="steelblue", 
           color="red") + 
  ggtitle("Trips Every Hour", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)

#####

day_data <- all_data %>% group_by(day) %>% dplyr::summarize(Trips = n())

ggplot(day_data, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Trips by day of the month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)

######
month_data <- all_data %>% group_by(month) %>% dplyr::summarize(Total = n())
ggplot(month_data, aes(month, Total, fill = month)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Trips in a month") 


######

wday_data <- all_data %>% group_by(dayofweek) %>% dplyr::summarize(Trips = n())
ggplot(wday_data, aes(dayofweek, Trips)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Trips by day of the month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)

####

day_month_data <- all_data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())
ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by Day and Month")

######


