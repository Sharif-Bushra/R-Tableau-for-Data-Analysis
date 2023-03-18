#load libraries

library(tidyverse) #data wrangling
library(lubridate) #dates-times
library(hms) #durations or time-of-day
library(data.table) #aggregation of large data
library(dplyr) #data manipulation
library(skimr) #summary statistics
library(janitor) #Cleaning

setwd("~/Google Data Analytics/Capstone Project/Datasets/.CSV")

#load original .csv files, data for one year from September 2021 to August 2022
sep_21 <- read_csv("202109-divvy-tripdata.csv")
oct_21 <- read_csv("202110-divvy-tripdata.csv")
nov_21 <- read_csv("202111-divvy-tripdata.csv")
dec_21 <- read_csv("202112-divvy-tripdata.csv")
jan_22 <- read_csv("202201-divvy-tripdata.csv")
feb_22 <- read_csv("202202-divvy-tripdata.csv")
mar_22 <- read_csv("202203-divvy-tripdata.csv")
apr_22 <- read_csv("202204-divvy-tripdata.csv")
may_22 <- read_csv("202205-divvy-tripdata.csv")
jun_22 <- read_csv("202206-divvy-tripdata.csv")
jul_22 <- read_csv("202207-divvy-tripdata.csv")
aug_22 <- read_csv("202208-divvy-tripdata.csv")

colnames(sep_21) #check column names
colnames(0ct_21)
colnames(nov_21)
colnames(dec_21)
colnames(jan_22)
colnames(feb_22)
colnames(mar_22)
colnames(apr_22)
colnames(may_22)
colnames(jun_22)
colnames(jul_22)
colnames(aug_22)

str(sep_21) #inspect data frames
str(0ct_21)
str(nov_21)
str(dec_21)
str(jan_22)
str(feb_22)
str(mar_22)
str(apr_22)
str(may_22)
str(jun_22)
str(jul_22)
str(aug_22)

compare_df_cols(sep_21,oct_21,nov_21,dec_21,jan_22,feb_22,mar_22,apr_22,may_22,jun_22,jul_22,aug_22, return = "mismatch") #double check column datatypes


Cyclistic <- rbind(sep_21,oct_21,nov_21,dec_21,jan_22,feb_22,mar_22,apr_22,may_22,jun_22,jul_22,aug_22) #stack all data frames into one data frame

remove(sep_21,oct_21,nov_21,dec_21,jan_22,feb_22,mar_22,apr_22,may_22,jun_22,jul_22,aug_22) #remove individual data frames


cyclistic$ride_duration <- difftime(cyclistic$ended_at, cyclistic$started_at, units = "mins") #calculate ride duration in minutes, in ride_duration column

cyclistic$date <- as.Date(cyclistic$started_at) #create a column for date only,default format is yyyy-mm-dd

cyclistic$month_year <- format(as.Date(cyclistic$date), "%b-%y") #create a column for month-year

cyclistic$day_of_week <- format(as.Date(cyclistic$date), "%A") #create a column for day of week

cyclistic$time <- as_hms(cyclistic$started_at) #create a column for time only

cyclistic$hour <- hour(cyclistic$time) #create a column for hour

cyclistic <-cyclistic %>% mutate(time_of_day = case_when(hour == "0" ~ "Night",
                                             		 hour == "1" ~ "Night",
                                             		 hour == "2" ~ "Night",
                                             		 hour == "3" ~ "Night",
                                             		 hour == "4" ~ "Night",
                                             		 hour == "5" ~ "Night",
                                             		 hour == "6" ~ "Morning",
                                             		 hour == "7" ~ "Morning",
                                             		 hour == "8" ~ "Morning",
                                             		 hour == "9" ~ "Morning",
                                             		 hour == "10" ~ "Morning",
                                             		 hour == "11" ~ "Morning",
                                             		 hour == "12" ~ "Afternoon",
                                             		 hour == "13" ~ "Afternoon",
                                             		 hour == "14" ~ "Afternoon",
                                             		 hour == "15" ~ "Afternoon",
                                             		 hour == "16" ~ "Afternoon",
                                             		 hour == "17" ~ "Afternoon",
                                             		 hour == "18" ~ "Evening",
                                             		 hour == "19" ~ "Evening",
                                             		 hour == "20" ~ "Evening",
                                             		 hour == "21" ~ "Evening",
                                             		 hour == "22" ~ "Evening",
                                             		 hour == "23" ~ "Evening")
) #create a column for time of the day

cyclistic %>% skim_without_charts() #check for missing values, whitespaces and other variables

colSums(is.na(cyclistic)) #check for N/A values per column

table(cyclistic$rideable_type) #view column contents, found out there is only three types of bikes

table(cyclistic$member_casual) #view column contents, found out there is only two types of customers

cyclistic <- distinct(cyclistic) #remove duplicates

cyclistic <- cyclistic %>% select(-c(started_at,ended_at,start_station_id,end_station_id,start_lat,start_lng,end_lat,end_lng)) #remove unnecessary columns

cyclistic <- cyclistic %>% rename(Bike_type=rideable_type, Customer_type=member_casual) #rename two cloumns

cyclistic <- cyclistic %>% filter(ride_duration >= 1 & ride_duration <= 1440) #filter out rides lasting less than one minute and more than 1440 minutes(1 day)

cyclistic$ride_duration <- as.numeric(cyclistic$ride_duration) #change variable type to numeric
  
cyclistic$day_of_week <- ordered(cyclistic$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                         "Friday", "Saturday", "Sunday"))    #order day of the week

cyclistic$month_year <- ordered(cyclistic$month_year, levels=c("Sep-21", "Oct-21", "Nov-21", "Dec-21", 
                                                                 "Jan-22", "Feb-22", "Mar-22", "Apr-22", "May-22", "Jun-22", "Jul-22", "Aug-22")) #order month

View(cyclistic) #view the data frame

write.csv(cyclistic,"[...]\\Google Data Analytics\\Capstone Project\\Datasets\\.CSV\\cyclistic.csv", row.names = FALSE) #export the data frame
