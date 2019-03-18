#install.packages(c("dplyr","ggplot2","lubridate","XML"))
library(dplyr)
library(ggplot2)
library(lubridate)
library(XML)
library(xml2)
library(tidyverse)

### Exporting and Transforming the data --------------------------------------

#How to Export Apple Health App Data
#1) Launch the Apple Health App on your iPhone

#2) Tap the Health Data icon in the bottom navigation. 
#This will launch a list of all your Apple Heath data. 
#In the List view, tap the “All” item which is the first item in the list.

###load apple health export.xml file -------------------------------------------
xmlfile <- "C:/temp/apple_health_data/export.xml"
#xml <- xmlParse("C:\\temp\\apple_health_data\\export.xml")
xml <- xmlParse(xml)
xml <- read_xml("C:/temp/watchdata/apple_health_export/export.xml")

#transform xml file to data frame - select the Record rows from the xml file
df <- XML:::xmlAttrsToDataFrame(xml["//Record"])
str(df)

#make value variable numeric
df$value <- as.numeric(as.character(df$value))
str(df)

#make endDate in a date time variable POSIXct using lubridate with eastern time zone
df$endDate <-ymd_hms(df$endDate,tz="America/New_York")
str(df)

##add in year month date dayofweek hour columns
df$month<-format(df$endDate,"%m")
df$year<-format(df$endDate,"%Y")
df$date<-format(df$endDate,"%Y-%m-%d")
df$dayofweek <-wday(df$endDate, label=TRUE, abbr=FALSE)
df$hour <-format(df$endDate,"%H")
str(df)
head(df)

write.csv(df,"C:/temp/AppleWatchData_03182019.csv")
### Working with the data ----------------------------------------------------
#show steps by month by year using dplyr then graph using ggplot2
df %>%
  filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(year,month) %>%
  summarize(steps=sum(value)) %>%
  #print table steps by month by year
  print (n=100) %>%
  #graph data by month by year
  ggplot(aes(x=month, y=steps, fill=year)) + 
  geom_bar(position='dodge', stat='identity') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank())

### Boxplots -------------------------------------------------------------------

# HeartRate By day of week year
df %>%
  filter(df$type == 'HKQuantityTypeIdentifierHeartRate') %>%
  group_by(dayofweek,hour) %>%
  summarize(HR=mean(value)) %>%
  #print (n=100) %>%
  ggplot(aes(x=dayofweek, y=HR)) + 
  geom_boxplot() + 
  #facet_grid(~dayofweek) +
  scale_fill_brewer() +
  theme_bw() +  
  scale_y_log10() +
  theme(panel.grid.major = element_blank())

# By day of week year
df %>%
  filter(df$type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(dayofweek,date,year) %>%
  summarize(steps=sum(value)) %>%
  #print table steps by date by month by year
  print (n=100) %>%
  ggplot(aes(x=dayofweek, y=steps)) + 
  geom_boxplot(aes(fill=(year))) + 
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank())

# By month by year
df %>%
  filter(df$type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(date,month,year) %>%
  summarize(steps=sum(value)) %>%
  #print table steps by date by month by year
  print (n=100) %>%
  ggplot(aes(x=month, y=steps)) + 
  geom_boxplot(aes(fill=(year))) + 
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank())

## #summary statistics----------------------------------------------------------
# By day of week for 2015
df %>%
  filter(df$type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(dayofweek,date,year) %>%
  summarize(steps=sum(value)) %>%
  filter(year==2018) %>%
  group_by(dayofweek) %>%
  summarize(mean = round(mean(steps), 2), sd = round(sd(steps), 2), 
            median = round(median(steps), 2), max = round(max(steps), 2), 
            min = round(min(steps), 2),`25%`= quantile(steps, probs=0.25),
            `75%`= quantile(steps, probs=0.75)) %>%
  arrange(desc(median))

#By month for 2015
df %>%
  filter(df$type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(date,month,year) %>%
  summarize(steps=sum(value)) %>%
  filter(year==2018) %>%
  group_by(month) %>%
  summarize(mean = round(mean(steps), 2), sd = round(sd(steps), 2), 
            median = round(median(steps), 2), max = round(max(steps), 2), 
            min = round(min(steps), 2),`25%`= quantile(steps, probs=0.25),
            `75%`= quantile(steps, probs=0.75))

### Heatmap day of week hour of day --------------------------------------------
df %>%
  filter(df$type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(date,dayofweek,hour) %>% 
  summarize(steps=sum(value)) %>% 
  group_by(hour,dayofweek) %>% 
  summarize(steps=sum(steps)) %>% 
  arrange(desc(steps)) %>%
  #print table steps by date by month by year
  print (n=100) %>%
  ggplot(aes(x=dayofweek, y=hour, fill=steps)) + 
  geom_tile() + 
  scale_fill_continuous(labels = scales::comma, low = 'white', high = 'red') +
  theme_bw() + 
  theme(panel.grid.major = element_blank())
