## R code to manipulate and plot climate data for my three field sites ##
## Code from Joe Joe Endris ##

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)

##temp<-read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/Climate Data/Alabama.csv")
setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/")

## create objects from datasets##
AL <- read_csv("Alabama.csv")
TN <- read_csv("Tennessee.csv")
IN <- read_csv("Indiana.csv")

##################################
####Alabama segment starts here###
##################################

str(AL)  #view structure of data ##

## create column for julian date##
AL <- mutate(AL, julian_date=format(DATE,"%j"))

#omit NA in temperature recordings 
AL<-AL[complete.cases(AL[,5]),]

#calculate last day below freezing for each year
AL_last <- AL%>%
  dplyr::group_by(year=lubridate::floor_date(DATE, "day")) %>%
  dplyr::last (TMIN, 0)

## monthly absolute low temp ##
## update this after creating julian dates ##
AL_TMIN <- AL %>%
  group_by(month=lubridate::floor_date(DATE, "month")) %>%
  summarise(temp = min(TMIN))

## create graph for temps by month of year ##
AL_TMIN %>%
  filter(year>1980) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "black") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Lowest Temperatures",
       subtitle = "Tuscaloosa, AL",
       y= "Temperature (Celcius)",
       x= "Year") + theme_bw(base_size = 15)

#Number of Days Below -2
AL_freeze <- AL %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMIN < 0))

#plot Number of Days Below zero
AL_freeze %>%
  filter(as.integer(year)>1980)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days Below Zero",
       subtitle = "Tuscaloosa, AL",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


AL_freeze %>%
  filter(n>0)%>%
  filter(year>1980)

#Calculate mean temperature by Julian date
AL_mean <- AL %>%
  group_by(julian_date) %>%
  summarise(mean_low = mean(TMIN))

AL_mean$julian_date = as.numeric(as.character(AL_mean$julian_date))

#plot mean temperature by julian date
AL_mean_plot <- ggplot(AL_mean, aes(x= julian_date, y=mean_low))+
                         xlim(1,135)+
                         geom_point(color = "grey") +
                         geom_smooth(method="lm")+
                         labs(title = "Mean Low Temperture by Julian Date",
                              subtitle = "Tuscaloosa, AL",
                              y= "Temperature (C)",
                              x= "Julian Date") + theme_bw(base_size = 15)

AL_mean_plot
##################################
####Tennessee segment starts here###
##################################

str(TN)  #view structure of data ##

## create column for julian date##
TN <- mutate(TN, julian_date=format(DATE,"%j"))

#omit NA in temperature recordings 
TN<-TN[complete.cases(TN[,5]),]

#calculate last day below freezing for each year
TN_last <- TN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  mutate(last = day(min(which(TMIN<=0))))
  
## monthly mean low temp ##
## update this after creating julian dates ##
TN_TMIN <- TN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = min(TMIN))

## create graph for temps by month of year ##
TN_TMIN %>%
  filter(year>1980-01-01) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Lowest Temperatures",
       subtitle = "Clarksville, TN",
       y= "Daily High Temperature (Celcius)",
       x= "Year") + theme_bw(base_size = 15)

TN_TMIN %>%
  filter(year>"1940")

#Number of days below zero
TN_freeze <- TN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMIN<0))

#plot number of days below zero
TN_freeze %>%
  filter(as.integer(year)>1980)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days Below Freezing",
       subtitle = "Clarksville, TN",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


TN_freeze %>%
  filter(n>0)%>%
  filter(year>1980)

##################################
####Indiana segment starts here###
##################################

str(IN)  #view structure of data ##

## create column for julian date##
## trying to replicate https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r##
IN <- mutate(IN, julian_date=format(DATE,"%j"))

#omit NA in temperature recordings 
IN<-IN[complete.cases(IN[,5]),]

## monthly mean low temp ##
## update this after creating julian dates ##
IN_TMIN <- IN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = min(TMIN))

## create graph for temps by month of year ##

IN_TMIN %>%
  filter(year>1980) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Lowest Temperatures",
       subtitle = "Hoosier National Forest, IN",
       y= "Daily High Temperature (Celcius)",
       x= "Year") + theme_bw(base_size = 15)


#number of days below zero
IN_freeze <- IN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMIN<0))

#plot Number of Days Below -2
IN_freeze %>%
  filter(as.integer(year)>1980)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days Below -2C",
       subtitle = "Hoosier National Forest, IN",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


IN_freeze %>%
  filter(n>0)%>%
  filter(year>1980)

#Issues to correct
#how to account for/correct leap days in Julian date count
#Line 32 - calculation for last freeze of the year
