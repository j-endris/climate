## R code to manipulate and plot cliamte data for my four field sites ##
## aka another hot mess from Joe Endris ##

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

## create objects from datasets##
##temp<-read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/Climate Data/Alabama.csv")
setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/")

AL <- read_csv("Alabama.csv")
TN <- read_csv("Tennessee.csv")
IN <- read_csv("Indiana.csv")
MI <- read_csv("Michigan.csv")

str(AL)  #view structure of data ##

## create column for julian date##
## trying to replicate https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r##
AL <- mutate(AL, Julian=format(DATE,"%j"))

#omit NA in temperature recordings 
AL<-AL[complete.cases(AL[,5]),]

## monthly mean low temp ##
## update this after creating julian dates ##
AL_TMIN <- AL %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = min(TMIN))

## create graph for temps by month of year ##

AL_TMIN %>%
  filter(year>1950) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Lowest Temperatures",
       subtitle = "Tuscaloosa, AL",
       y= "Daily Low Temperature (Celcius)",
       x= "Year") + theme_bw(base_size = 15)


#Number of Days Below -2
AL_freeze <- AL %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMIN < -2))

#plot Number of Days Below -2
AL_freeze %>%
  filter(as.integer(year)>1950)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days Below -2C",
       subtitle = "Tuscaloosa, AL",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


AL_freeze %>%
  filter(n>0)%>%
  filter(year>1960)

##################################
####Tennessee segment starts here###
##################################

str(TN)  #view structure of data ##

## create column for julian date##
## trying to replicate https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r##
TN <- mutate(TN, Julian=format(DATE,"%j"))

#omit NA in temperature recordings 
TN<-TN[complete.cases(TN[,5]),]

## monthly mean low temp ##
## update this after creating julian dates ##
TN_TMIN <- TN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = min(TMIN))

## create graph for temps by month of year ##

TN_TMIN %>%
  filter(year>1950-01-01) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Lowest Temperatures",
       subtitle = "Clarksville, TN",
       y= "Daily High Temperature (Celcius)",
       x= "Year") + theme_bw(base_size = 15)

TN_TMIN %>%
  filter(year>"1940")

#Number of Days Below -2
TN_freeze <- TN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMIN<-2))

#plot Number of Days Below -2
TN_freeze %>%
  filter(as.integer(year)>1950)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days Below -2C",
       subtitle = "Clarksville, TN",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


TN_freeze %>%
  filter(n>0)%>%
  filter(year>1960)

##################################
####Indiana segment starts here###
##################################

str(IN)  #view structure of data ##

## create column for julian date##
## trying to replicate https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r##
IN <- mutate(IN, Julian=format(DATE,"%j"))

#omit NA in temperature recordings 
IN<-IN[complete.cases(IN[,5]),]

## monthly mean low temp ##
## update this after creating julian dates ##
IN_TMIN <- IN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = min(TMIN))

## create graph for temps by month of year ##

IN_TMIN %>%
  filter(year>1950) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Lowest Temperatures",
       subtitle = "Hoosier National Forest, IN",
       y= "Daily High Temperature (Celcius)",
       x= "Year") + theme_bw(base_size = 15)


#number of days below -2
IN_freeze <- IN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMIN<-2))

#plot Number of Days Below -2
IN_freeze %>%
  filter(as.integer(year)>1950)%>%
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
  filter(year>1960)

##################################
####Michigan segment starts here###
##################################

str(MI)  #view structure of data ##

## create column for julian date##
## trying to replicate https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r##
MI <- mutate(MI, Julian=format(DATE,"%j"))

#omit NA in temperature recordings 
MI<-MI[complete.cases(MI[,5]),]

## monthly mean low temp ##
## update this after creating julian dates ##
MI_TMIN <- MI %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = min(TMIN))

## create graph for temps by month of year ##

MI_TMIN %>%
  filter(year>"1950-01-01") %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Lowest Temperatures",
       subtitle = "Chelsea, MI",
       y= "Daily High Temperature (Celcius)",
       x= "Year") + theme_bw(base_size = 15)

filter(MI_TMIN,year>"1950-01-01")
#Number of Days Below -2
MI_freeze <- MI %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMIN<-2))

#plot Number of Days Below -2
MI_freeze %>%
  filter(as.integer(year)>1950)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days Below -2C",
       subtitle = "Chelsea, MI",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


MI_freeze %>%
  filter(n>0)%>%
  filter(year>1960)
