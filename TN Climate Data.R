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

TN <- read_csv("Tennessee.csv")

str(TN)  #view structure of data ##

## create column for julian date##
## trying to replicate https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r##
TN <- mutate(TN, Julian=format(DATE,"%j"))

#omit NA in temperature recordings 
TN<-TN[complete.cases(TN[,4]),]

## monthly mean low temp ##
## update this after creating julian dates ##
TN_TMAX <- TN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = max(TMAX))

## create graph for temps by month of year ##

TN_TMAX %>%
  filter(year>1980) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Highest Temperatures",
       subtitle = "Clarksville, TN",
       y= "Daily High Temperature (Celcius)",
       x= "Year") + theme_bw(base_size = 15)


#number of days above 32.2
TN_32.2 <- TN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMAX>32.2))

#plot number of days above 32.2
TN_32.2 %>%
  filter(as.integer(year)>1980)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days >32.2°C",
       subtitle = "Clarksville, TN",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


TN_32.2 %>%
  filter(n>0)%>%
  filter(year>1960)
