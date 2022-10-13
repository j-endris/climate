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

MI <- read_csv("Indiana.csv")

str(MI)  #view structure of data ##

## create column for julian date##
## trying to replicate https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r##
MI <- mutate(MI, Julian=format(DATE,"%j"))

#omit NA in temperature recordings 
MI<-MI[complete.cases(MI[,7]),]

## monthly mean low temp ##
## update this after creating julian dates ##
MI_TMAX <- MI %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = max(TMAX))

## create graph for temps by month of year ##

MI_TMAX %>%
  filter(year>1980) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Highest Temperatures",
       subtitle = "Chelsea, MI",
       y= "Daily High Temperature (Celcius)",
       x= "Year") + theme_bw(base_size = 15)


#number of days above 37.7
MI_37.7 <- MI %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMAX>37.7))

#plot number of days above 37.7
MI_37.7 %>%
  filter(as.integer(year)>1980)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days >37.7",
       subtitle = "Chelsea, MI",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


MI_37.7 %>%
  filter(n>0)%>%
  filter(year>1960)
