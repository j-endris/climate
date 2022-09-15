## R code to manipulate and plot cliamte data for my four field sites ##
## aka another hot mess from Joe Endris ##

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

## create objects from datasets##

MI <- read_csv("Michigan.csv")
IN <- read_csv("Alabama.csv")
AL <- read_csv("Alabama.csv")
TN <- read_csv("Tennessee.csv")

str(AL)  #view structure of data ##

## create column for julian date##
## trying to replicate https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r##
AL_julian <- mutate(AL, Julian=format("%j"))


## monthly mean low temp ##
## update this after creating julian dates ##
AL %>%
  group_by(month) %>%
  summarise(total = mean(value))

## create graph for temps by month of year ##

AL %>%
  ggplot(aes(x = DATE, y = TMIN)) +
  geom_point(color = "grey") +
  labs(title = "Daily Low Temperatures",
       subtitle = "Tuscaloosa, AL",
       y= "Daily Low Temperature (Celcius)",
       x= "Date") + theme_bw(base_size = 15)

  
