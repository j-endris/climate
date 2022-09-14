## R code to manipulate and plot cliamte data for my four field sites ##
## aka another hot mess from Joe Endris ##

library(readr)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(emmeans)
library(multcomp)
library(multcompView)
library(HH)

## create objects from datasets##

MI <- read_csv("Michigan.csv" )
IN <- read_csv("Alabama.csv")
AL <- read_csv("Alabama.csv")

mutate