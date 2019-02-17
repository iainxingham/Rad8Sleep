# Take pulse and oximetry data from Rad-8 and produce a sleep study

library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data

oxi <- read.csv("./Data/file.csv", 
                header = FALSE,
                skip = 1,
                stringsAsFactors = FALSE)

oxi1 <- oxi %>%
  mutate(Date = mdy(V1)) %>%
  filter(! is.na(Date)) %>%
  unite("T3", c("V1", "V2")) %>%
  extract(V3, c("T1"), "((?<!SpO)[[:digit:]]{1,3})") %>%
  extract(V4, c("T2"), "([[:digit:]]{1,3})") %>%
  extract(V7, c("Exception"), "([[:xdigit:]]{4})") %>%
  transmute(Timepoint = mdy_hms(T3) + hours(7) + minutes(2), # time offset specific for particular data file
            SpO2 = as.integer(T1),
            Pulse = as.integer(T2),
            Exception = Exception)

g1 <- ggplot(oxi1, aes(Timepoint, SpO2)) + geom_step(colour = "red")