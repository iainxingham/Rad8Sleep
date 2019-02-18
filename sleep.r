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

# Single variable plot
g1 <- ggplot(oxi1, aes(Timepoint, SpO2)) + geom_step(colour = "red")
g2 <- ggplot(oxi1, aes(Timepoint, Pulse)) + geom_step(colour = "blue")

# Plot both heart rate and oximetry
plot_colours <- c("SpO2" = "red", "Pulse" = "blue")
g3 <- ggplot(oxi1, aes(x = Timepoint)) + 
  geom_step(aes(y = SpO2, colour = plot_colours["SpO2"])) +
  geom_step(aes(y = Pulse, colour = plot_colours["Pulse"])) +
  labs(y = "Oxygen saturations",
       x = "Time",
       colour = "Signal") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Heart rate (bpm)"))

# Plot just one hour
onehour <- interval(dmy_hm("10.2.2019_21:59"), dmy_hm("10.2.2019_22:59"))
# Alternate form for interval()
hourtwo <- dmy_hm("10.2.2019_22:59") %--% dmy_hm("10.2.2019_23:59")
g4 <- ggplot(filter(oxi1, Timepoint %within% onehour), aes(Timepoint, SpO2)) +
  geom_step(colour = "red")