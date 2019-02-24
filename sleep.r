# Take pulse and oximetry data from Rad-8 and produce a sleep study

library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(purrr)

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
            Exception = Exception) %>%
  distinct()

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

# Split data into hour blocks
study_period <- range(oxi1$Timepoint)
seq(min(oxi1$Timepoint), max(oxi1$Timepoint), by="hours")
t1 <- seq(floor_date(study_period[1], "hour"),
          ceiling_date(study_period[2], "hour"),
          by = "hours")
t2 <- int_diff(t1)

# Plot single hour SpO2 and pulse
g5 <- ggplot(filter(oxi1, Timepoint %within% t2[2]), aes(Timepoint, SpO2)) +
  geom_step(colour = "red") +
  labs(y = "Oxygen saturations", x = "Time")
g6 <- ggplot(filter(oxi1, Timepoint %within% t2[2]), aes(Timepoint, Pulse)) +
  geom_step(colour = "blue") +
  labs(y = "Heart rate", x = "Time")
plot_grid(g5, g6, ncol = 1)

# Plot first and last hours with scales
# t2[1] for first, t2[13] for last
g7 <- ggplot(filter(oxi1, Timepoint %within% t2[13]), aes(Timepoint, SpO2)) +
  geom_step(colour = "red") +
  labs(y = "Oxygen saturations", x = "Time") +
  xlim(int_start(t2[13]), int_end(t2[13])) + 
  ylim(70, 100)
g8 <- ggplot(filter(oxi1, Timepoint %within% t2[13]), aes(Timepoint, Pulse)) +
  geom_step(colour = "blue") +
  labs(y = "Heart rate", x = "Time") +
  xlim(int_start(t2[13]), int_end(t2[13])) +
  ylim(40, 170)
plot_grid(g7, g8, ncol = 1)

# Create full hourly plots

# Function for single hour plot

single_hour_plot <- function (oxi_data, xstart, xend, hr_range=c(50,150)) {
  g1 <- ggplot(oxi_data, aes(Timepoint, SpO2)) +
    geom_step(colour = "red") +
    labs(y = "Oxygen saturations", x = "Time") +
    xlim(xstart, xend) + 
    ylim(70, 100)
  g2 <- ggplot(oxi_data, aes(Timepoint, Pulse)) +
    geom_step(colour = "blue") +
    labs(y = "Heart rate", x = "Time") +
    xlim(xstart, xend) +
    ylim(hr_range[1], hr_range[2])
  return(plot_grid(g1, g2, ncol = 1))
}

# single_hour_plot rewritten to take an interval as first arguement to allow use of map()
single_hour_plot <- function (time_int, oxi_data, oxi_range=c(70,100), hr_range=c(50,150)) {
  oxi1 <- filter(oxi_data, Timepoint %within% time_int)
  
  g1 <- ggplot(oxi1, aes(Timepoint, SpO2)) +
    geom_step(colour = "red") +
    labs(y = "Oxygen saturations", x = "Time") +
    xlim(int_start(time_int), int_end(time_int)) + 
    ylim(oxi_range[1], oxi_range[2]) +
    theme(axis.title = element_text(size = 8),
          axis.text = element_text(size = 8))
  
  g2 <- ggplot(oxi1, aes(Timepoint, Pulse)) +
    geom_step(colour = "blue") +
    labs(y = "Heart rate", x = "Time") +
    xlim(int_start(time_int), int_end(time_int)) +
    ylim(hr_range[1], hr_range[2]) +
    theme(axis.title = element_text(size = 8),
          axis.text = element_text(size = 8))
  
  return(plot_grid(g1, g2, ncol = 1) )  # Add a geom_polgon() here to associate the two graphs visually? element_rect()?
}

# Multiple hour plots
multi_hour_plot <- function (oxi_data) {
  study_period <- int_diff(
    seq(floor_date(min(oxi_data$Timepoint), "hour"),
        ceiling_date(max(oxi_data$Timepoint), "hour"),
        by = "hours"))
  
  return(map(study_period, single_hour_plot, oxi_data))
}

# Plot to pdf - incomplete
sleep_hours_pdf <- function (hour_plots, file="sleep_hours.pdf") {

  pdf(file, paper = "a4")
  for(i in length(hour_plots)) { 
    if((i %% 4) == 1) {
      j <- ifelse((i+3) < length(hour_plots), i+3, length(hour_plots))
      print(plot_grid(plotlist = hour_plots[i:j], ncol = 1) +
              theme(plot.margin = margin(1, 1, 1, 1, "cm"))
      )
    }
  }
  dev.off()
}

# Calculate ODI
calculate_ODI <- function (oxi_data, odi_threshold=4) {
  peak <- 0
  trough <- 0
  max_dip <- 0
  desaturations <- 0
  counted <- FALSE
  
  time_at_zero <- 0
  last_time_zero <- oxi_data$Timepoint[1] # Initial assignment probably not necessary
  at_zero <- FALSE
  
  #debug_log <- data.frame(it = -1, at_z = FALSE, zerotime = 0, time_since = 0, timepoint = now())
                          
  
  for(i in 1:nrow(oxi_data)) {
    #print(sprintf("i = %d, SpO2 = %d, desaturations = %d, peak = %d, trough = %d, counted = %s", i, oxi_data$SpO2[i], desaturations, peak, trough, counted))
    #print(sprintf("i = %d, at_zero = %s, time_at_zero = %d, time_since = %d", i, at_zero, time_at_zero, int_length(last_time_zero %--% oxi_data$Timepoint[i])))
    #debug_log <- add_row(debug_log, it=i, at_z=at_zero, zerotime=time_at_zero, time_since=int_length(last_time_zero %--% oxi_data$Timepoint[i]), timepoint=oxi_data$Timepoint[i])
    
    if(oxi_data$SpO2[i] == 0) {
      if(at_zero == TRUE) {
        time_at_zero <- time_at_zero + int_length(last_time_zero %--% oxi_data$Timepoint[i])
      }
      peak <- 0
      trough <- 0
      counted <- FALSE
      at_zero <- TRUE
      last_time_zero <- oxi_data$Timepoint[i]
      next
    } else {
      at_zero <- FALSE
    }
    
    if(oxi_data$SpO2[i] > peak) {
      peak <- oxi_data$SpO2[i]
      trough <- oxi_data$SpO2[i]
    }
    else if(oxi_data$SpO2[i] < trough) {
      trough <- oxi_data$SpO2[i]
    }
    
    if((oxi_data$SpO2[i] <= (peak - odi_threshold)) && (counted == FALSE)) {
      desaturations <- desaturations + 1
      counted <- TRUE
    }
    
    if((peak - trough) > max_dip) max_dip <- peak - trough
    
    if(oxi_data$SpO2[i] >= (trough + odi_threshold)) {
      counted <- FALSE
      peak <- oxi_data$SpO2[i]
      trough <- oxi_data$SpO2[i]
    }
  }
  
  #return (debug_log)
  return (list(total_time = int_length(oxi_data$Timepoint[1] %--% oxi_data$Timepoint[nrow(oxi_data)]),
               recording_time = int_length(oxi_data$Timepoint[1] %--% oxi_data$Timepoint[nrow(oxi_data)]) - time_at_zero,
               total_desats = desaturations,
               odi = (desaturations * 3600) / ((int_length(oxi_data$Timepoint[1] %--% oxi_data$Timepoint[nrow(oxi_data)]) - time_at_zero)),
               max_desat = max_dip)) 
}

# Wrapper to use map easily on calculate_ODI() 
odi_block <- function(time_int, oxi_data) {
  ox <- filter(oxi_data, Timepoint %within% time_int)
  if(nrow(ox) == 0) return (NA)
  return (calculate_ODI(ox))
}

# Load data function
load_raw_oxi <- function (filename) {
  oxi <- read.csv(file = filename, 
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
    transmute(Timepoint = mdy_hms(T3), 
              SpO2 = as.integer(T1),
              Pulse = as.integer(T2),
              Exception = Exception) %>%
    distinct()

  return (oxi1)
}

# Calculate heart rate index
calculate_HRI <- function (oxi_data, hri_threshold=6) {
  peak <- 250
  trough <- 250
  max_spike <- 0
  spikes <- 0
  counted <- FALSE
  
  time_at_zero <- 0
  last_time_zero <- oxi_data$Timepoint[1] # Initial assignment probably not necessary
  at_zero <- FALSE
  
  for(i in 1:nrow(oxi_data)) {
     if(oxi_data$Pulse[i] == 0) {
      if(at_zero == TRUE) {
        time_at_zero <- time_at_zero + int_length(last_time_zero %--% oxi_data$Timepoint[i])
      }
      peak <- 0
      trough <- 0
      counted <- FALSE
      at_zero <- TRUE
      last_time_zero <- oxi_data$Timepoint[i]
      next
    } else {
      at_zero <- FALSE
    }
    
    if(oxi_data$Pulse[i] > peak) {
      peak <- oxi_data$Pulse[i]
    }
    else if(oxi_data$Pulse[i] < trough) {
      trough <- oxi_data$Pulse[i]
      peak <- oxi_data$Pulse[i]
    }
    
    if((oxi_data$Pulse[i] >= (trough + hri_threshold)) && (counted == FALSE)) {
      spikes <- spikes + 1
      counted <- TRUE
    }
    
    if((peak - trough) > max_spike) max_spike <- peak - trough
    
    if(oxi_data$Pulse[i] <= (peak - hri_threshold)) {
      counted <- FALSE
      peak <- oxi_data$Pulse[i]
      trough <- oxi_data$Pulse[i]
    }
  }
  
  return (list(total_time = int_length(oxi_data$Timepoint[1] %--% oxi_data$Timepoint[nrow(oxi_data)]),
               recording_time = int_length(oxi_data$Timepoint[1] %--% oxi_data$Timepoint[nrow(oxi_data)]) - time_at_zero,
               total_spikes = spikes,
               hri = (spikes * 3600) / ((int_length(oxi_data$Timepoint[1] %--% oxi_data$Timepoint[nrow(oxi_data)]) - time_at_zero)),
               max_spike = max_spike)) 
}

# Correct time from oximeter device
correct_time <- function (oxi_data, add_hours, add_mins) {
  return(mutate(oxi_data, Timepoint = Timepoint + hours(add_hours) + minutes(add_mins)))
}

# Create summary plot
summary_plot <- function(oxi_data) {
  g1 <- ggplot(oxi_data, aes(Timepoint, SpO2)) +
    geom_step(colour = "red") +
    labs(y = "Oxygen saturations", x = "Time") +
    ylim(70, 100)
  g2 <- ggplot(oxi_data, aes(Timepoint, Pulse)) +
    geom_step(colour = "blue") +
    labs(y = "Heart rate", x = "Time") +
    ylim(0, 200)
  return(plot_grid(g1, g2, ncol = 1))  
}
