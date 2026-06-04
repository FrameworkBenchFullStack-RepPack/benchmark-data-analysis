# Performs the One-sample Kolmogorov-Smirnov test
library(dplyr)

home_interact <- read.csv("benchmark-data/home-interact.csv", check.names = FALSE)
list_interact <- read.csv("benchmark-data/list-interact.csv", check.names = FALSE)
list_data     <- read.csv("benchmark-data/list.csv", check.names = FALSE)
live_data     <- read.csv("benchmark-data/live.csv", check.names = FALSE)
navigate_data <- read.csv("benchmark-data/navigate.csv", check.names = FALSE)
static_data   <- read.csv("benchmark-data/static.csv", check.names = FALSE)

data_list <- list(
  "Home Interact" = home_interact,
  "List Interact" = list_interact,
  "List Page"     = list_data,
  "Live Page"     = live_data,
  "Navigate"      = navigate_data,
  "Static Pages"  = static_data
)

all_data <- bind_rows(data_list, .id = "Source")

all_power_avg_data <- all_data$`Combined Energy Average (J)`

ks_result <- ks.test(
  all_power_avg_data, 
  "pnorm", 
  mean = mean(all_power_avg_data, na.rm = TRUE), 
  sd = sd(all_power_avg_data, na.rm = TRUE)
)

print(ks_result)
