##this script identifies closed roads for each day in the study period

##load libraries
library(tidyverse)

#read in closure data
closure_data <- read_csv("data/dodgr_id_road_closures.csv")

#get a list of all days from 01-01-2016 to 07-05-2023
list_days <- seq(as.Date("2016-01-01"), as.Date("2023-07-05"), by="days")

#get closures that were closed on each day and make a new .csv file
for (i in 1:length(list_days)){
  date <- list_days[i]
  print(date)
  filtered_data <- closure_data %>% filter(start_day <= date & end_day >= date) 
  edge_list <- unlist(strsplit(filtered_data$edge_id_dodgr, ", "))
  df <- as.data.frame(edge_list)
  write_csv(df, paste("data/daily_road_closures/", date, ".csv", sep = ""))
}