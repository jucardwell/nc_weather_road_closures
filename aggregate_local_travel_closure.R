##this script aggregates daily output to the census block group

#load libraries
library(tidyverse)
library(sf)

#get list of closure files
closure_files <- list.files("output/local_travel/", full.names = TRUE)

#get block groups to get population data
cbgs <- st_read("")

#for each closure file
for (day in closure_files){
  #extract date
date <- substr(day, 48, 57)
#create new file name
new_file_name <- paste("output/agg_local_travel/agg_daily_analysis_", date, ".csv", sep = "")
#calculate number of trips, replace any negatives with 0s
daily_file <- read_csv(day) %>% left_join(cbgs, join_by("o_fid" == "fid")) %>% mutate(trips = (perc/100) * POP20, diff = ifelse(diff < 0, 0, diff), total_diff = diff * trips)
#aggregate to cbg
agg_file <- daily_file %>% group_by(o_fid) %>% summarise(tot_trips = first(POP20), num_unroutable = sum(trips[is.na(diff)]), total_time_add = sum(total_diff, na.rm = TRUE), average_time_add = weighted.mean(diff, perc, na.rm = TRUE)) %>% mutate(perc_unroutable = (num_unroutable/tot_trips) *100)

#write new file
write_csv(agg_file, new_file_name)
}

