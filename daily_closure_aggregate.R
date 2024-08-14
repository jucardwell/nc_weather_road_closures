##this script creates a full analytical file

##analyze closure data
library(tidyverse)
library(sf)

#bgs
bg_data <- st_read("")
closure_files <- list.files("output/agg_local_travel", full.names = TRUE)
total_days <- 2743

# Initialize an empty data frame to store the combined data
combined_data <- NULL

# Loop through each file
for (file in closure_files) {
  print(file)
  # Read the file
  # Read the file
  data <- read_csv(file)
  
  # Add a substring of the file name as a prefix to each column
  prefix <- substr(file, start = 66, stop = 75)  
  colnames(data) <- paste0(prefix, "_", colnames(data))
  
  # Combine the modified data frame with the existing data
  if (is.null(combined_data)) {
    combined_data <- data
  } else {
    data <- select(data, -ends_with("o_fid"), -ends_with("tot_trips"))
    combined_data <- bind_cols(combined_data, data)
  }
}

combined_data <- combined_data %>% rename("o_fid" = "2016-01-01_o_fid", "tot_trips" = "2016-01-01_tot_trips")


write_csv(combined_data, "output/aggregated_closure_impact.csv")
