##this script determines matched and unmatched closures from the QGIS analysis

##load libraries
library(sf)
library(tidyverse)
library(stringdist)

##joined data (join by location in QGIS, 75ft buffer)
joined_closures<- read_csv("") %>% filter(!is.na(edge_id))

#closure data
closure_data <- read_csv("data/aggregated_road_closures.csv") %>% mutate(edge_id = "", way_id = "", match = 0, oneway = 0)

#loop too add spatial join information to closure data
for (i in 1:nrow(closure_data)){
  closure_id <- closure_data[i,]$IncidentID
  joined_matches <- joined_closures %>% filter(IncidentID == closure_id)
  #if there are any matches from the spatial join, add edge_id, way_id and oneway column
  if(nrow(joined_matches) > 0){
  edge_id <- joined_matches$edge_id
  way_id <- joined_matches$way_id
  closure_data[i,]$edge_id <- paste(edge_id, collapse = ", ")
  closure_data[i,]$way_id <- paste(way_id, collapse = ", ")
  closure_data[i,]$oneway <- ifelse(max(joined_matches$oneway) == 1, 1, 0)
  }
}

matched_closures <- closure_data %>% filter(nchar(edge_id) > 0)

non_matched_closures <- closure_data %>% filter(nchar(edge_id) == 0)

#write new closure dataset
write_csv(matched_closures, "data/matched_closures.csv")
write_csv(non_matched_closures, "data/unmatched_closures.csv")
