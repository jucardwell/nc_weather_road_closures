##This script identifies a matching osm road for each road closure that was
##not matched with a spatial join


#load libraries
library(sf)
library(tidyverse)
library(stringdist)

#closure data- manual 
closure_data <- read_csv("../data/unmatched_closures.csv")

#osm road data
road_data <- st_read("../data/nc_sf_network.shp") %>% st_transform(st_crs(2264))

#spatialize closures
spatial_closures <- closure_data %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
                    st_transform(st_crs(road_data))

spatial_closures_na <- spatial_closures %>% filter(is.na(CommonName) == TRUE)

spatial_closures_nona <- spatial_closures %>% filter(is.na(CommonName) == FALSE)

##looks within 300ft of roads without a name and extracts the minimum distance
for (i in 1:nrow(spatial_closures_na)) {
  print(i)
  closure <- spatial_closures_na[i,]
  closure_buffer <- st_buffer(closure, 300)
  road_subset <- road_data %>% filter(st_intersects(., closure_buffer, sparse=FALSE))
  
  if(nrow(road_subset) > 0) {
    road_subset$geo_distance <- road_subset %>% st_distance(closure)
    selected_roads <- road_subset %>% filter(geo_distance == min(geo_distance))
    edge_id <- selected_roads$edge_id
    way_id <- selected_roads$way_id
    spatial_closures_na[i,]$edge_id <- paste(edge_id, collapse = ", ")
    spatial_closures_na[i,]$way_id <- paste(way_id, collapse = ", ")
    spatial_closures_na[i,]$match <- 2
    spatial_closures_na[i,]$oneway <- ifelse(max(selected_roads$oneway) == 1, 1, 0)
  }
}

#looks within 300ft and matches based on name and distance

for (i in 1:nrow(spatial_closures_nona)) {
  print(i)
  closure <- spatial_closures_nona[i,]
  closure_buffer <- st_buffer(closure, 300)
  match_road <- closure$CommonName
  match_length <- nchar(match_road) / 2
  road_subset <- road_data %>% filter(st_intersects(., closure_buffer, sparse=FALSE))
  
  if(nrow(road_subset) > 0){
    road_subset$string_distance <- stringdist(match_road, road_subset$name, method = "lv") 
    road_subset$string_distance[is.na(road_subset$string_distance)] <- 100
    road_subset$geo_distance <- road_subset %>% st_distance(closure)
    min <- min(road_subset$string_distance)
    
    if(min < match_length) {
      filtered_road_subset <- road_subset %>% filter(string_distance == min(string_distance))
      selected_roads <- filtered_road_subset %>% filter(geo_distance == min(geo_distance))
      edge_id <- selected_roads$edge_id
      way_id <- selected_roads$way_id
      spatial_closures_nona[i,]$edge_id <- paste(edge_id, collapse = ", ")
      spatial_closures_nona[i,]$way_id <- paste(way_id, collapse = ", ")
      spatial_closures_nona[i,]$match <- 1
      spatial_closures_nona[i,]$oneway <- ifelse(max(selected_roads$oneway) == 1, 1, 0)
    } else {
      selected_roads <- road_subset %>% filter(geo_distance == min(geo_distance))
      edge_id <- selected_roads$edge_id
      way_id <- selected_roads$way_id
      spatial_closures_nona[i,]$edge_id <- paste(edge_id, collapse = ", ")
      spatial_closures_nona[i,]$way_id <- paste(way_id, collapse = ", ")
      spatial_closures_nona[i,]$match <- 2
      spatial_closures_nona[i,]$oneway <- ifelse(max(selected_roads$oneway) == 1, 1, 0) 
    }
  }
}

#binds together
binded_closures <- rbind(spatial_closures_na, spatial_closures_nona)

#drops geometry
non_spatial <- binded_closures %>% st_drop_geometry()

#writes file
write_csv(non_spatial, "../data/unmatched_matched.csv")
