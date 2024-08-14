##this script creates a workaround to match the sf and dodgr network. The sf network was necessary to match the TIMS data spatially
##connects sf and dodgr network based on node ids and distance

##load libraries
library(dodgr)
library(tidyverse)
library(sf)

#read in dodgr network
nc_roads <- dodgr_load_streetnet("data/nc_weighted_network.Rds")
nc_roads <- dodgr_contract_graph(nc_roads) %>% as_tibble()

#read in sf network
nc_sf <- st_read("data/nc_sf_network.shp") 

##check that edge_ids do not match
unique(nc_roads$edge_id %in% nc_sf$edge_id)


#check whether from_id and to_id match as well as distance and name. There are a small 
#amount of duplicates. Initially there were some non-matches which was shown to be
#rounding issues with the distance column
nc_sf_ids <- nc_sf %>% select(from_id, to_id, name, d) %>% mutate(d = round(d, 6)) %>% st_drop_geometry() %>% unique()
nc_roads_ids <- nc_roads %>% as_tibble() %>% select(from_id, to_id, name, d) %>% mutate(d = round(d, 6)) %>% unique()

unique(do.call(paste0, nc_sf_ids) %in% do.call(paste0, nc_roads_ids))

###create new merged dataset based on from_id and to_id and name
##creates some duplicates with different edge IDs for same from_id, to_id, and name
simp_nc_roads <- nc_roads %>% select(edge_id, from_id, to_id, name, d) %>% mutate(d = round(d, 6))
simp_nc_sf <- nc_sf %>% select(edge_id, from_id, to_id, name, d) %>% st_drop_geometry() %>% mutate(d = round(d, 6))

merged_df <- merge(simp_nc_sf, simp_nc_roads, by = c("from_id", "to_id", "name", "d")) %>% unique()


##set up process to identify new edge_ids that match the dodgr network for each
##closure in the closure file
closure_data <- read_csv("data/final_road_closures.csv") %>% mutate(edge_id_dodgr = "")

##add a new column that has the dodgr network edgeids that match the sf network edge ids
for (i in 1:nrow(closure_data)) {
  print(i)
  edge_list <- unlist(strsplit(closure_data[i,]$edge_id, ", "))
  filtered_data <- merged_df %>% filter(edge_id.x %in% edge_list) 
  filtered_edges <- filtered_data$edge_id.y
  closure_data[i,]$edge_id_dodgr <- paste(filtered_edges, collapse = ", ")
}

#save file
write_csv(closure_data, "data/dodgr_id_road_closures.csv")



