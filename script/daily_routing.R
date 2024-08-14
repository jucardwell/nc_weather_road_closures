library(tidyverse)
library(dodgr)

#read in osm data
nc_roads <- dodgr_load_streetnet("../data/nc_weighted_network.Rds")
nc_roads <- dodgr_contract_graph(nc_roads)
#get largest component
largest_component <- nc_roads[nc_roads$component == 1, ]

#routing data
comp_df <- read_csv("../data/local_travel_baseline.csv")
bg_o <- read_csv("../data/bg_o.csv") %>% mutate(node_id = as.character(node_id))
bg_d <- read_csv("../data/bg_d.csv") %>% mutate(node_id = as.character(node_id))

#get all closure data
closure_files <- list.files("../data/daily_road_closures/") 

#go through each daily file in the year
for (i in 1:length(filtered_files)){
  #get file name
  file_name <- filtered_files[i] 
  #get file name without .csv for save
  updated_file_name <- substr(file_name, 1, 10)
  #print to keep track of loop
  print(file_name)
  
  #if it is the first iteration, run the entire process
  if(i == 1){
    #get closures for the day
    closures <- read_csv(paste("../data/daily_road_closures/", file_name, sep = ""))
    
    #filter road network to just open roads
    open <- largest_component[!(largest_component$edge_id %in% closures$edge_list),]
    
    #get routable bgs. Dodgr will throw error if there are from or to nodes that don't exist in the open network
    bg_o_routable <- bg_o %>% filter(node_id %in% open$to_id)
    bg_d_routable <- bg_d %>% filter(node_id %in% open$to_id)
    #set these as to  and from for the routing
    from <- bg_o_routable$node_id
    to <- bg_d_routable$node_id
    
    #calculate shortest time
    time <- dodgr_times(open, from = from, to = to, shortest = FALSE)
    
    #turn to data frame
    time_dataframe <- as.data.frame(time)
    
    colnames(time_dataframe) <- bg_d_routable$fid
    
    time_dataframe$o_fid <- bg_o_routable$fid
    
    pivoted_df <- time_dataframe %>% pivot_longer(cols = !o_fid, names_to = "d_fid", values_to = "closure_secs_travel")
    
    #merge with baseline scenario
    merged_df <- merge(comp_df, pivoted_df, by = c("o_fid", "d_fid"), all.x = TRUE) %>% mutate(diff = round(closure_secs_travel/60, 2) - round(baseline_secs_travel/60, 2))
    
    #write to .csv
    write_csv(merged_df, paste("../output/local_travel/", updated_file_name, "local_travel_closures.csv", sep = ""))
    
    ##if it isn't the first iteration, check if the closures for this day are the exactly same as from the day before
  } else{
    #get name of the previous day
    previous_file_name <- filtered_files[i-1]
    #get data for the current day
    closures <- read_csv(paste("../data/daily_road_closures/", file_name, sep = ""))
    #read in data for the previos day
    previous_file <- read_csv(paste("../data/daily_road_closures/", previous_file_name, sep = ""))
    
    ##if there are 0 rows do nothing
    if(nrow(closures) == 0){
      
      
      #if the file is exactly the same as the day before save the already existing file
    }else if (setequal(closures$edge_list, previous_file$edge_list)){
      write_csv(merged_df, paste("output/local_travel/", updated_file_name, "local_travel_closures.csv", sep = ""))
      
      #if it is different, re-calculate
    }else {
      
      open <- largest_component[!(largest_component$edge_id %in% closures$edge_list),]
      
      #get routable bgs. Dodgr will throw error if there are from or to nodes that don't exist in the open network
      bg_o_routable <- bg_o %>% filter(node_id %in% open$to_id)
      bg_d_routable <- bg_d %>% filter(node_id %in% open$to_id)
      #set these as to for the routing
      from <- bg_o_routable$node_id
      to <- bg_d_routable$node_id
      
      #calculate shortest time
      time <- dodgr_times(open, from = from, to = to, shortest = FALSE)
      
      #turn to data frame
      time_dataframe <- as.data.frame(time)
      
      colnames(time_dataframe) <- bg_d_routable$fid
      
      time_dataframe$o_fid <- bg_o_routable$fid
      
      pivoted_df <- time_dataframe %>% pivot_longer(cols = !o_fid, names_to = "d_fid", values_to = "closure_secs_travel")
      
      #merge with baseline scenario
      merged_df <- merge(comp_df, pivoted_df, by = c("o_fid", "d_fid"), all.x = TRUE) %>% mutate(diff = round(closure_secs_travel/60, 2) - round(baseline_secs_travel/60, 2))
      
      #write to .csv
      write_csv(merged_df, paste("../output/local_travel/", updated_file_name, "local_travel_closures.csv", sep = ""))
    }
  }
}
