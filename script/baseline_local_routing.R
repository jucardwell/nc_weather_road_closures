##Calculate baseline routing
library(dodgr)
library(tidyverse)

#read in osm data
nc_roads <- dodgr_load_streetnet("../data/nc_weighted_network.Rds")
nc_roads <- dodgr_contract_graph(nc_roads)
#get largest component
largest_component <- nc_roads[nc_roads$component == 1, ]


#read in local neighbor file
neighbor <- read_csv("../data/local_travel_od.csv")
bg <- read_csv("../data/bg_simp_routable_manual.csv") %>% mutate(node_id = as.character(node_id))

bg_o <- bg %>% filter(fid %in% neighbor$o_fid)  
bg_d <- bg %>% filter(fid %in% neighbor$d_fid)

write_csv(bg_o, "../data/bg_o.csv")
write_csv(bg_o, "../data/bg_d.csv")

from <- bg_o$node_id
to <- bg_d$node_id

#calculate shortest time
time <- dodgr_times(largest_component, from = from, to = to, shortest = FALSE)

#to dataframe
time_dataframe <- as.data.frame(time)

#columns are the destinations
colnames(time_dataframe) <- bg_d$fid

#rows are the origins
time_dataframe$o_fid <- bg_o$fid

pivoted_df <- time_dataframe %>% pivot_longer(cols = !o_fid, names_to = "d_fid", values_to = "baseline_secs_travel")

#merge to neighbor file
merged_neighbor <- merge(neighbor, pivoted_df, by = c("o_fid", "d_fid"))

#write final file
write_csv(merged_neighbor, "../data/local_travel_baseline.csv")

