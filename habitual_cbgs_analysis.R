library(tidyverse)
library(sf)
library(tmap)
library(ggplot2)
library(geojsonsf)

#cbgs
cbgs <- st_read("../../../DISS_DATA_SP2023/census_data/nc_blockgroup_centroids_2264.shp")
cbgs <- st_transform(cbgs, 4326)
urban_areas <- geojson_sf("../../../DISS_DATA_SP2023/other_data/2010_Census_Urban_Areas.geojson") %>% st_union(by_feature = FALSE)

nc_county <- st_read("../../../DISS_DATA_SP2023/census_data/NCDOT_County_Boundaries/NCDOT_County_Boundaries.shp") %>% st_transform(st_crs(cbgs))

#neighbors
neighbor <- read_csv("../../../DISS_DATA_SP2023/updated_routing_data/local_travel_baseline_11152023.csv") 
summarized <- neighbor %>% group_by(o_fid) %>% summarise("Number of Habitual Block Groups" = n(), "Mean Travel Time to Habitual Block Groups (mins)" = mean(baseline_secs_travel)/ 60) %>%st_drop_geometry()

##make example figure
one_example <- neighbor %>% filter(o_fid == 1000)
origin <- cbgs %>% filter(fid == 1000)
destinations <- cbgs %>% filter(fid %in% one_example$d_fid)
tmap_mode("view")
tm_shape(origin) + tm_dots(col = "blue", size = .1) + tm_shape(destinations) + tm_dots(col = "red", size = .1)

#merge data and add urban/non-urban designation
merged_data <- cbgs %>% left_join(summarized, by = c("fid" = "o_fid")) 
merged_data <- merged_data %>% mutate(urban = st_intersects(merged_data, urban_areas, sparse = FALSE))
merged_data <- st_transform(merged_data, 2264)


#hexagonize 
hexagons <- st_make_grid(merged_data, cellsize = 35000, square = FALSE) %>% st_sf() %>% rowid_to_column('hex_id')

joined_data <- st_join(merged_data, hexagons, join = st_within)

neighbors_avrg <- joined_data %>% group_by(hex_id) %>% summarise(`# of Habitual Block Groups` = mean(`Number of Habitual Block Groups`, na.rm = TRUE), `Mean Travel Time to Habitual Block Groups` = mean(`Mean Travel Time to Habitual Block Groups (mins)`, na.rm = TRUE), n = n()) %>% st_drop_geometry()
#Filter to hexs that have at least 1 cbg in it and also does not have a value of NaN which is CBGS that don't have matching data (unroutable or safegraph issue)
neighbors_hex <- hexagons %>% left_join(neighbors_avrg) %>% filter(n > 0) %>% filter(!(is.na(`# of Habitual Block Groups`)))
tm_shape(neighbors_hex) + tm_polygons("Mean Travel Time to Habitual Block Groups", style = "jenks", palette = "BuPu") + tm_shape(nc_county) + tm_borders(lwd = .8) +   tm_legend(legend.text.size = .9,
                                                                                                                                                          legend.title.size=1.2)






# Calculate statistics for number of neighbors (overall)
min_val <- round(min(summarized$`Number of Habitual Block Groups`), 1)
max_val <- round(max(summarized$`Number of Habitual Block Groups`), 1)
num_obs <- round(length(summarized$`Number of Habitual Block Groups`), 1)
mean_val <- round(mean(summarized$`Number of Habitual Block Groups`), 1)
median_val <- round(median(summarized$`Number of Habitual Block Groups`), 1)



# Calculate statistics for distance (overall)
min_val <- round(min(summarized$`Mean Distance to Habitual Block Groups (mins)`), 1)
max_val <- round(max(summarized$`Mean Distance to Habitual Block Groups (mins)`), 1)
num_obs <- round(length(summarized$`Mean Distance to Habitual Block Groups (mins)`), 1)
mean_val <- round(mean(summarized$`Mean Distance to Habitual Block Groups (mins)`), 1)
median_val <- round(median(summarized$`Mean Distance to Habitual Block Groups (mins)`), 1)


#filter to urban/non-urban
urban <- merged_data %>% filter(urban == TRUE)
non_urban <- merged_data %>% filter(urban == FALSE)


# Calculate statistics for number of neighbors (urban)
min_val <- round(min(urban$`Number of Habitual Block Groups`), 1)
max_val <- round(max(urban$`Number of Habitual Block Groups`), 1)
num_obs <- round(length(urban$`Number of Habitual Block Groups`), 1)
mean_val <- round(mean(urban$`Number of Habitual Block Groups`), 1)
median_val <- round(median(urban$`Number of Habitual Block Groups`), 1)


# Calculate statistics for distance (urban)
min_val <- round(min(urban$`Mean Distance to Habitual Block Groups (mins)`), 1)
max_val <- round(max(urban$`Mean Distance to Habitual Block Groups (mins)`), 1)
num_obs <- round(length(urban$`Mean Distance to Habitual Block Groups (mins)`), 1)
mean_val <- round(mean(urban$`Mean Distance to Habitual Block Groups (mins)`), 1)
median_val <- round(median(urban$`Mean Distance to Habitual Block Groups (mins)`), 1)


# Calculate statistics for number of neighbors (non-urban)
min_val <- round(min(non_urban$`Number of Habitual Block Groups`), 1)
max_val <- round(max(non_urban$`Number of Habitual Block Groups`), 1)
num_obs <- round(length(non_urban$`Number of Habitual Block Groups`), 1)
mean_val <- round(mean(non_urban$`Number of Habitual Block Groups`), 1)
median_val <- round(median(non_urban$`Number of Habitual Block Groups`), 1)


# Calculate statistics for distance (non-urban)
min_val <- round(min(non_urban$`Mean Distance to Habitual Block Groups (mins)`), 1)
max_val <- round(max(non_urban$`Mean Distance to Habitual Block Groups (mins)`), 1)
num_obs <- round(length(non_urban$`Mean Distance to Habitual Block Groups (mins)`), 1)
mean_val <- round(mean(non_urban$`Mean Distance to Habitual Block Groups (mins)`), 1)
median_val <- round(median(non_urban$`Mean Distance to Habitual Block Groups (mins)`), 1)


ggplot(merged_data, aes(x= `Number of Habitual Block Groups`, fill = urban)) + geom_histogram()
ggplot(merged_data, aes(x= `Mean Distance to Habitual Block Groups (mins)`, fill = urban)) + geom_histogram()
