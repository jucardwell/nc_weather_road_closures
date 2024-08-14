##analyze closure data
library(tidyverse)
library(sf)
library(tmap)
library(kableExtra)
library(ggplot2)
library(geojsonsf)
library(RColorBrewer)
tmap_mode("plot")

#read in aggregated daily closure data (wide-form)
agg_data <- read_csv("output/aggregated_closure_impact.csv")
bg_data <- st_read("")

base_df <- agg_data %>% select(o_fid, tot_trips)

nc_county <- st_read("") %>% st_transform(st_crs(bg_data)) %>% filter(STATEFP == "37")
## Unroutable Florence
unroutable_flo <- c("unroutable")
selected_data <- agg_data[, c(1, 2, 3935:4020)]

# Select columns that have the specified substring
unroutable_flo_df <- selected_data %>%
  select(o_fid, tot_trips, matches(unroutable_flo))

sum_unroutable_flo <- unroutable_flo_df %>% select(-o_fid, -tot_trips) %>% rowwise() %>% summarise(unroutable_trips = sum(c_across(everything())))

unroutable_full_flo <- sum_unroutable_flo %>% cbind(base_df)
unroutable_sf_flo <- bg_data %>% left_join(unroutable_full_flo, join_by("fid" == "o_fid"))

#hexagonize 
hexagons_unroutable_flo <- st_make_grid(unroutable_sf_flo, cellsize = 35000, square = FALSE) %>% st_sf() %>% rowid_to_column('hex_id')
joined_data_unroutable_flo <- st_join(unroutable_sf_flo, hexagons_unroutable_flo, join = st_within)

unroutable_avrg_flo <- joined_data_unroutable_flo %>% group_by(hex_id) %>% summarise(`Florence Unrouteable Trips` = sum(unroutable_trips, na.rm = TRUE), n = n()) %>% st_drop_geometry()
#Filter to hexs that have at least 1 cbg in it and also does not have a value of NaN which is CBGS that don't have matching data (unroutable or safegraph issue)
unroutable_hex_flo <- hexagons_unroutable_flo %>% left_join(unroutable_avrg_flo) %>% filter(n > 0) %>% filter(`Florence Unrouteable Trips` > 0)
tm_shape(unroutable_hex_flo) + tm_polygons("Florence Unrouteable Trips", style = "jenks", palette = "BuPu") + tm_shape(nc_county) + tm_borders(lwd = .8) + tm_layout(
  legend.title.size = 1.4, 
  legend.text.size = 1.1
) 




##total Unroutable 
unroutable <- "num_unroutable"

# Select columns that have the specified substring
unroutable_df <- agg_data %>%
  select(o_fid, tot_trips, matches(unroutable))

sum_unroutable <- unroutable_df %>% select(-o_fid, -tot_trips) %>% rowwise() %>% summarise(unroutable_trips = sum(c_across(everything())))

unroutable_full <- sum_unroutable %>% cbind(base_df)
unroutable_sf <- bg_data %>% left_join(unroutable_full, join_by("fid" == "o_fid"))

#hexagonize 
hexagons_unroutable <- st_make_grid(unroutable_sf, cellsize = 35000, square = FALSE) %>% st_sf() %>% rowid_to_column('hex_id')
joined_data_unroutable <- st_join(unroutable_sf, hexagons_unroutable, join = st_within)

unroutable_avrg <- joined_data_unroutable %>% group_by(hex_id) %>% summarise(`Total Number of Unrouteable Trips` = sum(unroutable_trips, na.rm = TRUE), n = n()) %>% st_drop_geometry()
#Filter to hexs that have at least 1 cbg in it and also does not have a value of NaN which is CBGS that don't have matching data (unroutable or safegraph issue)
unroutable_hex <- hexagons_unroutable %>% left_join(unroutable_avrg) %>% filter(n > 0) %>% filter(`Total Number of Unrouteable Trips` > 0)
tm_shape(unroutable_hex) + tm_polygons("Total Number of Unrouteable Trips", style = "jenks", palette = "BuPu") + tm_shape(nc_county) + tm_borders(lwd = .5) + tm_layout(
  legend.title.size = 1.4, 
  legend.text.size = 1.1,   
) 


###Maximum
maximum <- "average_time_add"

# Select columns that have the specified substring
maximum_df <- agg_data %>%
  select(o_fid, tot_trips, matches(maximum))

maximum <- maximum_df %>% select(-o_fid, -tot_trips) %>% rowwise() %>% summarise(max_average_time_add = max(c_across(everything()), na.rm = TRUE))

maximum_full <-  maximum %>% cbind(base_df)
maximum_sf <- bg_data %>% left_join(maximum_full, join_by("fid" == "o_fid"))

#hexagonize 
hexagons <- st_make_grid(maximum_sf, cellsize = 35000, square = FALSE) %>% st_sf() %>% rowid_to_column('hex_id')
joined_data <- st_join(maximum_sf, hexagons, join = st_within)

max_avrg <- joined_data %>% group_by(hex_id) %>% summarise(`Max Disruption` = mean(max_average_time_add, na.rm = TRUE), n = n()) %>% st_drop_geometry()
#Filter to hexs that have at least 1 cbg in it and also does not have a value of NaN which is CBGS that don't have matching data (unroutable or safegraph issue)
max_hex <- hexagons %>% left_join(max_avrg) %>% filter(n > 0) %>% filter(!(is.na(`Max Disruption`))) %>% mutate(`Max Disruption` = round(`Max Disruption`, 0))


tm_shape(max_hex) + tm_polygons("Max Disruption", style = "jenks", palette = "BuPu") + tm_shape(nc_county) + tm_borders(lwd = .8) + 
  tm_layout(legend.position = c("left", "bottom")) + 
  tm_legend(legend.text.size = 1.1,
            legend.title.size=1.4)

for_hist <- data.frame(
  maximum_average_time_add = rep(maximum_full$max_average_time_add, each = maximum_full$tot_trips)
) %>% mutate(`Max Disruption` = ifelse(maximum_average_time_add > 8, 8, maximum_average_time_add))

for_hist %>% 
  ggplot(aes(`Max Disruption`)) +
  geom_histogram(binwidth = .5, col = "black", fill = "cornflowerblue") + scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, max(for_hist$`Max Disruption`), by = 2)) +
  labs(x = "Max Disruption Per Person", y = "Frequency") + 
  theme(axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22), 
        axis.text.x = element_text(size = 20),  
        axis.text.y = element_text(size = 20),  
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA))


###num days with disruption 
disruption <- "average_time_add"

# Select columns that have the specified substring
disruption_df <- agg_data %>%
  select(o_fid, tot_trips, matches(disruption))

disruption <- disruption_df %>% select(-o_fid, -tot_trips) %>% rowwise() %>% summarise(days_disruption = sum(c_across(everything()) != 0, na.rm = TRUE))


disruption_full <-  disruption %>% cbind(base_df)
disruption_sf <- bg_data %>% left_join(disruption_full, join_by("fid" == "o_fid"))


#hexagonize 
hexagons_disuptions <- st_make_grid(disruption_sf, cellsize = 35000, square = FALSE) %>% st_sf() %>% rowid_to_column('hex_id')
joined_data_disruption <- st_join(disruption_sf, hexagons_disuptions, join = st_within)

disruption_avrg <- joined_data_disruption %>% group_by(hex_id) %>% summarise(`Days with Disruption` = round(mean(days_disruption, na.rm = TRUE),0), n = n()) %>% st_drop_geometry()
#Filter to hexs that have at least 1 cbg in it and also does not have a value of NaN which is CBGS that don't have matching data (unroutable or safegraph issue)
disruption_hex <- hexagons_disuptions %>% left_join(disruption_avrg) %>% filter(n > 0) %>% filter(!(is.na(`Days with Disruption`)))
tm_shape(disruption_hex) + tm_polygons("Days with Disruption", style = "jenks", palette = "BuPu") +  tm_shape(nc_county) + tm_borders(lwd = 1) + 
  tm_legend(legend.text.size = 1.1,
            legend.title.size=1.4)

for_hist <- data.frame(
  num_days = rep(disruption_full$days_disruption, each = disruption_full$tot_trips)
) %>% mutate(`Days with Disruption` = ifelse(num_days> 100, 100, num_days)) 

for_hist %>% 
  ggplot(aes(`Days with Disruption`)) +
  geom_histogram(binwidth = 10, col = "black", fill = "cornflowerblue") + scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, max(for_hist$`Days with Disruption`), by = 25)) + 
  theme(axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 22), 
        axis.text.x = element_text(size = 20),  
        axis.text.y = element_text(size = 20),  
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "Days of Disruption Per Person", y = "Frequency")



###Mean
mean <- "average_time_add"

# Select columns that have the specified substring
mean_df <- agg_data %>%
  select(o_fid, tot_trips, matches(mean))

mean <- mean_df %>% select(-o_fid, -tot_trips) %>% rowwise() %>% summarise(mean_average_time_add = mean(c_across(everything())[c_across(everything()) > 0], na.rm = TRUE)) 
mean_full <-  mean %>% replace(is.na(.), 0) %>% cbind(base_df) 
mean_sf <- bg_data %>% left_join(mean_full, join_by("fid" == "o_fid"))

#hexagonize 
hexagons_mean<- st_make_grid(mean_sf, cellsize = 35000, square = FALSE) %>% st_sf() %>% rowid_to_column('hex_id')
joined_data_mean <- st_join(mean_sf, hexagons_mean, join = st_within)

mean_avrg <- joined_data_mean %>% group_by(hex_id) %>% summarise(`Mean Disruption` = round(median(mean_average_time_add, na.rm = TRUE), 0), n = n()) %>% st_drop_geometry()
#Filter to hexs that have at least 1 cbg in it and also does not have a value of NaN which is CBGS that don't have matching data (unroutable or safegraph issue)
disruption_hex <- hexagons_mean %>% left_join(mean_avrg) %>% filter(n > 0) %>% filter(!(is.na(`Mean Disruption`)))
tm_shape(disruption_hex) + tm_polygons("Mean Disruption", style = "jenks", palette = "BuPu")  + tm_shape(nc_county) + tm_borders(lwd = 1) + 
  tm_legend(legend.text.size = 1.1,
            legend.title.size=1.4)

for_hist <- data.frame(
  mean = rep(mean_full$mean_average_time_add, each = mean_full$tot_trips)
)  %>% mutate(`Mean Disruption` = ifelse(mean> 3, 3, mean))

for_hist %>% 
  ggplot(aes(`Mean Disruption`)) +
  geom_histogram(binwidth = .25, col = "black", fill = "cornflowerblue") + scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, max(for_hist$`Mean Disruption`), by = 1)) + 
  theme(axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22), 
        axis.text.x = element_text(size = 20),  
        axis.text.y = element_text(size = 20),  
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "Mean Disruption Per Person", y = "Frequency")



###Sum of average
sum <- "average_time_add"

# Select columns that have the specified substring
sum_df <- agg_data %>%
  select(o_fid, tot_trips, matches(sum))

sum <- sum_df %>% select(-o_fid, -tot_trips) %>% rowwise() %>% summarise(sum_average_time_add = sum(c_across(everything()), na.rm = TRUE))
sum_full <-  sum %>% cbind(base_df)
sum_sf <- bg_data %>% left_join(sum_full, join_by("fid" == "o_fid"))

#hexagonize 
hexagons_sum<- st_make_grid(sum_sf, cellsize = 35000, square = FALSE) %>% st_sf() %>% rowid_to_column('hex_id')
joined_data_sum <- st_join(sum_sf, hexagons_sum, join = st_within)

sum_avrg <- joined_data_sum %>% group_by(hex_id) %>% summarise(`Aggregate Disruption` = round(mean(sum_average_time_add, na.rm = TRUE),0), n = n()) %>% st_drop_geometry()
#Filter to hexs that have at least 1 cbg in it and also does not have a value of NaN which is CBGS that don't have matching data (unroutable or safegraph issue)
sum_hex <- hexagons_sum %>% left_join(sum_avrg) %>% filter(n > 0) %>% filter(!(is.na(`Aggregate Disruption`)))
tm_shape(sum_hex) + tm_polygons("Aggregate Disruption", style = "jenks", palette = "BuPu") + tm_shape(nc_county) + tm_borders(lwd = 1) + 
  tm_legend(legend.text.size = 1.1,
            legend.title.size=1.4)

for_hist <- data.frame(
  sum_average_time_add = rep(sum_full$sum_average_time_add, each = sum_full$tot_trips)
) %>% mutate(`Aggregate Disruption` = ifelse(sum_average_time_add > 30, 30, sum_average_time_add))

for_hist %>% 
  ggplot(aes(`Aggregate Disruption`)) +
  geom_histogram(binwidth = 2, col = "black", fill = "cornflowerblue") + scale_y_continuous(labels = scales::comma) + 
  theme(axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22), 
        axis.text.x = element_text(size = 20),  
        axis.text.y = element_text(size = 20),  
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "Aggregate Disruption Per Person", y = "Frequency")


#aggregate disruption per hex
###Sum of average
agg <- "total_time_add"

# Select columns that have the specified substring
sum_df <- agg_data %>%
  select(o_fid, tot_trips, matches(agg))

agg <- sum_df %>% select(-o_fid, -tot_trips) %>% rowwise() %>% summarise(sum_time_add = sum(c_across(everything()), na.rm = TRUE))
agg_full <-  agg %>% cbind(base_df)
agg_sf <- bg_data %>% left_join(agg_full, join_by("fid" == "o_fid"))

#hexagonize 
hexagons_agg<- st_make_grid(agg_sf, cellsize = 35000, square = FALSE) %>% st_sf() %>% rowid_to_column('hex_id')
joined_data_agg <- st_join(agg_sf, hexagons_sum, join = st_within)

agg_avrg <- joined_data_agg %>% group_by(hex_id) %>% summarise(`Aggregate Disruption` = round(sum(sum_time_add, na.rm = TRUE),0), n = n()) %>% st_drop_geometry()
#Filter to hexs that have at least 1 cbg in it and also does not have a value of NaN which is CBGS that don't have matching data (unroutable or safegraph issue)
agg_hex <- hexagons_sum %>% left_join(agg_avrg) %>% filter(n > 0) %>% filter(!(is.na(`Aggregate Disruption`)))
tm_shape(agg_hex) + tm_polygons("Aggregate Disruption", style = "jenks", palette = "BuPu") + tm_shape(nc_county) + tm_borders(lwd = 1) + 
  tm_legend(legend.text.size = .9,
            legend.title.size=1.2)


# Get the column name with the maximum value for each row
maximum_colnames <- maximum_df %>% 
  select(-o_fid, -tot_trips) %>% 
  rowwise() %>% 
  mutate(max_col = names(.)[which.max(c_across(everything()))]) %>% 
  ungroup() %>% 
  select(max_col)



maximum_year <- maximum_colnames %>% mutate(Year = substr(max_col, 1, 4))
binded <-  maximum_year %>% cbind(base_df)
binded_sf <- bg_data %>% left_join(binded, join_by("fid" == "o_fid")) %>% drop_na()

binded_no_sf <- binded_sf %>% st_drop_geometry()
library(tigris)
bg_tigris <- block_groups(state = "NC", year = 2020, cb = TRUE) %>% rename(BGGEOID20 = GEOID) %>% left_join(binded_no_sf) %>% drop_na()
tm_shape(bg_tigris) + tm_polygons("Year", palette = brewer.pal(7, "Dark2"), border.alpha = 0)+ 
  tm_legend(legend.text.size = .9,
            legend.title.size=1.2)

hexagons_max_year<- st_make_grid(binded_sf, cellsize = 35000, square = FALSE) %>% st_sf() %>% rowid_to_column('hex_id')
joined_data_max_year <- st_join(binded_sf, hexagons_max_year, join = st_within)


maximum <- maximum_df %>% select(-o_fid, -tot_trips) %>% rowwise() %>% summarise(max_average_time_add = max(c_across(everything()), na.rm = TRUE))

maximum_full <-  maximum %>% cbind(base_df)
maximum_sf <- bg_data %>% left_join(maximum_full, join_by("fid" == "o_fid")) %>% mutate(fips = substr(BGGEOID20, 1, 5))

rurality <- read_csv("") %>% mutate(fips = as.character(FIPS))

maximum_rurality <- maximum_sf %>% left_join(rurality) %>% drop_na()

ggplot(maximum_rurality, aes(x = max_average_time_add, y = rurality)) +
  geom_hex() + 
  scale_fill_continuous(name = "Count") +
  labs(x = "Maximum Disruption", y = "Rurality Index") + 
  theme(axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22), 
        axis.text.x = element_text(size = 20),  
        axis.text.y = element_text(size = 20),  
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.text =  element_text(size = 15),
        legend.title = element_text(size = 17))
