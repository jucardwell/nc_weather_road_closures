###This script re-calculates the start and end date of closures based on new criteria

##load libraries
library(tidyverse)

##original data
weather <- read_csv("")
obstruction <- read_csv("")

#processed data
matched <- read_csv("")
nonmatched <- read_csv("")


#need to add back in lat/lon
original_nonmatch <- read_csv("") %>% select(IncidentID, Latitude, Longitude)

##merge back to add lat/lon
merged_nonmatch <- merge(nonmatched, original_nonmatch, by = "IncidentID")


#new full dataset
merged_data <- rbind(matched, merged_nonmatch)

##get observations for all closures in full dataset
filtered_weather <- weather %>% filter(IncidentID %in% merged_data$IncidentID)
filtered_obstruction <- obstruction %>% filter(IncidentID %in% merged_data$IncidentID)


#get rid of observations with eroneous end dates and ferry closures
filtered_data <- rbind(filtered_weather, filtered_obstruction) %>% mutate(updated_start_time = strptime(`StartTime (EST)`, format = "%m/%d/%Y %I:%M:%S %p", tz ="EST"), 
                                                                          updated_end_time = strptime(`EndTime (EST)`, format = "%m/%d/%Y %I:%M:%S %p", tz ="EST"),
                                                                          updated_created_on = strptime(`CreatedOn (EST)`, format = "%m/%d/%Y %I:%M:%S %p", tz ="EST"), 
                                                                          day = format(updated_start_time, "%Y-%m-%d"), year = format(updated_end_time, "%Y")) %>% filter(year != 2099 & year != is.na(year) & year != 9999 & year != 4202 & ConditionName != 'Ferry Closed')


##calculate a variable that will help identify whether a single incidentID is being used for multiple incidents by calculating lag time
filtered_data_diff <- filtered_data %>% select(IncidentID, ConditionName, updated_start_time, updated_end_time, updated_created_on) %>% group_by(IncidentID) %>% arrange(updated_end_time, .by_group = TRUE) %>% arrange(updated_start_time, .by_group = TRUE) %>% mutate(time_diff = updated_start_time -lag(updated_end_time), diff_hours = as.numeric(time_diff, units = 'hours'), group = 1)

##initialize variable
filtered_data_diff$diff_hours[is.na(filtered_data_diff$diff_hours)] <- 0

###if the lag time between closure observations is greater than 48 hours, subdivde an incident ID and give it an updated incident ID
filtered_data_updated <- data.frame()
for (i in unique(filtered_data_diff$IncidentID)){
  print(i)
  counter <- 1
  filtered_data_time <- filtered_data_diff %>% filter(IncidentID == i)
  for (j in 1:nrow(filtered_data_time)){
    if (filtered_data_time[j,]$diff_hours > 48){
      counter <- counter + 1
      filtered_data_time[j,]$group <- counter
    }else {
      filtered_data_time[j,]$group <- counter
    }
  }
  filtered_data_updated <- rbind(filtered_data_updated, filtered_data_time)  
}

##get info about how many observations had different groups within a single incident ID
test <- filtered_data_updated %>% group_by(IncidentID) %>% summarise(n = length(unique(group)))

##merge back to get rest of closure information
filtered_data_updated_merge <- filtered_data_updated %>% mutate(updated_ID = paste(IncidentID, group, sep = "")) %>% select(IncidentID, updated_start_time, updated_end_time, updated_ID, updated_created_on) 



##new dataset with updated incident ID
new_nonagg_dataset <- merge(filtered_data_updated_merge, merged_data, by = "IncidentID")

##aggregate events and calculate end time based on a new criteria (most up to date entry in TIMS). THis is to fix a problem that was noticed where when there are multiple
##entries for a closure, some of the end dates might ahve been outdated and superceded by a new entry. Original calculation method (max) wouldnt have caught this
agg_events <- new_nonagg_dataset %>% group_by(updated_ID) %>% summarise(IncidentID =first(IncidentID), start = min(updated_start_time), end = updated_end_time[first(which(updated_created_on == max(updated_created_on)))], RoadName = first(RoadName), 
                                                                                     CommonName = first(CommonName), Direction= first(Direction), CrossStreetName = first(CrossStreetName), 
                                                                                     CrossStCommonName = first(CrossStCommonName), Latitude= first(Latitude), Longitude = first(Longitude), ConditionName= first(ConditionName), 
                                                                                     IncidentType = first(IncidentType), Event = first(EventName), EventName = first(EventName), Reason= first(Reason), DOTNotes = first(DOTNotes),
                                                                                     day= first(day), flood = max(flood), wind = max(wind), winter = max(winter), fog = max(fog), rainevent = max(rainevent), winterevent = max(winterevent), cycloneevent = max(cycloneevent), noweather = max(noweather), edge_id = first(edge_id), way_id = first(way_id), match = first(match), oneway = first(oneway)) %>% 
                                                                                      mutate(dur = as.numeric(end-start,"days"), updated_ID = as.numeric(updated_ID), start_day = date(start), end_day = date(end))



##manually remove ferry roads
##Sans Souci Ferry, Parker's Ferry, Roundabout construction (has water in text), Elwell Ferry
interim_final <- agg_events %>% filter(!(IncidentID %in% c(507271, 491382, 495315, 494975, 662235, 660068)))

##these events were identified by manually going through high duration closures in TIMS and looking for anomolies. An anomoly can be many things. FOr instance, a road closed due to 
##flooding that is closed for a long period of time but has no mention of washout, a closure that just has a single entry in TIMS where it was closed for a long period of time and the created
##on was at the beginning of the closure, many closures for an event that were ended at the same exact high duration time. Basically, I went through TIMS manually and made sure that high
##duration closures had continous updates and it didn't seem like a TIMS error and isolated all those that seemed like an error. 
fix_flood_other <- c(5446331, 4744241, 4925861, 4986961, 5056361, 4991591, 5144821, 5474051, 5511131, 5673051, 5432431, 5509661, 5509681, 5433611,
                     5455421, 5431741, 5432831, 5433311, 5434801, 5434831, 5434871, 5435951, 5436251, 5436991, 5437401, 5442111, 6243481, 4927181,
                     5049891, 5470241, 5455581, 5995501, 5955321, 6173761, 4919051, 5841981, 5805831, 5891701, 4097011, 5945471, 5894921, 5832621,
                     5349201, 4685461, 5428041, 4068791, 5461761, 4136131, 4946631, 5450831, 4947611, 4947641, 4932371, 5839761, 4255401, 4933691,
                     4942871, 5992001, 5448781, 4932281, 4248051, 5491221, 4953801, 5428381, 4745091, 4939331, 4928541, 5439441, 4927501, 4937941,
                     4938131, 4925361)

###THese were washouts that did not have continous updates or were all opened on the same high duration day. 
fix_washout <- c(4925861, 4986961,5989991,5990131, 4949851)

#get those that don't need to be fixed
interim_final_nofix <- interim_final %>% filter(!(updated_ID %in% fix_flood_other | updated_ID %in% fix_washout)) %>% select(updated_ID, IncidentID, RoadName, CommonName, Latitude, Longitude, EventName, flood, wind, winter, fog, rainevent, winterevent, cycloneevent, noweather, edge_id, way_id, match, oneway, start_day, end_day, dur)

#get those that do need to be fixed
interim_final_fix <- interim_final %>% filter(updated_ID %in% fix_flood_other | updated_ID %in% fix_washout)

#get event names for ones that do need to be fixed
test <- unique(interim_final_fix$EventName)

#get mean closure time for all the events that will then be used to assign a new end date
matthew <- round(mean(interim_final_nofix[interim_final_nofix$EventName == "Hurrricane Matthew", ]$dur))
sandy <- round(mean(interim_final_nofix[interim_final_nofix$EventName == "Hurricane Sandy 2012", ]$dur))
joaquin <- round(mean(interim_final_nofix[interim_final_nofix$EventName == "Hurricane Joaquin", ]$dur))
flooding_2017 <- round(mean(interim_final_nofix[interim_final_nofix$EventName == "4/24-4/25 2017 Flooding", ]$dur))
flooding_2018 <- round(mean(interim_final_nofix[interim_final_nofix$EventName == "Western Flooding May 2018", ]$dur))
florence <- round(mean(interim_final_nofix[interim_final_nofix$EventName == "Hurricane Florence", ]$dur))
michael <- round(mean(interim_final_nofix[interim_final_nofix$EventName == "Hurricane Michael", ]$dur))
flooding_2020 <- round(mean(interim_final_nofix[interim_final_nofix$EventName == "2/6/20 Rain Event", ]$dur))
rain <- round(mean(interim_final_nofix[interim_final_nofix$EventName == "Rain Event Beginning 11/11.", ]$dur))
none <- 1
washout <- 30

#assign a new end date based on mean closure times
final_fix <- interim_final_fix %>% mutate(add = case_when(EventName =="Hurrricane Matthew" ~ matthew,
                                                          EventName == "Hurricane Sandy 2012" ~ sandy,
                                                          EventName == "Hurricane Joaquin" ~ joaquin,
                                                          EventName == "4/24-4/25 2017 Flooding" ~ flooding_2017,
                                                          EventName == "Western Flooding May 2018" ~ flooding_2018,
                                                          EventName == "Hurricane Florence" ~ florence,
                                                          EventName == "Hurricane Michael" ~ michael,
                                                          EventName == "2/6/20 Rain Event" ~ flooding_2020,
                                                          EventName == "Rain Event Beginning 11/11." ~ rain,
                                                          EventName == "None"~ none)) %>% mutate(add = ifelse(updated_ID %in% fix_washout, 30, add), end_day = start_day + add, dur = as.numeric(end_day -start_day,"days")) %>% select(updated_ID, IncidentID, RoadName, CommonName, Latitude, Longitude, EventName, flood, wind, winter, fog, rainevent, winterevent, cycloneevent, noweather, edge_id, way_id, match, oneway, start_day, end_day, dur)


#create new full dataset
merged_final <- rbind(final_fix, interim_final_nofix)  

#save new full dataset
write_csv(merged_final, "data/final_road_closure.csv")
