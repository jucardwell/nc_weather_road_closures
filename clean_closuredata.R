#this script does initial cleaning of TIMS data. Much of what is in this script needed to be determined by manually examining the data files

##load libraries
library(tidyverse)
library(lubridate)


##read in closure data. Fill in with road closure data filtered to weather events
TIMS_weather_data <- read_csv("") 

#read in obstruction data
TIMS_obstruction_data <- read_csv("") 


#set up strings to match for any kind of weather
y <- c('\\bice\\b', '\\bIce\\b','\\bsnow\\b', '\\bSnow\\b', '\\bwinter\\b', '\\bWinter\\b', 
       '\\bWind\\b', '\\bwind\\b', '\\btree\\b', '\\bTree\\b',  '\\btrees\\b', '\\bTrees\\b',
       '\\bflood\\b', '\\bFlood\\b', '\\bflooding\\b', '\\bFlooding\\b', '\\bFLOODING\\b', '\\bFLOOD\\b', '\\FLOODED\\b', '\\bflooded\\b','\\bFlooded\\b', '\\bwater\\b', '\\bWater\\b', '\\bstormwater\\b', '\\bStormwater\\b', '\\bwash\\b', '\\bWash\\b', '\\bwashed\\b', '\\bWashed\\b', '\\bwashout\\b', '\\boverwash\\b', '\\brain\\b', '\\bRain\\b', '\\brainy\\b', '\\brains\\b', '\\bRains\\b', '\\bsurge\\b', '\\bHurricane\\b', '\\bTropical\\b', '\\bhurricane\\b', '\\btropical\\b',
       '\\bfog\\b', '\\bFog\\b', '\\bvisibility\\b', '\\bVisibility\\b')


#add logical weather variables
TIMS_obstruction_data$weather1 <- grepl(paste(y, collapse = "|"), TIMS_obstruction_data$Reason)
TIMS_obstruction_data$weather2 <- grepl(paste(y, collapse = "|"), TIMS_obstruction_data$DOTNotes)
TIMS_obstruction_data$weather3 <- grepl(paste(y, collapse = "|"), TIMS_obstruction_data$EventName)


#select only weather obstructions
TIMS_obstruction_weather <- TIMS_obstruction_data %>% filter(weather1 == TRUE | weather2== TRUE | weather3 == TRUE) %>% select(-c(weather1, weather2, weather3))

#create full weather closure 
TIMS_data <- rbind(TIMS_weather_data, TIMS_obstruction_weather)

#summarize events, manually extract and categorize events (see categorizing_events_07052023.xlxs)
summ <- TIMS_data %>% group_by(EventName) %>% summarise(event = first(EventName))

winterweatherevents <- c('1/6-1/7 2017 Winter Weath', '2/20-2/21 2020 Snow', '2022 Dec 22-23 Winter Weather', 
                         '2022 Jan 3 Storm', '2022 January 16 Storm', '2022 January 21 Winter Event', '2022 January 28 Winter Event',
                         '2023 Feb 11-12 Winter Weather', 'Ice Event 1/14/2015', 'Snow Event 1/28/14', 'Snow Event 2/11/14', 
                         'Winter Storm 3/3/14', 'Winter Weather 01/16/18', 'Winter Weather 1/03/2018',
                         'Winter Weather 1/17/2013', 'Winter Weather 1/21/16', 'Winter Weather 1/25/2013',
                         'Winter Weather 1/5/17', 'Winter Weather 12/8 - 12/', 'Winter Weather 2-16-2015',
                         'Winter Weather 2/24-26/15', 'Winter Weather October 2012')
  
rainfloodevents <- c('2/6/20 Rain Event', '2023 June 20-23 Rain Event', '4/24-4/25 2017 Flooding', 'Rain Event Beginning 11/11.',
                     'Western Flooding May 2018', 'Western NC Flood May 2013')
  
tropicalcyloneevents <- c('2020-08-01 Hurricane Isaias', '2021 Elsa Severe Weather', '2021 Fred Severe Weather', 
                          '2021 Ida Severe Weather', '2022 Ian Tropical Storm', '2022 Nicole Tropical Storm', 
                          'Hurricane Delta', 'Hurricane Dorian', 'Hurricane Florence', 'Hurricane Irene', 
                          'Hurricane Irma', 'Hurricane Joaquin', 'Hurricane Michael', 'Hurricane Sandy 2012',
                          'Hurrricane Matthew', 'Tropical Storm Andrea', 'Tropical Storm Arthur', 'Tropical Storm Hermine',
                          'Tropical Storm Zeta 10/29/2020')

#set up strings to match for flooding
flood <- c('\\bflood\\b', '\\bFlood\\b', '\\bflooding\\b', '\\bFlooding\\b', '\\bFLOODING\\b', '\\bFLOOD\\b', '\\FLOODED\\b', '\\bflooded\\b','\\bFlooded\\b', '\\bwater\\b', '\\bWater\\b', '\\bstormwater\\b', '\\bStormwater\\b', '\\bwash\\b', '\\bWash\\b', '\\bwashed\\b', '\\bWashed\\b', '\\bwashout\\b', '\\boverwash\\b', '\\brain\\b', '\\bRain\\b', '\\brainy\\b', '\\brains\\b', '\\bRains\\b', '\\bsurge\\b')

#add logical flooding variables
TIMS_data$flooding1 <- grepl(paste(flood, collapse = "|"), TIMS_data$Reason)
TIMS_data$flooding2 <- grepl(paste(flood, collapse = "|"), TIMS_data$DOTNotes)


#set up strings to match for wind
wind <- c('\\bWind\\b', '\\bwind\\b', '\\btree\\b', '\\bTree\\b',  '\\btrees\\b', '\\bTrees\\b', '\\bpowerline\\b', '\\bpower\\b', '\\bPowerline\\b', '\\bPower\\b', '\\bpowerlines\\b', '\\bPowerline\\b')

#add logical wind variables
TIMS_data$wind1 <- grepl(paste(wind, collapse = "|"), TIMS_data$Reason)
TIMS_data$wind2 <- grepl(paste(wind, collapse = "|"), TIMS_data$DOTNotes)

#set up strings to match for winter
winter <- c('\\bice\\b', '\\bIce\\b','\\bsnow\\b', '\\bSnow\\b', '\\bwinter\\b', '\\bWinter\\b')

#add logical winter variables
TIMS_data$winter1 <- grepl(paste(winter, collapse = "|"), TIMS_data$Reason)
TIMS_data$winter2 <- grepl(paste(winter, collapse = "|"), TIMS_data$DOTNotes)


#set up strings to match for fog
fog <- c('\\bfog\\b', '\\bFog\\b', '\\bvisibility\\b', '\\bVisibility\\b')

#add logical fog variables
TIMS_data$fog1 <- grepl(paste(fog, collapse = "|"), TIMS_data$Reason)
TIMS_data$fog2 <- grepl(paste(fog, collapse = "|"), TIMS_data$DOTNotes)

#simplified variables
TIMS_data$flood <- ifelse(TIMS_data$flooding1==TRUE | TIMS_data$flooding2==TRUE, TRUE, FALSE)

TIMS_data$wind <- ifelse(TIMS_data$wind1==TRUE | TIMS_data$wind2==TRUE, TRUE, FALSE)

TIMS_data$winter <- ifelse(TIMS_data$winter1==TRUE | TIMS_data$winter2 == TRUE, TRUE, FALSE)

TIMS_data$fog <- ifelse(TIMS_data$fog1==TRUE | TIMS_data$fog2==TRUE, TRUE, FALSE)

TIMS_data$winterevent <- ifelse(TIMS_data$EventName %in% winterweatherevents, TRUE, FALSE)

TIMS_data$rainevent <- ifelse(TIMS_data$EventName %in% rainfloodevents, TRUE, FALSE)

TIMS_data$cycloneevent <- ifelse(TIMS_data$EventName %in% tropicalcyloneevents, TRUE, FALSE)


#add correct date-time
TIMS_data_datetime <- TIMS_data %>% mutate(updated_start_time = strptime(`StartTime (EST)`, format = "%m/%d/%Y %I:%M:%S %p", tz ="EST"), 
                                                 updated_end_time = strptime(`EndTime (EST)`, format = "%m/%d/%Y %I:%M:%S %p", tz ="EST"), 
                                                 day = format(updated_start_time, "%Y-%m-%d"))

##get a list of unique clusters
list_events <- unique(TIMS_data_datetime$IncidentID)

##init blank dataframe
closure_events = data.frame()                                     

#filter out events with just lane closure, etc. (aka no full road closure at any time)
for (value in list_events) {
  filtered_obs <- subset(TIMS_data_datetime, IncidentID == value)
  closure = sum(filtered_obs$ConditionName == 'Road Closed' | filtered_obs$ConditionName == 'Road Impassable' | filtered_obs$ConditionName == 'Road Closed with Detour')
  if (closure > 0) {
    closure_events <- rbind(closure_events,filtered_obs)
  }
}



#aggregate events, take the earliest start and the latest end
agg_closure_events <- closure_events %>% group_by(IncidentID) %>% summarise(start = min(updated_start_time), end = max(updated_end_time), RoadName = first(RoadName), 
                                                                            CommonName = first(CommonName), Direction= first(Direction), CrossStreetName = first(CrossStreetName), 
                                                                            CrossStCommonName = first(CrossStCommonName), Latitude= first(Latitude), Longitude = first(Longitude), ConditionName= first(ConditionName), 
                                                                            IncidentType = first(`Incident Type`), Event = first(EventName), EventName = first(EventName), Reason= first(Reason), DOTNotes = first(DOTNotes),
                                                                            day= first(day), flood = max(flood), wind = max(wind), winter = max(winter), fog = max(fog), rainevent = max(rainevent), winterevent = max(winterevent), cycloneevent = max(cycloneevent)) %>% mutate(dur = as.numeric(end-start,"days"), noweather = ifelse(flood == 0 & wind == 0 & winter == 0 & fog == 0 & rainevent == 0 & cycloneevent == 0 & winterevent == 0, 1, 0))


#write aggregated events as a .csv
write_csv(agg_closure_events, "data/aggregated_road_closuress.csv")

