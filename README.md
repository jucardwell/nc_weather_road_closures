# nc_weather_road_closures
This repository contains the scripts utilized in analysis for manuscript "Impacts of weather-related road closures on habitual travel in North Carolina"

## **Software environment:** 
Software environment and package versions can be viewed in “environment.txt”

## **Data availability:**

**Road Closure Data:** TIMS closure data was downloaded from NCDOT’s TIMS system (https://tims.ncdot.gov/tims/V2/Login?ReturnUrl=%2ftims) on 07-05-2023. Downloading from TIMS requires TIMS admin access as an External User, which can be granted by contacting NCDOT Help Desk at 919-707-7000 or dothelp.gov. For replication of this project, a user would need to make their own request for the closure data. 

TIMS was queries using the “deep search” function, as suggested by NCDOT employee. Query filters were suggested by NCDOT employee. 
Incident Types: Weather Event, Road Obstruction
Condition: Road Closed, Road Closed with Detour, Road Impassable, Local Traffic Only

**SafeGraph Data:** Monthly patterns SafeGraph data from 2018-2022 was obtained through an academic research agreement. This raw data cannot be publicly shared. For replication of this project, a user would need to make their own request for the Safegraph data.

**North Carolina Block Group and County**: North Carolina block group and county shapefile can be downloaded from the United States Census Bureau (https://www.census.gov/geographies/mapping-files/2020/geo/tiger-line-file.html)

**Rurality Data:** Rurality data can be downloaded from https://scholarsjunction.msstate.edu/mafes-publications/1/

**2010 to 2020 Crosswalk Table:** Can be downloaded from IPUMS NHGIS (https://www.nhgis.org/geographic-crosswalks)

*Researchers who do not have access to SafeGraph data or TIMS data may need to seek alternative datasets or contact the respective data providers for access. Please note that replication may be limited by the availability of these specific data sources*

## **Folder setup:**
Project Directory/
├── data/
│   ├── daily_road_closures/
│   ├── safegraph/
├── output/
│   ├── local_travel/
│   ├── agg_local_travel/

## **Workflow:**
Note that some of the processes in this analysis were done outside of a scripting workflow. Those instances are denoted with a **

**Network Dataset Production Workflow:**
1.	**extract_osmnetwork.R** extracts osm data for “driving” mode for North Carolina via API. Saves “data/nc_road_network.shp”
2.	**save_weighted_network.R** removes “service roads” and weights the road network using the “motorcar” weighting scheme available in dodgr. Saves “data/nc_weighted_network.Rds”
3.	**save_sf_network.R** turns the weighted network into an sf object. This sf object is utilized to identify routable locations for census centroids. Creates “data/nc_sf_network.shp”

**Routable Location Workflow:**
1.	**Upload “data/nc_sf_network.shp” into QGIS and filter to only two-way roads on the largest connected component.
2.	**Use “Join by Nearest” processing tool between all locations and two-way largest components.
3.	**Export as .csv  “blockgroup_routable_nodeid.csv”
4.	**prep_routable_locations.R** to identify a single two-way, largest component node for each location. Creates “data/bg_simp_routable.csv”
5.	**baseline_local_routing.R** to identify un-routable locations.
6.	**Use QGIS to manually identify an acceptable node for each un-routable location. The un-routable locations were due to small topological errors. 
7.	**Export final routable files. “data/bg_simp_routable_manual.csv”

**Road Closure Location Workflow:** 
1.	**clean_closuredata.R** to process raw closure data. Creates "data/aggregated_road_closures.csv"
2.	**Upload road closure data (“data/aggregated_road_closures.csv”) to QGIS 
3.	**Buffer each closure by 75ft in QGIS.
4.	**Executed “Spatial Join” in QGIS between buffered road closures and nc_sf_network.shp.
5.	**Export spatial join output as joined_closures.csv.
6.	**road_closure_processing.R** processes spatial join output and creates “data/matched_closures.csv” and “data/unmatched_closures.csv”.
7.	**identify_nonmatch_roadclosures.R** identifies a closure for each closure that was not matched during the spatial join. Creates "data/unmatched_matched.csv"
8.	**Manually examine all closures with a “one-way” tag in QGIS to make sure the correct segments were selected. Create “data/manual_matched.csv” and “data/manual_unmatched_matched.csv”
9.	**datecheck_extract.R** to select updated end date for closures. Creates "data/final_road_closure.csv"
10.	**segment_match_dodgr_sf.R** to re-identify edge_ids that match the contracted dodgr_graph using distance, to node and from node, that are consistent between the two versions. Creates “data/dodgr_id_road_closures.csv”
11.	**closure_day_selection.R** to create a .csv with closures for each day from 01-01-2016 to 07-05-2023.

**Safegraph Workflow:**
1.	**create_yearly_safegraph_OD.R** to aggregate monthly Safegraph data into yearly files. Creates “data/safegraph/[year]_2010cbg_home.csv”
2.	**aggregate_yearly_safegraph.R** to aggregate yearly safegraph data to the full study period. Creates “data/safegraph/fullsafegraph.csv”
3.	**local_area_2010_2020.R** to convert 2010 home census block groups to 2020 and select habitual destinations. Creates "data/local_travel_od.csv".

**Routing Workflow:**
1.	Re-run **baseline_local_routing.R** to create finalized baseline routing scenario for habitual bgs. Creates "data/local_travel_baseline.csv"
2.	**daily_routing.R** to create a daily routing change file for every day in the study period. 

**Analysis Workflow:**
1.	**aggregate_local_travel_closure.R** to create a daily dataset with one row per census block group
2.	**daily_closure_aggregate.R** to create a full analysis dataset “output/aggregated_closure_impact.csv”
3.	**daily_closure_analysis.R** to re-create figures and analysis in the manuscript. 

