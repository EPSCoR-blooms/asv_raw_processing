# This script compiles all metadata for the robotics runs from the ASV template.

#load libraries
library(tidyverse)
library(readxl)

# determine OS for automatic filepath determination
os <- osVersion
path_pat = NULL

if (grepl('win', os, ignore.case = T) == T ){
  path_pat = 'Z:/'
  } else if (grepl('mac', os, ignore.case = T) == T ){
  path_pat = '/Volumes/EpscorBlooms/'
  } else {
    message('OS path pattern not detected. Please store OS path pattern manually.')
  }

# point to directories
lake_dir = paste0(path_pat, 'project_data/ASV_data/raw_data/')
dump_dir = paste0(path_pat, 'project_data/ASV_data/compiled/')

#list lakes in lake directory
lake_list <- dir(lake_dir, recursive = F)

#create a matrix of lakes, years, dates of data to work through the folder system
for(i in 1:length(lake_list)) {
  year_list = dir(file.path(lake_dir, lake_list[i]), recursive = F)
  for(x in 1:length(year_list)) {
    year_lake_list = c()
    year_lake_list$lake = lake_list[i]
    year_lake_list$year = year_list[x]
    df = as.data.frame(year_lake_list)
    if(x == 1) {
      df_compiled_list <-  df
    } else {
      df_compiled_list <- full_join(df_compiled_list, df)
    }
  }    
  rm(df)
  df_all <-  df_compiled_list
  rm(df_compiled_list)
  if(i == 1) {
    compiled_lake_list <-  df_all
  } else {
    compiled_lake_list <-  full_join(compiled_lake_list, df_all)
  }
  rm(df_all, year_lake_list)
}

#function to get the folder list for each row of the above matrix
get_deployment_dates <- function(lake, year){
  list.files(file.path(lake_dir, lake, year))
}

#create a df of lake, year, deployment date
for (y in 1:nrow(compiled_lake_list)) {
  date_list <- get_deployment_dates(compiled_lake_list$lake[y], compiled_lake_list$year[y])
  date_list_df <- as.data.frame(date_list)
  date_list_df$lake = compiled_lake_list$lake[y]
  date_list_df$year = compiled_lake_list$year[y]
  if (y == 1) {
    deployment_date_list = date_list_df
  } else {
    deployment_date_list =full_join(deployment_date_list, date_list_df)
  }
  rm(date_list_df)
}

rm(compiled_lake_list)

write.csv(deployment_date_list, file.path(dump_dir, paste0('ASV_deployment_date_list_v', Sys.Date(), '.csv')), row.names = F)


# open and save files into main dataframe

file_list <- dir(lake_dir, pattern = ('.csv'),  recursive = T)

#remove file names that have already been collated - need to add this for next round, no need to read things in again
# list.files(dump_dir)
#get most recent version
# imported_list <- read.csv(most reacent version)
# summarize imported list; 
# remove those files from the file list



#collate data with CV
for(l in 1:length(file_list)) {
  df = read.csv(file.path(lake_dir, file_list[l])) %>% 
    mutate(file_path = file_list[l])
  print(file_list_2021[l])
  if(l == 1){
    all_asv_data = df
  } else {
    all_asv_data = full_join(all_asv_data, df)
  }
}

#grab lake, year, dep date
all_asv_data <- all_asv_data %>% 
  mutate(lake = substr(file_path, 1, 3),
    year = substr(file_path, 5, 8),
    deployment_date = substr(file_path, 10, 19))

#join with og file


#### change this to rename! and put in loop
#make list of column names
column_names <- names(all_asv_data) 
column_names <- as.data.frame(column_names)
column_names <- column_names %>% 
  mutate(CV_name_unit = case_when(column_names == 'gps_timestamp' | column_names == 'gpsspeed_timestamp' ~ 'timestamp_gps_ns',
                                  column_names == 'latitude' ~ 'latitude_deg',
                                  column_names == 'longitude' ~ 'longitude_deg',
                                  column_names == 'compass_timestamp' ~ 'timestamp_compass_ns',
                                  column_names == 'heading' ~ 'heading_deg',
                                  column_names == 'airspeed_timestamp' ~ 'timestamp_airspeed_ns',
                                  column_names == 'airspeed' ~ 'airspeed_mps', 
                                  column_names == 'sonar_timestamp' ~ 'timestamp_sonar_ns',
                                  column_names == 'distance' | column_names == 'sonar' ~ '', # need definition
                                  column_names == 'gps_velocity_timestamp' ~ 'timestamp_gpsvelocity_ns',
                                  column_names == 'gps_linear_x' ~ '', 
                                  column_names == 'gps_linear_y' ~ '',
                                  column_names == 'local_velocity_timestamp' ~ 'timestamp_localvelocity_ns',
                                  column_names == 'local_linear_x'| column_names == 'pose.pose.position.x' ~ '',
                                  column_names == 'local_linear_y'| column_names == 'pose.pose.position.y' ~ '',
                                  column_names == 'local_linear_z'| column_names == 'pose.pose.position.z' ~ '',
                                  column_names == 'local_angular_x'| column_names ==  'pose.pose.orientation.x' ~ '',
                                  column_names == 'local_angular_y'| column_names ==  'pose.pose.orientation.y' ~ '',
                                  column_names == 'local_angular_z' | column_names == 'pose.pose.orientation.z' ~ '',
                                  column_names == 'local_angular_z' | column_names == 'pose.pose.orientation.w' ~ '',
                                  column_names == 'sonde_timestamp' ~ 'timestamp_sonde_ns',
                                  column_names == 'Battery.V' ~ 'batteryVoltage_V',
                                  column_names == 'Temp.Â.C' | column_names == 'Temp..C' ~ 'temperatureWater_degC',
                                  column_names == 'Depth.m' |column_names == 'Depth..m' ~ 'depthSensor_sonde_m',
                                  column_names == 'Cond.ÂµS.cm'| column_names == 'Cond.µS.cm' ~ 'electricalConductivity_uscm',
                                  column_names == 'SpCond.mS.cm' ~ 'specificConductance_mscm',
                                  column_names == 'SpCond.ÂµS.cm'| column_names ==  'SpCond.µS.cm' ~ 'specificConductance_uscm',
                                  column_names == 'Chlorophyll.RFU' | column_names == 'Chlorophyll..µg.L' ~  'chlorophyll_a_RFU',
                                  column_names == 'ODO...sat' ~ 'oxygenDissolved_perc',
                                  column_names == 'ODO.mg.L' ~ 'oxygenDissolved_mgl', 
                                  column_names == 'Turbidity.NTU' ~ 'turbidity_NTU', 
                                  column_names == 'Salinity.PPT' ~ 'salinity_PPT',
                                  column_names == 'BGA.PC.µg.L' ~ 'blue_GreenAlgae_Cyanobacteria_Phycocyanin_ugl',
                                  column_names == 'TSS.mg.L' ~ 'solidsTotalSuspended_mgl',
                                  column_names == 'Wiper.Position.V' ~ '',
                                  TRUE ~ column_names))
#paste names on these
names(all_asv_data) = column_names$CV_name_unit

#harmonize for multiple col name changes
all_asv_data <- all_asv_data %>% 
  
  filter(~'')


write.csv(all_asv_data, file.path(dump_dir, paste0('ASV_raw_data_allmissions_v', Sys.Date(), '.csv')), row.names = F)

          