# This script compiles all raw data for the robotics runs from the bag csv files, the rosmav files, and the waypoints files. 
# Note, the completed metadata template is also required so that the proper waypoints files are associated with the rosmav files.
# If you have not yet run 0.compile_metadata.R go do that before you run this script.

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

# point to directories ####
lake_dir = paste0(path_pat, 'project_data/ASV_data/raw_csv_data/')
path_dir = paste0(path_pat, 'project_data/ASV_data/raw_path_data/')
meta_dir = paste0(path_pat, 'project_data/ASV_data/metadata/')
comp_dir = paste0(path_pat, 'project_data/ASV_data/compiled/')
inter_dir = paste0(path_pat, 'project_data/ASV_data/intermediary/')
proc_dir = paste0(path_pat, 'project_data/ASV_data/analysis_ready/')
parent_dir = paste0(path_pat, 'project_data/ASV_data/')

# read in metadata and select columns for use in this script ####
metadata <- read.csv(file.path(comp_dir, 'compiled_ASV_deployment_general_info.csv')) 

#grab data for asv processing ####
metadata <- metadata %>% 
  select(date, lake, lab, deployment_type, equipment,
         deployment_instance, deployment_starttime, deployment_endtime,
         path_filename, ASV_processed_filename, rosmav_missionreached_filename) %>% 
  rename(rosmav_filename = rosmav_missionreached_filename) %>% 
  filter(!is.na(rosmav_filename))

# list lakes in lake directory ####
lake_list <- dir(lake_dir, recursive = F)

# create a matrix of lakes, years, dates of data to work through the folder system ####
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

deployment_date_list <- deployment_date_list %>% 
  select(lake, year, date_list) 

rm(compiled_lake_list)

write.csv(deployment_date_list, file.path(parent_dir, paste0('ASV_deployment_date_list.csv')), row.names = F)


# open and save bag files into main dataframe ####
file_list <- dir(lake_dir, pattern = ('.csv'),  recursive = T)

# break out the bag and rosmav files
bag_csv <- file_list[!grepl('rosmav', file_list)]
rosmav_csv <- file_list[grepl('rosmav', file_list)]

# # remove file names that have already been collated - need to add this for next round, no need to read things in again
# #read in the names of files already incorporated
# incorp_bag_filelist <- readRDS(file.path(inter_dir,'bag_file_list.RDS'))
# incorp_rosmav_filelist <- readRDS(file.path(inter_dir,'rosmav_file_list.RDS'))
# 
# #saverds
# saveRDS(bag_csv, file.path(inter_dir,'bag_file_list.RDS'))
# saveRDS(rosmav_csv, file.path(inter_dir,'rosmav_file_list.RDS'))
# 
# #remove already-incorporated files
# bag_csv <- bag_csv[incorp_bag_filelist]
# rosmav_csv <- rosmav_csv[incorp_rosmav_filelist]


# read in the translate table for existing names to cv
bag_colnames_cv <- read.csv(file.path(meta_dir, 'colnames_cv/bag_colnames_cvnames.csv')) %>% 
  rename(df_names = column_names)

#collate bag data with CV
for(l in 1:length(bag_csv)) {
  df = read.csv(file.path(lake_dir, bag_csv[l])) 
  
  #get column names and change to CV
  df_names = names(df)
  df_names = data.frame(df_names)
  df_names <- left_join(df_names, bag_colnames_cv)
  names(df) = df_names$CV_units 
  
  #add filepath
  df <- df %>% 
    mutate(bag_file_path = bag_csv[l])
  
  print(bag_csv[l])
  if(l == 1){
    all_asv_data = df
  } else {
    all_asv_data = full_join(all_asv_data, df)
  }
}

#grab lake, year, dep date
all_asv_data <- all_asv_data %>% 
  mutate(lake = substr(bag_file_path, 1, 3),
    year = substr(bag_file_path, 5, 8),
    date = substr(bag_file_path, 10, 19),
    ASV_processed_filename = substr(bag_file_path, 21, nchar(bag_file_path)))

names(all_asv_data)
names(metadata)

#add metadata
all_asv_data <- all_asv_data %>% 
  left_join(., metadata)

#write asv data as RDS (it's a huge file and it takes a minute)
saveRDS(all_asv_data, file.path(inter_dir, 'asv_data_raw.RDS'))

# read in rosmav data ####

# read in the translate table for existing names to cv
rosmav_colnames_cv <- read.csv(file.path(meta_dir, 'colnames_cv/rosmav_colnames_cvnames.csv')) %>% 
  rename(df_names = column_names)

for(y in 1:length(rosmav_csv)) {
  df = read.csv(file.path(lake_dir, rosmav_csv[y])) 
  
  #get column names and change to CV
  df_names = names(df)
  df_names = data.frame(df_names)
  df_names <- left_join(df_names, rosmav_colnames_cv)
  names(df) = df_names$cv_units 
  
  #add filepath
  df <- df %>% 
    mutate(rosmav_file_path = rosmav_csv[y])
  
  print(rosmav_csv[y])
  if(y == 1){
    all_rosmav = df
  } else {
    all_rosmav = full_join(all_rosmav, df)
  }
}

#grab lake, year, dep date
all_rosmav <- all_rosmav %>% 
  mutate(lake = substr(rosmav_file_path, 1, 3),
         year = substr(rosmav_file_path, 5, 8),
         date = substr(rosmav_file_path, 10, 19),
         rosmav_filename = substr(rosmav_file_path, 21, nchar(rosmav_file_path)))

#join with metadata
all_rosmav <- all_rosmav %>% 
  left_join(., metadata)%>% 
  filter(!is.na(rosmav_filename))



# read in the waypoints files ####
wp_list <- dir(path_dir, pattern = ('.waypoints'),  recursive = T)

# #remove file names that have already been collated - need to add this for next round, no need to read things in again
# #read in the names of files already incorporated
# incorp_wp_filelist <- readRDS(file.path(inter_dir,'waypoint_file_list.RDS'))
# 
# #list files in meta dir
# saveRDS(wp_list, file.path(inter_dir,'waypoint_file_list.RDS'))
# 
# #remove already-incorporated files
# wp_list <- wp_list[incorp_wp_filelist]


wp_colnames = c('waypoint_seq', 'wp_seq', 'wp_coord_frame', 'wp_command', 
                'wp_param1', 'wp_param2', 'wp_param3', 'wp_param4', 'wp_param_lat', 
                'wp_param_long', 'wp_param_alt', 'wp_autocontinue')


for(k in 1:length(wp_list)) {
  df = read.delim(file.path(path_dir, wp_list[k]),
                  sep = '\t',
                  skip = 1,
                col.names = wp_colnames) %>% 
    mutate(wp_file_path = wp_list[k])
  print(wp_list[k])
  if(k == 1){
    all_wp = df
  } else {
    all_wp = full_join(all_wp, df)
  }
}

#grab lake, year, dep date
all_wp <- all_wp %>% 
  mutate(lake = substr(wp_file_path, 1, 3),
         path_filename = substr(wp_file_path, 5, nchar(wp_file_path))) %>% 
  select(lake, path_filename, waypoint_seq, wp_command, wp_param1)

#join with metadata
all_wp <- all_wp %>% 
  left_join(., metadata) 

# collate and save rosmav and waypoint files ####

#join rosmav and waypoints files
rosmav_wp <- full_join(all_rosmav, all_wp) %>% 
  filter(!is.na(datetime)) #drop data from paths that didn't complete or record

#grab only the columns we need
rosmav_wp <- rosmav_wp %>% 
  select(lake, year, date, lab, deployment_type, equipment, deployment_instance, deployment_starttime, deployment_endtime,
         timestamp_header_sec, header_seq, waypoint_seq, wp_command, wp_param1,
         rosmav_filename, path_filename, ASV_processed_filename)

# save rosmav_wp file
write.csv(rosmav_wp, file.path(inter_dir, 'rosmav_waypoints_allmissions.csv'), row.names = F)
