# This script compiles all raw data for the robotics runs from the bag csv files, the rosmav files, and the waypoints files. 
# Note, the completed metadata template is also required so that the proper waypoints files are associated with the rosmav files.

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
dump_dir = paste0(path_pat, 'project_data/ASV_data/compiled/')

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

write.csv(deployment_date_list, file.path(dump_dir, paste0('ASV_deployment_date_list.csv')), row.names = F)


# open and save bag files into main dataframe ####
file_list <- dir(lake_dir, pattern = ('.csv'),  recursive = T)

# break out the bag and rosmav files
bag_csv <- file_list[!grepl('rosmav', file_list)]
rosmav_csv <- file_list[grepl('rosmav', file_list)]

#remove file names that have already been collated - need to add this for next round, no need to read things in again
# list.files(dump_dir)
#get most recent version
# imported_list <- read.csv(most recent version)
# summarize imported list; 
# remove those files from the file list


# read in the translate table for existing names to cv
bag_colnames_cv <- read.csv(file.path(meta_dir, 'bag_colnames_cvnames.csv')) %>% 
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
    deployment_date = substr(bag_file_path, 10, 19),
    bag_orig_file = substr(bag_file_path, 21, nchar(bag_file_path)))

## collate rosmav data with asv data ####

# read in the translate table for existing names to cv
rosmav_colnames_cv <- read.csv(file.path(meta_dir, 'rosmav_colnames_cvnames.csv')) %>% 
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

names(all_rosmav)

#grab lake, year, dep date
all_rosmav <- all_rosmav %>% 
  mutate(lake = substr(rosmav_file_path, 1, 3),
         year = substr(rosmav_file_path, 5, 8),
         deployment_date = substr(rosmav_file_path, 10, 19),
         rosmav_orig_file = substr(rosmav_file_path, 21, nchar(rosmav_file_path)))

all_rosmav

## grab the waypoints files ####
wp_list <- dir(lake_dir, pattern = ('.waypoints'),  recursive = T)

#remove file names that have already been collated - need to add this for next round, no need to read things in again
# list.files(dump_dir)
#get most recent version
# imported_list <- read.csv(most reacent version)
# summarize imported list; 
# remove those files from the file list


wp_colnames = c('wp_index', 'waypoint_seq', 'wp_coord_frame', 'wp_command', 
                'wp_param1', 'wp_param2', 'wp_param3', 'wp_param4', 'wp_param_lat', 
                'wp_param_long', 'wp_param_alt', 'wp_autocontinue')


for(k in 1:length(wp_list)) {
  df = read.delim(file.path(lake_dir, wp_list[k]),
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
         year = substr(wp_file_path, 5, 8),
         deployment_date = substr(wp_file_path, 10, 19),
         wp_orig_file = substr(wp_file_path, 21, nchar(wp_file_path)))

## collate files ####

#might need to read in the metadata for the proper wp file association (names don't always match)



## truncate before WP1; add flag for loiter ####


#join with existing file ####

# save file ####
write.csv(all_asv_data, file.path(dump_dir, paste0('ASV_raw_data_allmissions_v', Sys.Date(), '.csv')), row.names = F)

          