# Add flags to run based on visualizations of the data in asv_mapping repo

# detach all libraries
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

#load libraries
library(data.table)
library(tidyverse)

# determine OS for automatic filepath determination
os <- osVersion
path_pat = NULL

if (grepl('win', os, ignore.case = T) == T ){
  path_pat = 'Z:/'
  message('Windows OS detected.')
} else if (grepl('mac', os, ignore.case = T) == T ){
  path_pat = '/Volumes/EpscorBlooms/'
  message('Mac OS detected')
} else {
  message('OS path pattern not detected. Please store OS path pattern manually.')
}

# point to directories
data_dir = paste0(path_pat, 'project_data/ASV_data/analysis_ready/')


## Sunapee 2021-08-27 HC ----

#robot came home between WP 14&16

#read in processed file
sun210827hc <- read.csv(file.path(data_dir, 'SUN/SUN_2021-08-27_HC_asv_processed_v2022-01-24.csv'))

#check flag names
names_col = colnames(sun210827hc)
names_col[grepl('flag', names_col)]

#get time of wp 14 and wp16
wp14 = which(sun210827hc$waypoint_seq==14)
wp14 = sun210827hc$timestamp_gps_sec[wp14]

wp16 = which(sun210827hc$waypoint_seq==16)
wp16 = sun210827hc$timestamp_gps_sec[wp16]

#create a flag for run
sun210827hc <- sun210827hc %>% 
  mutate(run_flag = case_when(timestamp_gps_sec> wp14 & timestamp_gps_sec<wp16 ~ 'off mission to home',
                              TRUE ~ ''))

#write new file
fwrite(sun210827hc, file.path(data_dir, 'SUN/SUN_2021-08-27_HC_asv_processed_v2022-01-31.csv'))

## Sunapee 2021-06-11 HC ----

#robot came home between WP 20&21

#read in processed file
sun210611hc <- read.csv(file.path(data_dir, 'SUN/SUN_2021-06-11_HC_asv_processed_v2022-01-24.csv'))

#check flag names
names_col = colnames(sun210827hc)
names_col[grepl('flag', names_col)]

#get time of wp 14 and wp16
wp20 = which(sun210611hc$waypoint_seq==20)
wp20 = sun210611hc$timestamp_gps_sec[wp20]

wp21 = which(sun210611hc$waypoint_seq==21)
wp21 = sun210611hc$timestamp_gps_sec[wp21]

#create a flag for run
sun210611hc <- sun210611hc %>% 
  mutate(run_flag = case_when(timestamp_gps_sec> wp20 & timestamp_gps_sec<wp21 ~ 'off mission to home',
                              TRUE ~ ''))
#write new file
fwrite(sun210827hc, file.path(data_dir, 'SUN/SUN_2021-06-11_HC_asv_processed_v2022-01-31.csv'))

