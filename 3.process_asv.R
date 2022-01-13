# This script processes new ASV deployments read in in previous scripts in this repo. 

# Processing is comprised of:
# 1) truncating pre-mission and post-mission data
# 2) flagging waypoints
# 3) flagging loiters

#NOTE: RESTART R BEFORE RUNNING THIS SCRIPT. data.table NEEDS TO BE LOADED BEFORE tidyverse. Go to 'Session', then click 'Restart R'.
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

# point to directories ####
comp_dir = paste0(path_pat, 'project_data/ASV_data/compiled/')
inter_dir = paste0(path_pat, 'project_data/ASV_data/intermediary/')
proc_dir = paste0(path_pat, 'project_data/ASV_data/analysis_ready/')

# read in files
asv_data_raw <- readRDS(file.path(inter_dir, 'asv_data_raw.RDS')) %>% 
  mutate(year = as.numeric(year))

rosmav_waypoints <- read.csv(file.path(inter_dir, 'rosmav_waypoints_allmissions.csv')) %>% 
  rename(timestamp_gps_ns = timestamp_header_sec)

# process each asv deployment one at a time ----

deployment_list = unique(asv_data_raw$ASV_processed_filename)

for(i in 1:length(deployment_list)) {
  message(paste0('processing raw asv data from file ', deployment_list[i]))
  
  #subset asv data
  asv <- asv_data_raw %>% 
    filter(ASV_processed_filename == deployment_list[i])
  #subset wp data
  wp <- rosmav_waypoints %>% 
    filter(ASV_processed_filename == deployment_list[i]) %>% 
    mutate()
  #join asv and wp data
  asv_wp <- full_join(wp, asv) %>% 
    arrange(timestamp_gps_ns)
  
  #remove data before header seq 0, intiation of mission
  ix = which(asv_wp$header_seq == 0)
  seq0_wp = asv_wp$timestamp_gps_ns[ix]
  asv_wp_trunc <- asv_wp %>% 
    filter(timestamp_gps_ns >= seq0_wp)
  
  #get the lat and long of initiation of mission
  orig_lat = round(asv_wp$latitude_deg[ix-1], digits = 3)
  orig_lon = round(asv_wp$longitude_deg[ix-1], digits = 3)
  
  #remove data at end of deployment, 
  zx = which(round(asv_wp_trunc$longitude_deg, digits = 3) == orig_lon & round(asv_wp_trunc$latitude_deg, digits = 3) == orig_lat)
  #get nrow in df; zx will be in last half of data
  half = round(0.50*nrow(asv_wp_trunc), 0)
  #filter rows for last half of data, take first zx
  zx = first(zx[zx>half])
  
  #add a safety measure in case there are no lat/lon matches in zx
  if(is.na(zx)){
    message('no data to truncate at end of record')
  } else {
    last_ts = asv_wp$timestamp_gps_ns[zx]
    asv_wp_trunc <- asv_wp_trunc %>% 
      filter(timestamp_gps_ns <= last_ts)
  }

  #add flags for loiter
  lx = which(wp$wp_command == 19)
  loiter_end = wp$timestamp_gps_ns[lx]
  loiter_length = wp$wp_param1[lx]
  loiter_start = loiter_end-loiter_length
  
  #initialize loiter flag column
  asv_wp_trunc$loiter_flag = ''

  for(l in 1:length(loiter_start)){
    asv_wp_trunc <- asv_wp_trunc %>% 
      mutate(loiter_flag = case_when(timestamp_gps_ns >= loiter_start[l] & timestamp_gpsspeed_ns <= loiter_end[l] ~ 'l',
                                     TRUE ~ loiter_flag))
  }
  
  # remove the wp rows
  asv_wp_trunc <- asv_wp_trunc %>% 
    filter(!is.na(latitude_deg))
  
  #get lake name and deployment dates
  lake_folder = asv_wp_trunc$lake[1]
  deployment_date = asv_wp_trunc$date[1]
  
  #write file to processed folder
  fwrite(asv_wp_trunc, file.path(proc_dir, lake_folder, paste0(lake_folder, '_', deployment_date, '_asv_processed.csv')), row.names = F)
  message(paste0('file processed and saved as ',lake_folder, '_', deployment_date, '_asv_processed.csv'))
  message('')
          
}
