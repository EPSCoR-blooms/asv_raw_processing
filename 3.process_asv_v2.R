# This script processes new ASV deployments read in in previous scripts in this repo. 

# Processing is comprised of:
# 1) truncating pre-mission and post-mission data
# 2) flagging waypoints
# 3) flagging loiters

#unload libraries
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

# point to directories ####
comp_dir = paste0(path_pat, 'project_data/ASV_data/compiled/')
inter_dir = paste0(path_pat, 'project_data/ASV_data/intermediary/')
proc_dir = paste0(path_pat, 'project_data/ASV_data/analysis_ready/')

# read in files
asv_data_raw <- readRDS(file.path(inter_dir, 'asv_data_raw.RDS')) %>% 
  mutate(year = as.numeric(year))

rosmav_waypoints <- read.csv(file.path(inter_dir, 'rosmav_waypoints_allmissions.csv')) %>% 
  rename(timestamp_gps_sec = timestamp_header_sec) 

# process each asv deployment one at a time ----
deployment_list = unique(asv_data_raw$ASV_processed_filename)

for(i in 1:length(deployment_list)) {
  message(paste0('processing raw asv data from file ', deployment_list[i]))
  
  #subset asv data
  asv <- asv_data_raw %>% 
    filter(ASV_processed_filename == deployment_list[i]) %>% 
    mutate(timestamp_gps_sec_integer = as.integer(timestamp_gps_sec)) 
  #subset wp data
  wp <- rosmav_waypoints %>% 
    filter(ASV_processed_filename == deployment_list[i]) %>% 
    rename(timestamp_gps_sec_integer = timestamp_gps_sec) %>% 
    mutate(timestamp_gps_sec_integer = as.integer(timestamp_gps_sec_integer))
  #join asv and wp data
  asv_wp <- left_join(asv,wp) %>% 
    arrange(timestamp_gps_sec) %>% 
    select(-timestamp_gps_sec_integer)
  
  #remove data before first waypoint of mission (1 or minimum waypoint)
  wpsq_min = min(asv_wp$waypoint_seq, na.rm = T)
  
  ix = which(asv_wp$waypoint_seq == wpsq_min)
  
  #if there are 2 waypoint #1 and there was a test run at the beginning of the run, then choose the second wp for truncation
  if(length(ix) ==1){
    ix = ix[1]
  } else if (length(ix) > 1 & asv_wp$test_run[1] == 'y') {
    ix = ix[2]
  }
  
  #if there is a waypoint to start at other than 1 (from metadata), truncate at that waypoing
  if(!is.na(asv_wp$waypoint_start[1])) {
    ix = which(asv_wp$waypoint_seq == asv_wp$waypoint_start[1])
  } else {
    ix = ix
  }
  
  start_wp = asv_wp$timestamp_gps_sec[ix]
  asv_wp_trunc <- asv_wp %>% 
    filter(timestamp_gps_sec >= start_wp)

  # find out which is later, last waypoint or within 50m of deployment, or truncation of last minute
  
  # last waypoint method:
  last_headerseq = max(asv_wp_trunc$header_seq, na.rm = T)
  zx = which(asv_wp_trunc$header_seq == last_headerseq)
  lastseq_wp = asv_wp_trunc$timestamp_gps_sec[zx]
  
  # remove last 5 minutes:
  last_timestamp = max(asv_wp_trunc$timestamp_gps_sec, na.rm = T)-(5*60)
  df = asv_wp_trunc %>% 
    filter(timestamp_gps_sec < last_timestamp)
  mx = last(df$timestamp_gps_sec)
  lastseq_time = asv_wp_trunc$timestamp_gps_sec[asv_wp_trunc$timestamp_gps_sec == mx]
  
  # #within 50m method:
  # #get the lat and long of initiation of mission
  # orig_lat = asv_wp_trunc$latitude_gps_deg[1]
  # orig_lon = asv_wp_trunc$longitude_gps_deg[1]
  # 
  # # calculate approximate distance from origin point - use 111km per degree as is at equator for approximation
  # asv_wp_trunc <- asv_wp_trunc %>%
  #   mutate(lat_origin_diff_m = abs(latitude_gps_deg - orig_lat)*111*1000,
  #          long_origin_diff_m = abs(longitude_gps_deg - orig_lon)*111*1000)
  # 
  # yx = which(asv_wp_trunc$long_origin_diff_m < 50 & asv_wp_trunc$lat_origin_diff_m < 50)
  # #get nrow in df; zx will be in last half of data
  # half = round(0.50*nrow(asv_wp_trunc), 0)
  # #filter rows for last half of data, take first zx
  # yx = first(yx[yx>half])
  # lastseq_deg = asv$timestamp_gps_sec[yx]
  
  if(lastseq_wp > lastseq_time) {
    message('last waypoint timestamp occurs after 5 minute truncation, truncating at last waypoint')
    asv_wp_trunc <- asv_wp_trunc %>% 
      filter(timestamp_gps_sec<lastseq_wp)
    asv_wp_trunc$eor_trunc_method = 'last waypoint'
    asv_wp_trunc$eor_flag = ''
  } else {
    message('5 minute truncation occurs after last waypoint timestamp, truncating at 5 minute prior to last record')
    last_ts = asv_wp_trunc$timestamp_gps_sec[asv_wp_trunc$timestamp_gps_sec == mx]
    asv_wp_trunc <- asv_wp_trunc %>% 
      filter(timestamp_gps_sec <= last_ts)
    asv_wp_trunc$eor_trunc_method = '5 minutes'
    asv_wp_trunc <- asv_wp_trunc %>% 
      mutate(eor_flag = case_when(timestamp_gps_sec>=lastseq_wp ~ 'data collected after last programmed waypoint, use with caution',
                                  TRUE ~ ''))
    }

  #add flags for loiter
  lx = which(wp$wp_command == 19)
  loiter_end = wp$timestamp_gps_sec[lx]
  loiter_length = wp$wp_param1[lx]
  loiter_start = loiter_end-loiter_length
  
  #initialize loiter flag column
  asv_wp_trunc$loiter_flag = ''

  for(l in 1:length(loiter_start)){
    asv_wp_trunc <- asv_wp_trunc %>% 
      mutate(loiter_flag = case_when(timestamp_gps_sec >= loiter_start[l] & timestamp_gps_sec <= loiter_end[l] ~ 'l',
                                     TRUE ~ loiter_flag))
  }
  
  #add speed flag for movement faster than 1mps
  asv_wp_trunc$velocity_flag = ''
  sx = which(asv_wp_trunc$velocity_gps_mps > 1)
  asv_wp_trunc$velocity_flag[sx] = 'gt1mps'
  
  #get lake name and deployment dates
  lake_folder = asv_wp_trunc$lake[1]
  deployment_date = asv_wp_trunc$date[1]
  deployment_inst = asv_wp_trunc$deployment_instance[1]
  
  #re-order columns
  col_names_asv <- colnames(asv_wp_trunc)
  
  asv_wp_trunc <- asv_wp_trunc %>% 
    select(lake, year, date, lab, equipment, deployment_type, deployment_instance, eor_trunc_method, eor_flag, velocity_flag, loiter_flag, everything())
  
  #write file to processed folder
  fwrite(asv_wp_trunc, file.path(proc_dir, lake_folder, paste0(lake_folder, '_', deployment_date, '_', deployment_inst, '_asv_processed_v', Sys.Date(), '.csv')), row.names = F)
  message(paste0('file processed and saved as ',lake_folder, '_', deployment_date,'_', deployment_inst,  '_asv_processed_v', Sys.Date(), '.csv'))
  message('')#left intentionally blank to create space between files
  
  rm(ix)
}
