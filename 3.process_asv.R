# This script processes new ASV deployments read in in previous scripts in this repo. 

# Processing is comprised of:
# 1) truncating pre-mission and post-mission data
# 2) flagging waypoints
# 3) flagging loiters

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
  
  #remove data before header seq 0, intiation of mission
  ix = which(asv_wp$header_seq == 0)
  seq0_wp = asv_wp$timestamp_gps_sec[ix]
  asv_wp_trunc <- asv_wp %>% 
    filter(timestamp_gps_sec >= seq0_wp)
  
  #truncate record after last WP
  last_headerseq = max(asv_wp$header_seq, na.rm = T)
  zx = which(asv_wp$header_seq == last_headerseq)
  lastseq_wp = asv$timestamp_gps_sec[zx]
  asv_wp_trunc <- asv_wp_trunc %>% 
    filter(timestamp_gps_sec<lastseq_wp)

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
  
    #get lake name and deployment dates
  lake_folder = asv_wp_trunc$lake[1]
  deployment_date = asv_wp_trunc$date[1]
  
  #write file to processed folder
  fwrite(asv_wp_trunc, file.path(proc_dir, lake_folder, paste0(lake_folder, '_', deployment_date, '_asv_processed.csv')), row.names = F)
  message(paste0('file processed and saved as ',lake_folder, '_', deployment_date, '_asv_processed.csv'))
  message('')#left intnetionally blank to create space between files
}
