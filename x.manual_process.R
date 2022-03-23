# Script to manually process asv runs

# This script pulls from the other scripts in this repo to process ASV runs that can't be processed in the automated workflow

## Set up workspace ----

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
comp_dir = paste0(path_pat, 'project_data/ASV_data/compiled/')
lake_dir = paste0(path_pat, 'project_data/ASV_data/manual_process/')
meta_dir = paste0(path_pat, 'project_data/ASV_data/metadata/')
path_dir = paste0(path_pat, 'project_data/ASV_data/raw_path_data/')
proc_dir = paste0(path_pat, 'project_data/ASV_data/analysis_ready/')

## Read in previously-created files required for processing ----
metadata <- read.csv(file.path(comp_dir, 'compiled_ASV_deployment_general_info.csv'))

#read in existing colnames_cvnames file
col_cv <- read.csv(file.path(meta_dir, 'colnames_cv/bag_colnames_cvnames.csv'))

## Read in files that need to be manually processed ----

# Note, you should only have to run the section of code that is pertinent to the run that requires manual processing. Do not overwrite other manually-processed runs.

### SUNAPEE 2021-07-22 ----
man_lake = 'SUN'
man_date = '2021-07-22'
sun20220722 <- metadata %>% 
  filter(lake == man_lake & date == man_date)

# get file names
asv_file <- sun20220722$ASV_processed_filename[1]
asv_year <- format(as.Date(man_date), '%Y')

#### check CV ####
asv_data <- read.csv(file.path(lake_dir, asv_file))
asv_col_for_cv <- colnames(asv_data)

asv_col_for_cv <- as.data.frame(unique(asv_col_for_cv))
colnames(asv_col_for_cv) = 'column_names'

#join new list with existing list
col_cv <- full_join(col_cv, asv_col_for_cv)

#save file
write.csv(col_cv, file.path(meta_dir, 'colnames_cv/bag_colnames_cvnames.csv'), row.names = F)

#see what needs cv
cv_needed <- col_cv %>% 
  filter(is.na(CV_units))

if(length(cv_needed$CV_units) > 0) {
  message('controlled vocabulary needed for bag file processing')
} else{
  message('all controlled vocabulary present for bag file processing')
}

#check the rosmav for CV
rosmav_file <- sun20220722$rosmav_missionreached_filename[1]

rosmav_data <- read.csv(file.path(lake_dir, rosmav_file))
rosmav_col_for_cv <- colnames(rosmav_data)

rosmav_col_for_cv <- as.data.frame(unique(rosmav_col_for_cv))
colnames(rosmav_col_for_cv) = 'column_names'

#read in existing colnames_cvnames file
rosmav_col_cv <- read.csv(file.path(meta_dir, 'colnames_cv/rosmav_colnames_cvnames.csv'))

#join new list with existing list
rosmav_col_cv <- full_join(rosmav_col_cv, rosmav_col_for_cv)

#save file
write.csv(rosmav_col_cv, file.path(meta_dir, 'colnames_cv/rosmav_colnames_cvnames.csv'), row.names = F)

#see what needs cv
cv_needed <- rosmav_col_cv %>% 
  filter(is.na(cv_units))

if(length(cv_needed$cv_units) > 0) {
  message('controlled vocabulary needed for bag file processing')
} else{
  message('all controlled vocabulary present for bag file processing')
}

#### Colnames to CV ----
asv_col_with_cv <- left_join(asv_col_for_cv, col_cv)
colnames(asv_data) = asv_col_with_cv$CV_units 
rosmav_col_with_cv <- left_join(rosmav_col_for_cv, rosmav_col_cv)
colnames(rosmav_data) = rosmav_col_with_cv$cv_units 

#add filepath
asv_data <- asv_data %>% 
  mutate(bag_file_path = file.path('manual_process', asv_file))
rosmav_data <- rosmav_data %>% 
  mutate(rosmav_file_path = file.path('manual_process', rosmav_file))

#grab lake, year, dep date
asv_data <- asv_data %>% 
  mutate(lake = man_lake,
         year = asv_year,
         date = man_date,
         ASV_processed_filename = asv_file)
rosmav_data <- rosmav_data %>% 
  mutate(lake = man_lake,
         year = asv_year,
         date = man_date,
         rosmav_filename = rosmav_file)

names(asv_data)
names(sun20220722)

#make sure there is only one observation and drop the waypoints file name column
sun20220722 <- sun20220722[1,] %>% 
  mutate(path_filename = NULL)

#add metadata
asv_data <- asv_data %>% 
  left_join(., sun20220722) 
rosmav_data <- rosmav_data %>% 
  left_join(., sun20220722)

#drop redundant timestamps from GPS sensor
asv_data <- asv_data %>% 
  select(-timestamp_gpslocalvelocity_sec,-timestamp_gpsspeed_sec,-timestamp_gpsvelocity_sec)

#split rosmav data into two files
rosmav_m1 <- rosmav_data %>% 
  filter(header_seq <= 31) %>% 
  mutate(path_filename = '2021-06-15_SUN-HC_path.waypoints')

rosmav_m2 <- rosmav_data %>% 
  filter(header_seq>31) %>% 
  mutate(path_filename = '2021-07-03_SUN-NW_path.waypoints')

#### apply waypoints to rosmav missions ####
wp_list <- dir(path_dir, pattern = ('.waypoints'),  recursive = T)
wp_list <- wp_list[grepl('SUN', wp_list)]

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

#grab filepath and waypoint info
all_wp <- all_wp %>% 
  mutate(path_filename = substr(wp_file_path, 5, nchar(wp_file_path))) %>% 
  select(path_filename, waypoint_seq, wp_command, wp_param1)

#join with rosmav missions
rosmav_m1 <- left_join(rosmav_m1, all_wp)
rosmav_m2 <- left_join(rosmav_m2, all_wp)

all_rosmav <- full_join(rosmav_m1, rosmav_m2)

# add CV

#join rosmav and waypoints files
rosmav_wp <- full_join(all_rosmav, all_wp) %>% 
  filter(!is.na(datetime)) #drop data from paths that didn't complete or record

#grab only the columns we need
rosmav_wp <- rosmav_wp %>% 
  select(lake, year, date, lab, deployment_type, equipment, deployment_instance, test_path, deployment_starttime, deployment_endtime,
         timestamp_header_sec, header_seq, waypoint_seq, wp_command, wp_param1,
         rosmav_filename, path_filename, ASV_processed_filename)

rosmav_wp <- rosmav_wp%>% 
  rename(timestamp_gps_sec = timestamp_header_sec) 

#subset asv data
asv <- asv_data %>% 
  mutate(timestamp_gps_sec_integer = round(timestamp_gps_sec, digits = 0)) 
#subset wp data
wp <- rosmav_wp %>% 
  rename(timestamp_gps_sec_integer = timestamp_gps_sec) %>% 
  mutate(timestamp_gps_sec_integer = round(timestamp_gps_sec_integer, digits = 0))
#join asv and wp data
asv_wp <- left_join(asv,wp) %>% 
  arrange(timestamp_gps_sec) %>% 
  select(-timestamp_gps_sec_integer)

#check to see if the correct number of waypoints were added
if (length(wp$header_seq) != length(unique(na.omit(asv_wp$header_seq)))) {
  message('method for adding waypoints by timestamp failed, associating missing waypoints with nearest neighbor')
  wp_list = unique(wp$header_seq)
  asv_list = unique(na.omit(asv_wp$header_seq))
  missing_wp = wp_list[!wp_list %in% asv_list]
  #adjust the time to one second earlier
  for (k in 1:length(missing_wp)){
    wp <- wp %>% 
      mutate(timestamp_gps_sec_integer = case_when(header_seq == (missing_wp[k]) ~ (timestamp_gps_sec_integer) - 1,
                                                   TRUE ~ timestamp_gps_sec_integer))
  }
  # re-join asv_wp to see if it fixed the dropped waypoint
  asv_wp = left_join(asv,wp) %>% 
    arrange(timestamp_gps_sec) %>% 
    select(-timestamp_gps_sec_integer)
} else {}

#check again and nudge forward if necessary
if ((length(wp$header_seq) != length(unique(na.omit(asv_wp$header_seq)))) == TRUE) {
  message('first time nudge failed, attempting alternate nearest neighbor')
  wp_list = unique(wp$header_seq)
  asv_list = unique(na.omit(asv_wp$header_seq))
  missing_wp = wp_list[!wp_list %in% asv_list]
  #adjust the time to one second earlier
  for (k in 1:length(missing_wp)){
    wp <- wp %>% 
      mutate(timestamp_gps_sec_integer = case_when(header_seq == (missing_wp[k]) ~ (timestamp_gps_sec_integer) + 2,
                                                   TRUE ~ timestamp_gps_sec_integer))
  }
  # re-join asv_wp to see if it fixed the dropped waypoint
  asv_wp = left_join(asv,wp) %>% 
    arrange(timestamp_gps_sec) %>% 
    select(-timestamp_gps_sec_integer)
} 

if ((length(wp$header_seq) != length(unique(na.omit(asv_wp$header_seq)))) == TRUE) {
  message('second time nudge unsuccessful, dataset will be missing a waypoint')
  asv_wp$flag_waypoint = 'at least one waypoint was not able to be joined to the processed ASV file'
} else {
  message('time nudge successful, ready to process further')
}

#remove data before first waypoint of mission 
wpsq_min = min(asv_wp$waypoint_seq, na.rm = T)[1]
ix = which(asv_wp$waypoint_seq == wpsq_min)[1]

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
fwrite(asv_wp_trunc, file.path(proc_dir, lake_folder, paste0(lake_folder, '_', deployment_date,'_asv_processed_v', Sys.Date(), '.csv')), row.names = F)
message(paste0('file processed and saved as ',lake_folder, '_', deployment_date,'_asv_processed_v', Sys.Date(), '.csv'))
