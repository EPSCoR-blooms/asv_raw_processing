# list out column names from the pre-processed ASV files to see if additional CV needs to be added to colname CV list

#load libraries
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
lake_dir = paste0(path_pat, 'project_data/ASV_data/raw_csv_data/')
meta_dir = paste0(path_pat, 'project_data/ASV_data/metadata/')

# read in compiled metadata sheet
metadata <- read.csv(file.path(comp_dir, 'compiled_ASV_deployment_general_info.csv')) %>% 
  filter(!is.na(rosmav_missionreached_filename)) #filter out incomplete files

# # I only prepped and moved most of the Sunapee data - a couple of files to leave out here to avoid script meltdown
# metadata <- metadata %>%
#   filter((lake == 'SUN' & date != '2021-07-22') |
#            lake == 'AUB' |
#            lake == 'CHN' |
#            lake == 'SAB')

# read in columns for processed Bag files ####
asv_filelist <- metadata$ASV_processed_filename
asv_lakelist <- metadata$lake
asv_datelist <- metadata$date
asv_yearlist <- format(as.Date(asv_datelist), '%Y')

for(i in 1:length(asv_filelist)){
  df <- read.csv(file.path(lake_dir, asv_lakelist[i], asv_yearlist[i], asv_datelist[i], asv_filelist[i]))
  if(i == 1){
  col_for_cv <- colnames(df)
  } else{
    col2_for_cv <- colnames(df)
    col_for_cv <- append(col_for_cv, col2_for_cv)
  }
}

col_for_cv <- as.data.frame(unique(col_for_cv))
colnames(col_for_cv) = 'column_names'

#read in existing colnames_cvnames file
col_cv <- read.csv(file.path(meta_dir, 'colnames_cv/bag_colnames_cvnames.csv'))

#join new list with existing list
col_cv <- full_join(col_cv, col_for_cv)

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

rm(df)

# read in metadata and select columns for processed rosmav files ####
rosmav_filelist <- metadata$rosmav_missionreached_filename

for(i in 1:length(rosmav_filelist)){
  df <- read.csv(file.path(lake_dir, asv_lakelist[i], asv_yearlist[i], asv_datelist[i], rosmav_filelist[i]))
  if(i == 1){
    rosmav_col_for_cv <- colnames(df)
  } else {
    rosmav_col2_for_cv <- colnames(df)
    rosmav_col_for_cv <- append(rosmav_col_for_cv, rosmav_col2_for_cv)
  }
}

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


