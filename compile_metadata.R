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
meta_dir = paste0(path_pat, 'project_data/ASV_data/raw_data/metadata/')
parent_dir = paste0(path_pat, 'project_data/compiled_metadata/')

#list files in meta dir
filelist <- list.files(meta_dir)

# write a function to read in all metadata files, including each of the worksheets
read_asv_uav_template = function(filename, directory){
  dep_log <- read_xlsx(file.path(directory, filename), 
            sheet = 'deployment_log')
  add_data <- read_xlsx(file.path(directory, filename), 
            sheet = 'additional_data_log')
  metadata <- full_join(dep_log, add_data) %>% 
    mutate(start_time = format(start_time, '%H:%M'),
           end_time = format(end_time, '%H:%M'))
}

#apply function over filelist
for(i in 1:length(filelist)) {
  dataframe = read_asv_uav_template(filelist[i], meta_dir)
  if(i == 1){
    compiled = dataframe
  } else {
    compiled <- full_join(compiled, dataframe)
  }
}

#save compiled metadata
write.csv(compiled, file.path(parent_dir, paste0('compiled_ASV_UAV_metadata_', Sys.Date(), '.csv')), row.names = F)
