# This script compiles all metadata for the robotics runs from the ASV template.

# this will likely need some attention at the end for dealing with incomparable data types in the joins.

#load libraries
library(tidyverse)
library(readxl)

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
meta_dir = paste0(path_pat, 'project_data/ASV_data/metadata/metadata_template/')
comp_dir = paste0(path_pat, 'project_data/ASV_data/compiled/')
inter_dir = paste0(path_pat, 'project_data/ASV_data/intermediary/')

# read in the ASV metadata files - the weather and deployment log sheets - these must always be present ####
read_asv_template = function(filename, directory){
  weather_obs <- read_xlsx(file.path(directory, filename),
                           sheet = 'WeatherFieldInfo')
  deploy_info <- read_xlsx(file.path(directory, filename), 
            sheet = 'deployment_log')
  metadata <- full_join(weather_obs, deploy_info)
}

#list files in meta dir
filelist <- list.files(meta_dir)

#apply function over filelist
for(i in 1:length(filelist)) {
  dataframe = read_asv_template(filelist[i], meta_dir)
  if(i == 1){
    compiled = dataframe
  } else {
    compiled <- full_join(compiled, dataframe)
  }
}

#format lake names to 3 letter and save file
compiled %>% 
  mutate(lake = case_when(lake == 'Auburn' ~ 'AUB',
                          lake == 'China' ~ 'CHN', 
                          lake == 'Sabattus' ~ 'SAB',
                          lake == 'Sunapee' ~ 'SUN',
                          TRUE ~ lake)) %>% 
  mutate(deployment_starttime = format(deployment_starttime, '%H:%M'),
         deployment_endtime = format(deployment_endtime, '%H:%M')) %>% 
  write.csv(., file.path(comp_dir, paste0('compiled_ASV_deployment_general_info.csv')), row.names = F)

# read in the ASV additional data sheets ####

#grab lake, date info from compiled
additional_data <- compiled %>% 
  select(lake, date)

# get the sheets list for each metadata file and use it for if statements in loop
for(j in 1:length(filelist)){
  sheets_list <- excel_sheets(file.path(meta_dir, filelist[j]))
  print(paste0('file name: ', filelist[j]))
  if (any(grepl('additional', sheets_list))){
    add_data <- read_xlsx(file.path(meta_dir, filelist[j]),
              sheet = 'additional_data_log')
    print('additional data metadata detected')
    additional_sampling <- full_join(additional_data, add_data)
  }  else {
    print('no additional metadata detected')
    additional_sampling <- additional_data
  }
}

# write the additional data metadata file
collated_additional_sampling <- read.csv(file.path(comp_dir, paste0('compiled_ASV_deployment_additionalsampling_info.csv'))) %>%
  mutate(date = as.character(date))

additional_sampling %>% 
  mutate(lake = case_when(lake == 'Auburn' ~ 'AUB',
                          lake == 'China' ~ 'CHN', 
                          lake == 'Sabattus' ~ 'SAB',
                          lake == 'Sunapee' ~ 'SUN',
                          TRUE ~ lake),
         date = as.character(date)) %>% 
  full_join(., collated_additional_sampling) %>%
  write.csv(., file.path(comp_dir, paste0('compiled_ASV_deployment_additionalsampling_info.csv')), row.names = F)

# get the sheets list for each metadata file and use it for if statements in loop
for(j in 1:length(filelist)){
  sheets_list <- excel_sheets(file.path(meta_dir, filelist[j]))
  print(paste0('file name: ', filelist[j]))
  if (any(grepl('sampling', sheets_list))) {
    samploc_data <- read_xlsx(file.path(meta_dir, filelist[j]),
                              sheet = 'sampling_locations')
    print('sampling location metadata detected')
    samploc_data <- full_join(additional_data, samploc_data)
  }  else {
    print('no additional metadata detected')
    samploc_data <- additional_data
  }
}

# write the additional data metadata file
collated_additional_sampling_loc <- read.csv(file.path(comp_dir, paste0('compiled_ASV_deployment_sampling_loc_info.csv'))) %>%
  mutate(date = as.character(date))

samploc_data %>% 
  mutate(lake = case_when(lake == 'Auburn' ~ 'AUB',
                          lake == 'China' ~ 'CHN', 
                          lake == 'Sabattus' ~ 'SAB',
                          lake == 'Sunapee' ~ 'SUN',
                          TRUE ~ lake),
         date = as.character(date),
         time_grab = as.character(format(as.POSIXct(time_grab), '%H:%M'))) %>%
  full_join(., collated_additional_sampling_loc) %>%
  write.csv(., file.path(comp_dir, paste0('compiled_ASV_deployment_sampling_loc_info.csv')), row.names = F)


# get the sheets list for each metadata file and use it for if statements in loop
for(j in 1:length(filelist)){
  sheets_list <- excel_sheets(file.path(meta_dir, filelist[j]))
  print(paste0('file name: ', filelist[j]))
  if (any(grepl('sonde', sheets_list))) {
    sonde_data <- read_xlsx(file.path(meta_dir, filelist[j]),
                            sheet = 'sonde_data')
    print('sonde data detected')
    sonde_data <- full_join(additional_data, sonde_data)
  }  else {
    print('no sonde data detected')
    sonde_data <- additional_data
  }
}

# write the additional data metadata file
collated_sonde <- read.csv(file.path(comp_dir, paste0('compiled_ASV_deployment_sonde_data.csv'))) %>%
  mutate(date = as.character(date))

sonde_data %>% 
  mutate(lake = case_when(lake == 'Auburn' ~ 'AUB',
                          lake == 'China' ~ 'CHN', 
                          lake == 'Sabattus' ~ 'SAB',
                          lake == 'Sunapee' ~ 'SUN',
                          TRUE ~ lake),
         date = as.character(date)) %>% 
  full_join(., collated_sonde) %>%
  write.csv(., file.path(comp_dir, paste0('compiled_ASV_deployment_sonde_data.csv')), row.names = F)



