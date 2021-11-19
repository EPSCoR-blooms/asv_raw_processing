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
lake_dir = paste0(path_pat, 'project_data/ASV_data/raw_data/')
dump_dir = paste0(path_pat, 'project_data/ASV_data/raw_data/compiled/')

#list lakes in lake directory
lake_list <- dir(lake_dir, recursive = F)
lake_list <- lake_list[!grepl('meta', lake_list)] #remove metadata folder name

#create a matrix of lakes, years, dates of data to work through the folder system
for(i in 1:length(lake_list)) {
  year_list = dir(file.path(lake_dir, lake_list[i]), recursive = F)
  for(x in 1:length(year_list)) {
    year_lake_list = c()
    year_lake_list$lake = lake_list[i]
    year_lake_list$year = year_list[x]
    df = as.data.frame(year_lake_list)
    if(x == 1) {
      compiled_list <-  df
    } else {
      df_compiled_list <- full_join(compiled_list, df)
    }
  }    
  rm(df, compiled_list)
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

rm(compiled_lake_list)

#make a list of all files in the above folders
get_deployment_files <- function(lake, year, date) {
  list.files(file.path(lake_dir, lake, year, date))
}

make_folder_path <- function(lake, year, date, fn){
  file.path(lake_dir, lake, year, date, fn)
}

#create a list of files to open
for (m in 1:nrow(deployment_date_list)){
  dep_file_list <- get_deployment_files(deployment_date_list$lake[m],
                                        deployment_date_list$year[m],
                                        deployment_date_list$date_list[m])
  for (n in 1: length(dep_file_list)){
    path <- make_folder_path(deployment_date_list$lake[m],
                     deployment_date_list$year[m],
                     deployment_date_list$date_list[m],
                     dep_file_list[n])
    if (m == 1 & n == 1) {
      path_list = path
    } else {
      path_list = append(path_list, path)
    }
  }
}

# open and save files into main dataframe
for(l in 1:length(path_list)) {
  df = read.csv(path_list[l])
  if(l == 1){
    all_asv_data = df
  } else {
    all_asv_data = full_join(all_asv_data, df)
  }
}
###   THESE FILES DON'T HAVE THE SAME COLUMN NAMES. #####
