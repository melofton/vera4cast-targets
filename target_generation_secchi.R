#test_func <- function(supply argumentf1, f2) {read f1, read f2 -then- rbind() -then- return (targets_df)}
#KKH
#Secchi target generation script

#Install the required googlesheets4 package
library(tidyverse)
library(dplyr)
# install.packages('googlesheets4')
#Load the library 
library(googlesheets4)


target_generation_daily_secchi_m <- function(current, edi){
  #read in the google sheet for secchi
  secchi_current_1 <- read_sheet(current)
  
  #EDI data link
  #https://portal.edirepository.org/nis/mapbrowse?packageid=edi.198.11
  
  #read in the EDI data
  inUrl1  <- edi 
  infile1 <- tempfile()
  try(download.file(inUrl1,infile1,method="curl"))
  if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
  edi <-read_csv(infile1)
  
  #site ids: BVR > bvre
  
  #should fit the format: targets_col_names <- c('datetime', 'site_id', 'depth', "observation", 'variable')
  #Reservoir = site_id, will need to change from upper to lower and add an e on the end of all the reservoirs so 
  #BVR>bvre, FCR>fcre
  #secchi doesn't have an associated depth, secchi_m is an observation, need to create a depth column
  #need to create a variable column: add a column with every row = secchi_m
  secchi_edi <- subset(edi, select = c(DateTime, Reservoir, Secchi_m))
  
  #should fit the format: targets_col_names <- c('datetime', 'site_id', 'depth', "observation", 'variable')
  #Reservoir = site_id, will need to change from upper to lower and add an e on the end of all the reservoirs so 
  #BVR>bvre, FCR>fcre
  #secchi doesn't have an associated depth, Secchi_m is an observation, need to create a depth column
  #need to create a variable column: add a column with every row = secchi_m
  secchi_current_2 <- secchi_current_1 %>% subset(select = c("DateTime", "Reservoir", "Secchi_m"))
  
  ## bind the two files using bind_rows()
  comb_secchi <- bind_rows(secchi_edi, secchi_current_2)
  
  #reformat the reservoir column
  #all lowercase
  #rename to site_id
  #mutate all BVR > bvre, FCR>fcre
  
  library(janitor)
  library(tibble)
  #change colnames to lowercase, add columns, rename and reorder columns to match scheme
  comb_secchi_1 <- comb_secchi %>% 
    clean_names %>% 
    rename("site_id" = "reservoir")%>% 
    rename("datetime" = "date_time")%>% 
    rename("observation" = "secchi_m") %>% 
    add_column(variable = "secchi_m") %>% 
    add_column(depth = NA) %>% 
    subset(select = c('datetime', 'site_id', 'depth', "observation", 'variable'))
  
  #rename all of the site ids
  comb_secchi_2 <- comb_secchi_1 %>% 
    mutate(site_id = replace(site_id, site_id == "BVR", "bvre")) %>% 
    mutate(site_id = replace(site_id, site_id == "FCR", "fcre")) %>% 
    mutate(site_id = replace(site_id, site_id == "CCR", "ccre")) %>% 
    mutate(site_id = replace(site_id, site_id == "GWR", "gwre")) %>%
    mutate(site_id = replace(site_id, site_id == "SHR", "shre"))
  
  comb_secchi_4 <- comb_secchi_2 %>% 
    distinct(datetime, depth, .keep_all=TRUE)
  
  #to check site ids
  # site_id <- unique(comb_secchi_2$site_id) 
  

  
return(comb_secchi_4)
  }

secchi <- target_generation_daily_secchi_m(current = 'https://docs.google.com/spreadsheets/d/1fvM0fDRliuthicQWZT7c9RYErikI5DwrzbOC7TCoMGI/edit#gid=1172894977', 
                                 edi = "https://pasta.lternet.edu/package/data/eml/edi/198/11/81f396b3e910d3359907b7264e689052")






