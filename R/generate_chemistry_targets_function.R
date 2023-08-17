
# packges 
pacman::p_load(tidyverse)

generate_chemistry_targets_function <- function(file_1, "https://pasta.lternet.edu/package/data/eml/edi/199/11/509f39850b6f95628d10889d66885b76"){
  
  ## read in current data file 
  # Currently no current data file for Chem
  dt1 <-NULL
  
  # read in historical data file 
  # EDI
  inUrl1 <- file_2
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/11/509f39850b6f95628d10889d66885b76" 
  infile1 <- tempfile()
  try(download.file(inUrl1,infile1,method="curl"))
  if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
  
  
  dt2 <-read_csv(infile1) 
  
  ## manipulate the data files to match each other 
  # secchi: match current data file and select only secchi data in EDI
  # temperature: in correct format as EDI 
  # swimmibility: select the sfc water and air temp columns and just use those
  # Phtyos/MOMs: select columns that you need 
  #MOM: EDI 
  #Phytos: Taken from catwalk file
  
  ## bind the two files using row.bind()
  te
    df<-bind_rows(dt1,dt2)%>%
      filter(Reservoir=="FCR"|Reservoir=="BVR")%>%
      filter(Site==50)%>%
      select(-starts_with("Flag"),-DIC_mgL,-DC_mgL,-DN_mgL)%>% # get rid of the columns we don't want
      group_by(Reservoir,Site,Depth_m,DateTime)%>%
      summarise(across(everything(), mean, na.rm=TRUE))%>% # average if there are reps taken at a depths
      ungroup()%>%
      mutate(Date=as.Date(DateTime))%>%
      group_by(Reservoir,Site,Depth_m,Date)%>% # average if there are more than one sample taken during that day
      summarise(across(everything(), mean, na.rm=TRUE))%>%
      ungroup()%>%
      mutate(datetime=ymd_hms(paste0(Date,"","00:00:00")))%>%
      mutate(Reservoir=)
      select()
      pivot_longer(cols=c('var1', 'var2', ...),
                   names_to='col1_name',
                   values_to='col2_name') 
  ## Match data to flare targets file 
  # Use pivot_longer to create a long-format table
  # for time specific - use midnight UTC values for daily 
  # for hourly 
  
      return(df)
  ## return dataframe formatted to match FLARE targets
}




# RESOURCES 

# FCRE catwalk L1 file 
# https://github.com/FLARE-forecast/FCRE-data/blob/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv

# targets file 
targets_col_names <- c('datetime', 'site_id', 'depth', "observation", 'variable')

# READ IN EDI FCRE Catwalk file 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
dt1 <-read_csv(infile1)
