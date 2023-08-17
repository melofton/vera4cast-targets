#calculates MOM_binary (eventually add upper and lower MOM bounds?)
#MOM is calculated as the layer where DO is < 2mg/L (for now)
#17Aug2023 HLW

if (!require("pacman"))install.packages("pacman")
pacman::p_load(utils, tidyverse)

targets_generation_daily_MOM <- function(file1, file2){
  
  #read in current CTD data
  #PLACEHOLDER for pulling in csv files off of github and then merging to one big file
  
  #download CTD data from EDI
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/13/27ceda6bc7fdec2e7d79a6e4fe16ffdf" 
  infile1 <- tempfile()
  try(download.file(inUrl1,infile1,method="curl"))
  if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
  file2 <- read_csv(infile1) |> filter(Site == 50, Reservoir == "FCR", Depth_m > 0)
  
  #select cols needed to generate targets
  #file1 <- file1 |> select()
  file2 <- file2 |> select(c(DateTime,Reservoir,Depth_m,DO_mgL))
  
  #combine current and historic ctd data
  #DO <- dplyr::bind_rows(file1, file2)
  DO <- dplyr::bind_rows(file2)
  
  #change reservoir site name
  DO$Reservoir <- "fcre"
  
  #change names so df matches targets format
  DO <- DO |> rename(datetime = DateTime,
                       site_id = Reservoir,
                       depth_m = Depth_m,
                       observation = DO_mgL) 
  
  #add variable column
  DO$variable <- "DO_mgL"
  
  #now calculate MOM binary
  MOM_binary <- DO |> group_by(datetime, site_id, variable) |>
    mutate(MOM_binary = ifelse(observation < 2 & depth_m >= 2 & depth_m <= 8, 1, 0)) |>
    summarise(MOM_binary = max(MOM_binary)) |> select(-variable)

  #rename columns to match targets file
  MOM_binary <- MOM_binary |>
    rename(observation = MOM_binary)
  
  MOM_binary$variable <- "MetalimneticOxygenMinimum_binary"
  
  #add upper and lower bounds next!
  
  }


