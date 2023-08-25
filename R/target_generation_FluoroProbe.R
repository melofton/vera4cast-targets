# Title: Generation of target files based on FluoroProbe data for VERA forecasts
# Author: Mary Lofton
# Date: 17Aug23

# Description: Generates the following targets using FluoroProbe data:

#' DeepChlorophyllMaximum_binary
#' TotalBiomass_ugL
#' GreenAlgae_ugL
#' Bluegreens_ugL
#' BrownAlgae_ugL
#' MixedAlgae_ugL
#' TotalBiomass_ugL_CM
#' GreenAlgae_ugL_CM
#' Bluegreens_ugL_CM
#' BrownAlgae_ugL_CM
#' MixedAlgae_ugL_CM
#' ChlorophyllMaximum_depth

# Load packages
library(tidyverse)
library(lubridate)
library(httr)


target_generation_FluoroProbe <- function(EDI_file = "https://portal.edirepository.org/nis/dataviewer?packageid=edi.272.7&entityid=001cb516ad3e8cbabe1fdcf6826a0a45"){
  
  ## read in current data file 
  # Github, Googlesheet, etc. 
  req <- GET("https://api.github.com/repos/CareyLabVT/Reservoirs/git/trees/master?recursive=1")
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  vec1 <- grep("Data/DataNotYetUploadedToEDI/Raw_fluoroprobe", filelist, value = TRUE, fixed = TRUE)
  vec2 <- grep(".txt", vec1, value = TRUE, fixed = TRUE)
  
  # Load in column names for .txt files to get template
  df <- read_tsv(paste0("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/",vec2[1]), n_max = 0) %>%
    mutate(cast = character(length = 0L))
  
  # Load in all txt files
  casts1 <- sapply(vec2, function(x) strsplit(x, "/"), USE.NAMES=FALSE)
  casts2 <- sapply(casts1, `[`, 4)
  
  for(i in 1:length(vec2)){
    fp_file <- read_tsv(paste0("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/",vec2[i])) %>%
      mutate(cast = casts2[i]) %>%
      slice(-1)
    
    df <- bind_rows(df, fp_file)
    
  }
  
  # read in historical data file 
  needed_cols <- c("Reservoir"       ,     "Site"            ,     "DateTime"  ,          
                   "GreenAlgae_ugL"   ,    "Bluegreens_ugL"   ,    "BrownAlgae_ugL"   ,   
                   "MixedAlgae_ugL"    ,   "TotalConc_ugL"     ,  
                    "Depth_m"            )
  
  edi <- readr::read_csv(EDI_file) %>%
    select(any_of(needed_cols))
  
  ## manipulate the data files to match each other 
  
  #split out column containing filename to get Reservoir and Site data
  df2 <- df %>%
    rowwise() %>% 
    mutate(Reservoir = unlist(strsplit(cast, split='_', fixed=TRUE))[2],
           Site = unlist(strsplit(cast, split='_', fixed=TRUE))[3],
           Site = as.numeric(unlist(strsplit(Site, split='.', fixed=TRUE))[1])) %>%
    ungroup() %>%
    rename(DateTime = `Date/Time`, GreenAlgae_ugL = `Green Algae...2`,Bluegreens_ugL = `Bluegreen...3`,
           BrownAlgae_ugL = `Diatoms...4`, MixedAlgae_ugL = `Cryptophyta...5`, YellowSubstances_ugL = `Yellow substances...9`,
           TotalConc_ugL = `Total conc.`, Transmission = `Transmission`, Depth_m = `Depth`, Temp_degC = `Temp. Sample`,
           RFU_525nm = `LED 3 [525 nm]`, RFU_570nm = `LED 4 [570 nm]`, RFU_610nm = `LED 5 [610 nm]`,
           RFU_370nm = `LED 6 [370 nm]`, RFU_590nm = `LED 7 [590 nm]`, RFU_470nm = `LED 8 [470 nm]`,
           Pressure_unit = `Pressure`) %>%
    select(cast, Reservoir, Site, DateTime, GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, MixedAlgae_ugL, YellowSubstances_ugL,
           TotalConc_ugL, Transmission, Depth_m, Temp_degC, RFU_525nm, RFU_570nm, RFU_610nm,
           RFU_370nm, RFU_590nm, RFU_470nm) %>%
  # Rename and select useful columns; drop metrics we don't use or publish such as cell count;
  # eliminate shallow depths because of quenching
    mutate(across(!DateTime & !cast & !Reservoir & !Site, as.numeric)) %>%
    mutate(DateTime = as.POSIXct(as_datetime(DateTime, tz = "", format = "%m/%d/%Y %I:%M:%S %p"))) %>%
    filter(Depth_m >= 0.2) %>%
    dplyr::mutate(DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
    DateTime = lubridate::with_tz(DateTime, tzone = "UTC"))
  
  #trim casts to eliminate poor readings due to sediment interference at bottom of reservoir
  #for our purposes, we consider the "max depth" of each reservoir to be:
  #FCR = 9.5 m; BVR = 10.0 m; CCR = 21 m 
  
  df3 = df2[FALSE,]
  
  for (i in 1:length(unique(df2$cast))){
    profile = subset(df2, cast == unique(df2$cast)[i])
    if(profile$Reservoir[1] == "FCR"){
      profile_trim <- profile %>% filter(Depth_m <= 9.5)
    } else if (profile$Reservoir[1] == "CCR"){
      profile_trim <- profile %>% filter(Depth_m <= 21)
    } else if (profile$Reservoir[1] == "BVR"){
      profile_trim <- profile %>% filter(Depth_m <= 10)
    }
    df3 <- bind_rows(df2, profile_trim)
  } 
  
  df4 <- df3 %>%
    select(any_of(colnames(edi)))
  
  ## bind the two files using bind_rows()
  # need to double-check that columnns match
  fp <- bind_rows(edi, df4) %>%
    filter(Reservoir %in% c("FCR","BVR") & Site == 50) %>%
    arrange(Reservoir, DateTime, Depth_m)
  
  biomass_exo <- fp %>%
    mutate(Date = date(DateTime)) %>%
    group_by(Date) %>%
    slice(which.min(abs(Depth_m - 1.6))) %>%
    pivot_longer(GreenAlgae_ugL:TotalConc_ugL, names_to = "variable", values_to = "observation") %>%
    rename(datetime = DateTime, depth_m = Depth_m) %>%
    mutate(site_id = ifelse(Reservoir == "FCR","fcre","bvre"),
           depth_m = 1.6) %>%
    ungroup() %>%
    select(datetime, site_id, depth_m, observation, variable)
  
  biomass_cmax <- fp %>%
    mutate(Date = date(DateTime)) %>%
    group_by(Date) %>%
    slice(which.max(TotalConc_ugL)) %>%
    pivot_longer(GreenAlgae_ugL:TotalConc_ugL, names_to = "variable", values_to = "observation") %>%
    rename(datetime = DateTime, depth_m = Depth_m) %>%
    mutate(site_id = ifelse(Reservoir == "FCR","fcre","bvre"),
           variable = paste0(variable, "_CM")) %>%
    ungroup() %>%
    select(datetime, site_id, depth_m, observation, variable)
  
  cmax_depth <- fp %>%
    mutate(Date = date(DateTime)) %>%
    group_by(Date) %>%
    slice(which.max(TotalConc_ugL)) %>%
    pivot_longer(Depth_m, names_to = "variable", values_to = "observation") %>%
    rename(datetime = DateTime) %>%
    mutate(site_id = ifelse(Reservoir == "FCR","fcre","bvre"),
           variable = "ChlorophyllMaximum_depth",
           depth_m = NA) %>%
    ungroup() %>%
    select(datetime, site_id, depth_m, observation, variable)
  
  mean_biomass <- fp %>%
    mutate(Date = date(DateTime)) %>%
    group_by(Date) %>%
    summarize(mean_biomass = mean(TotalConc_ugL, na.rm = TRUE)) 
  
  dcm <- fp %>%
    mutate(Date = date(DateTime)) %>%
    group_by(Date) %>%
    slice(which.max(TotalConc_ugL)) %>%
    left_join(mean_biomass) %>%
    mutate(DeepChlorophyllMaximum_binary = ifelse(TotalConc_ugL > mean_biomass + 0.5 & TotalConc_ugL > mean_biomass*1.5 & ((Reservoir == "FCR" & Depth_m > 0.93) | (Reservoir == "BVR" & Depth_m > 1)), 1, 0)) %>%
    pivot_longer(DeepChlorophyllMaximum_binary, names_to = "variable", values_to = "observation") %>%
    rename(datetime = DateTime) %>%
    mutate(site_id = ifelse(Reservoir == "FCR","fcre","bvre"),
           depth_m = NA) %>%
    ungroup() %>%
    select(datetime, site_id, depth_m, observation, variable)
    
  final <- bind_rows(biomass_exo, biomass_cmax, cmax_depth, dcm)

  
 return(final)
  
}




