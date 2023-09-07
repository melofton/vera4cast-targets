library(tidyverse)

fcr_files <- c("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv",
               "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f")

bvr_files <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv",
               "https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038")

# git_dir <- getwd()
# sourceDirectory(file.path(git_dir, "R"))

files.source <- list.files("R/", pattern = "*.R$")
invisible(sapply(file.path("R",files.source),source,.GlobalEnv))

## only use daily variables 

# save data of interest individually
chem_daily <- target_generation_chemistry_daily(current_data_file=NULL, 
                                                historic_data_file="https://pasta.lternet.edu/package/data/eml/edi/199/11/509f39850b6f95628d10889d66885b76")

secchi_daily <- target_generation_daily_secchi_m(current = 'https://docs.google.com/spreadsheets/d/1fvM0fDRliuthicQWZT7c9RYErikI5DwrzbOC7TCoMGI/edit#gid=1172894977', 
                                                 edi = "https://pasta.lternet.edu/package/data/eml/edi/198/11/81f396b3e910d3359907b7264e689052")

exo_daily <- target_generation_exo_daily(fcr_files = fcr_files, 
                                         bvr_files = bvr_files)

FluoroProbe_daily <- target_generation_FluoroProbe(EDI_file = "https://portal.edirepository.org/nis/dataviewer?packageid=edi.272.7&entityid=001cb516ad3e8cbabe1fdcf6826a0a45")

ghg_daily <- target_generation_ghg_targets(current_data_file=NULL,
                                           edi_data_file= "https://pasta.lternet.edu/package/data/eml/edi/551/7/38d72673295864956cccd6bbba99a1a3")

inflow_daily <- target_generation_inflow_daily(current_data_file="https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data-qaqc/FCRWeir_L1.csv",
                                               edi_data_file="https://pasta.lternet.edu/package/data/eml/edi/202/10/c065ff822e73c747f378efe47f5af12b")

metals_daily <-  target_generation_metals_daily(current_data_file=NULL,
                                                edi_data_file="https://pasta.lternet.edu/package/data/eml/edi/455/7/e71d70cac1650610e6a3fbbf7928007f")

no_strat_daily <- target_generation_NotStratified_binary(current_file = fcr_files[1], 
                                                         historic_file = fcr_files[2])

rdo_daily <- target_generation_rdo_daily(fcr_files = fcr_files, 
                                         bvr_files = bvr_files)

summer_strat_daily <- target_generation_SummerStratified_binary(current_file = fcr_files[1], 
                                                                historic_file = fcr_files[2])

swim_index_daily <- target_generation_swimability_index(fcre_current_temp_file = fcr_files[1],
                      fcre_historic_temp_file = fcr_files[2],
                      bvre_current_temp_file = bvr_files[1],
                      bvre_historic_temp_file = bvr_files[2],
                      current_met_file = 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_L1.csv',
                      historic_met_file = 'https://pasta.lternet.edu/package/data/eml/edi/389/7/02d36541de9088f2dd99d79dc3a7a853')

thermistor_daily <- target_generation_ThermistorTemp_C_daily(current_file = fcr_files[1], 
                                                             historic_file = fcr_files[2])

waterlevel_daily <- target_generation_waterlevel_daily(fcr_files = fcr_files, 
                                                       bvr_files = bvr_files)
## combine data 
combined_data <- dplyr::bind_rows(exo_daily,
                                  FluoroProbe_daily, 
                                  inflow_daily, 
                                  no_strat_daily, 
                                  rdo_daily, 
                                  summer_strat_daily, 
                                  swim_index_daily, 
                                  thermistor_daily, 
                                  waterlevel_daily)


combined_subset <- combined_data |> 
  filter(site_id %in% c('fcre','FCR'), 
         depth %in% c(NA, 0)) |> 
  select(-depth_m)
  
ggplot(combined_subset, aes(x = datetime)) +
  #geom_line(aes(y = observation)) +
  geom_point(aes(y = observation)) +
  facet_wrap(~variable, scale = 'free')
