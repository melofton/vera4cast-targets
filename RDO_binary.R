#' Generate EXO targets
#' ASL, 8 Aug 2023

library(tidyverse)


# FCR
fcr_files <- c("https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f",
           "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

fcr_df <- readr::read_csv(fcr_files) |> 
  dplyr::mutate(DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                DateTime = lubridate::with_tz(DateTime, tzone = "UTC"))

fcr_do_sum <- fcr_df |>
  dplyr::mutate(site_id = "fcre",
                Date = as.Date(DateTime)) |>
  dplyr::group_by(Date, site_id) |> #daily mean
  dplyr::summarise(Hypoxia_binary = as.numeric(mean(RDO_mgL_9, na.rm = T)<2),
                   Temp_C = mean(EXOTemp_C_1, na.rm = T),
                   Cond_uScm = mean(EXOCond_uScm_1, na.rm = T),
                   SpCond_uScm = mean(EXOSpCond_uScm_1, na.rm = T),
                   DO_Sat_percent = mean(EXODOsat_percent_1, na.rm = T),
                   DO_mgL = mean(EXODO_mgL_1, na.rm = T),
                   Chla_ugL = mean(EXOChla_ugL_1, na.rm = T),
                   fDOM_QSU = mean(EXOfDOM_QSU_1, na.rm = T),
                   Turbidity_FNU = mean(EXOTurbidity_FNU_1, na.rm = T),
                   Bloom_binary = as.numeric(mean(Chla_ugL, na.rm = T)>20))


#thermistors (0.1, 1, 5, 8, 9 for FCR)
#water leve
#there's also an exo depth column we could use

# BVR
bvr_current <- readr::read_csv("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv")
bvr_2020_2022 <- readr::read_csv("https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038")
bvr_2016_2020 <- readr::read_csv("https://pasta.lternet.edu/package/data/eml/edi/725/3/38965aab7e21bf7a6157ba4d199c5e2c")

bvr_do_sum <- bvr_current |> 
  full_join(bvr_2016_2020) |> 
  full_join(bvr_2020_2022) |> 
  dplyr::mutate(site_id = "bvre",
                DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                Date = as.Date(DateTime),
                ) |> 
  dplyr::group_by(Date, site_id) |> #daily mean
  dplyr::summarise(Hypoxia_binary = as.numeric(mean(RDO_mgL_13, na.rm = T) < 2),
                   Temp_C = mean(EXOTemp_C_1.5, na.rm = T),
                   Cond_uScm = mean(EXOCond_uScm_1.5, na.rm = T),
                   SpCond_uScm = mean(EXOSpCond_uScm_1.5, na.rm = T),
                   DO_Sat_percent = mean(EXODOsat_percent_1.5, na.rm = T),
                   DO_mgL = mean(EXODO_mgL_1.5, na.rm = T),
                   Chla_ugL = mean(EXOChla_ugL_1.5, na.rm = T),
                   fDOM_QSU = mean(EXOfDOM_QSU_1.5, na.rm = T),
                   Turbidity_FNU = mean(EXOTurbidity_FNU_1.5, na.rm = T),
                   Bloom_binary = as.numeric(mean(Chla_ugL, na.rm = T)>20)
                   )

#thermistors (BVR surface, middle, bottom)

#Combine
do_sum <- fcr_do_sum |> 
  rbind(bvr_do_sum) |> 
  rename(datetime = Date) |> 
  pivot_longer(cols = Hypoxia_binary:Bloom_binary, names_to = "variable", values_to = "observation") |> 
  mutate(depth_m = ifelse(site_id == "fcre" & variable == "Hypoxia_binary", 9, NA),
         depth_m = ifelse(site_id == "bvre" & variable == "Hypoxia_binary", 11, depth_m),
         depth_m = ifelse(site_id == "fcre" & variable != "Hypoxia_binary", 1.6, depth_m),
         depth_m = ifelse(site_id == "bvre" & variable != "Hypoxia_binary", 1.5, depth_m)) |> 
  select(datetime, site_id, depth_m, observation, variable) |> 
  mutate(observation = ifelse(!is.finite(observation),NA,observation))
