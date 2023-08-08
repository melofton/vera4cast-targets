#' Generate binary DO targets
#' ASL, 8 Aug 2023

# FCR
fcr_files <- c("https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f",
           "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

fcr_df <- readr::read_csv(fcr_files) |> 
  dplyr::mutate(DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                DateTime = lubridate::with_tz(DateTime, tzone = "UTC"))

fcr_do_sum <- fcr_df |>
  dplyr::mutate(Date = as.Date(DateTime)) |>
  dplyr::group_by(Date, Reservoir) |> #daily mean
  dplyr::summarise(Hypoxic_bin = as.numeric(mean(RDO_mgL_9, na.rm = T)<2))

# BVR
bvr_current <- readr::read_csv("https://github.com/FLARE-forecast/BVRE-data/blob/bvre-platform-data-qaqc/bvre-waterquality_L1.csv")
bvr_2020_2022 <- readr::read_csv("https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038")
bvr_2016_2020 <- readr::read_csv("https://pasta.lternet.edu/package/data/eml/edi/725/3/38965aab7e21bf7a6157ba4d199c5e2c")

bvr_do_sum <- bvr_current |> 
  full_join(bvr_2016_2020) |> 
  full_join(bvr_2020_2022) |> 
  dplyr::mutate(DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                Date = as.Date(DateTime),
                RDO_mgL_13 = ifelse(is.na(RDO_mgL_13),MiniDotDO_mgL_10,RDO_mgL_13)) |> #use MiniDot if no RDO available
  dplyr::group_by(Date, Reservoir) |> #daily mean
  dplyr::summarise(Hypoxic_bin = as.numeric(mean(RDO_mgL_13, na.rm = T) < 2))

do_sum <- fcr_do_sum%>%
  full_join(bvr_do_sum)
