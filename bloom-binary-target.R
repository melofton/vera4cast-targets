# Title: Binary Bloom Target for VERA forecasts
# Author: Mary Lofton
# Date: 14Aug23

# Load packages
library(tidyverse)
library(lubridate)

# List both EDI file and current Github file
files <- c("https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f",
           "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

# Read in files, combine them, and data wrangle to get bloom target
# bloom = 1 if daily mean chl-a from EXO is >= 20 ug/L, 0 if below that threshold

df <- readr::read_csv(files) |> 
  dplyr::mutate(DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                DateTime = lubridate::with_tz(DateTime, tzone = "UTC")) %>%
  dplyr::filter(Flag_EXOChla_ugL_1 == 0) %>%
  dplyr::select(DateTime, EXOChla_ugL_1) %>%
  dplyr::mutate(Date = lubridate::date(DateTime),
         Hour = lubridate::hour(DateTime)) %>%
  dplyr::group_by(Date, Hour) %>%
  dplyr::summarize(Chla_ugL_mean = mean(EXOChla_ugL_1, na.rm = TRUE),
            Chla_ugL_sd = sd(EXOChla_ugL_1, na.rm = TRUE)) %>%
  dplyr::group_by(Date) %>%
  dplyr::summarize(Chla_ugL_mean = mean(Chla_ugL_mean, na.rm = TRUE),
            Chla_ugL_sd = sd(Chla_ugL_sd, na.rm = TRUE)) %>%
  dplyr::mutate(Bloom = ifelse(Chla_ugL_mean >= 20, 1, 0)) %>%
  dplyr::select(Date, Bloom)

ggplot(data = df, aes(x = Date, y = Bloom))+
  geom_point()+
  theme_classic()

# Need to repeat this for BVR
