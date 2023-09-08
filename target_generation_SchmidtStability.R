generate_schmidt_stability <- function(current_file, historic_file) {
  
  source('R/find_depths.R')
  ## read in current data file
  # Github, Googlesheet, etc.
  current_df <- readr::read_csv(current_file, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp'))
  
  if (current_df$Reservoir[1] == 'BVR') {
    bvr_depths <- find_depths(data_file = current_file,
                              depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
                              output = NULL,
                              date_offset = "2021-04-05",
                              offset_column1 = "Offset_before_05APR21",
                              offset_column2 = "Offset_after_05APR21") |>
      dplyr::filter(variable == 'ThermistorTemp') |>
      dplyr::select(Reservoir, DateTime, Depth_m, variable, depth_bin, Position) |> 
      dplyr::rename(WaterLevel = Depth_m,
                    depth = depth_bin) |> 
      dplyr::mutate(date = lubridate::as_date(DateTime)) 
    
    current_df_1 <- current_df  |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = c('variable','Position'),
                          names_sep = '_C_',
                          values_to = 'observation') |>
      dplyr::mutate(date = lubridate::as_date(DateTime),
                    Position = as.numeric(Position)) |>
      na.omit() |>
      dplyr::left_join(bvr_depths,
                       by = c('Position', 'date', 'Reservoir', 'variable')) |>
      dplyr::group_by(date, Reservoir, depth) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       WaterLevel = mean(WaterLevel, na.rm=T),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
      na.omit()
    
  }
  
  # read in differently for FCR
  if (current_df$Reservoir[1] == 'FCR') {
    current_df_1 <- current_df |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = 'depth',
                          names_prefix = 'ThermistorTemp_C_',
                          values_to = 'observation') |>
      na.omit()|>
      dplyr::mutate(date = lubridate::as_date(DateTime), 
                    depth = ifelse(depth == 'surface', 0.1, depth)) |>
      na.omit() |>
      dplyr::group_by(date, Reservoir, depth) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
      na.omit()
    
  }
  
  # Need bathymetry
  infile <- tempfile()
  try(download.file("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184",
                    infile, method="curl"))
  if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  
  bathymetry <- readr::read_csv(infile, show_col_types = F)  |>
    dplyr::select(Reservoir, Depth_m, SA_m2) |>
    # dplyr::rename(depths = Depth_m,
    #               areas = SA_m2) |> 
    dplyr::filter(Reservoir == unique(current_df_1$Reservoir)) 
  
  # BVR requires flexible bathymetry to generate schmidt stability
  if (current_df$Reservoir[1] == 'BVR') {
    
    #Create a dataframe with bathymetry at each date
    flexible_bathy <- current_df_1 |> # takes the depth at each day
      dplyr::distinct(date, WaterLevel, Reservoir) |> 
      dplyr::full_join(bathymetry, multiple = 'all') %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(Depth_m = Depth_m - (max(Depth_m) - mean(unique(WaterLevel))),
                    WaterLevel = mean(WaterLevel)) %>%
      dplyr::filter(Depth_m>=0) |> 
      dplyr::distinct()
    
  }
  
  if (current_df$Reservoir[1] == 'FCR') {
    flexible_bathy <- current_df_1|> 
      dplyr::full_join(bathymetry, multiple = 'all')
  }
  
  #Calculate schmidt stability each day
  schmidts <- numeric(length(unique(current_df_1$date)))
  dates <- unique(current_df_1$date)
  
  for(i in 1:length(dates)) {
    baths <- flexible_bathy %>%
      dplyr::filter(date==dates[i])
    
    temps <- current_df_1 %>%
      dplyr::filter(date == dates[i], 
                    # cannot have an observation at a depth shallower than the 
                    # shallowest bathymetry (returns NA below) so these are filtered out
                    depth >= min(baths$Depth_m))
    
    
    
    schmidts[i] <- rLakeAnalyzer::schmidt.stability(wtr = temps$observation, 
                                                    depths = temps$depth, 
                                                    bthA = baths$SA_m2, 
                                                    bthD = baths$Depth_m, 
                                                    sal = rep(0,length(temps$observation)))
    
    print(i)
    
  }
  
  current_ss <- data.frame(datetime = unique(current_df_1$date), 
                           observation = schmidts,
                           variable = 'schmidt.stability')
  message('Current file ready')
  
  
  
  # read in historical data file
  # EDI
  infile <- tempfile()
  try(download.file(historic_file, infile, method="curl"))
  if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  historic_df <- readr::read_csv(infile, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp')) |>
    dplyr::rename_with(~gsub('ThermistorTemp_C', 'wtr', .x)) |>
    dplyr::rename_with(~gsub('surface', '0.1', .x))
  
}