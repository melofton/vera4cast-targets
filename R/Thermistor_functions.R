historic_file  <- "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f"
current_file <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv"

target_generation_ThermistorTemp_C_daily <- function(current_file, historic_file){

  ## read in current data file
  # Github, Googlesheet, etc.
  current_df <- readr::read_csv(current_file, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp')) |>
    tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                        names_to = 'depth',
                        names_prefix = 'ThermistorTemp_C_',
                        values_to = 'observation') |>
    dplyr::mutate(Reservoir = ifelse(Reservoir == 'FCR',
                                     'fcre',
                                     ifelse(Reservoir == 'BVR',
                                            'bvre', NA)),
                  date = lubridate::as_date(DateTime)) |>
    dplyr::group_by(date, Reservoir) |>
    dplyr::summarise(observation = mean(observation), .groups = 'drop') |>
    rename(site_id = Reservoir,
           datetime = date)
  message('Current file ready')

  # read in historical data file
  # EDI
  infile <- tempfile()
  try(download.file(historic_file, infile, method="curl"))
  if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  historic_df <- readr::read_csv(infile) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp')) |>
    tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                        names_to = 'depth',
                        names_prefix = 'ThermistorTemp_C_',
                        values_to = 'observation') |>
    dplyr::mutate(Reservoir = ifelse(Reservoir == 'FCR',
                                 'fcre',
                                 ifelse(Reservoir == 'BVR',
                                        'bvre', NA)),
                  date = lubridate::as_date(DateTime)) |>
    dplyr::group_by(date, Reservoir) |>
    dplyr::summarise(observation = mean(observation), .groups = 'drop') |>
    rename(site_id = Reservoir,
           datetime = date)
  message('EDI file ready')

  ## manipulate the data files to match each other


  ## bind the two files using row.bind()
  final_df <- bind_rows(historic_df, current_df) |>
    dplyr::mutate(variable = 'ThermistorTemp_C')
  ## Match data to flare targets file
  # Use pivot_longer to create a long-format table
  # for time specific - use midnight UTC values for daily
  # for hourly

  ## return dataframe formatted to match FLARE targets
  return(final_df)
}


target_generation_ThermistorTemp_C_hourly <- function(current_file, historic_file){

  ## read in current data file
  # Github, Googlesheet, etc.
  current_df <- readr::read_csv(current_file, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp')) |>
    tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                        names_to = 'depth',
                        names_prefix = 'ThermistorTemp_C_',
                        values_to = 'observation') |>
    dplyr::mutate(Reservoir = ifelse(Reservoir == 'FCR',
                                     'fcre',
                                     ifelse(Reservoir == 'BVR',
                                            'bvre', NA)),
                  date = lubridate::as_datetime(paste0(format(DateTime, "%Y-%m-%d %H"), ":00:00"))) |>
    dplyr::group_by(date, Reservoir) |>
    dplyr::summarise(observation = mean(observation), .groups = 'drop') |>
    rename(site_id = Reservoir,
           datetime = date)
  message('Current file ready')

  # read in historical data file
  # EDI
  infile <- tempfile()
  try(download.file(historic_file, infile, method="curl"))
  if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  historic_df <- readr::read_csv(infile) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp')) |>
    tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                        names_to = 'depth',
                        names_prefix = 'ThermistorTemp_C_',
                        values_to = 'observation') |>
    dplyr::mutate(Reservoir = ifelse(Reservoir == 'FCR',
                                     'fcre',
                                     ifelse(Reservoir == 'BVR',
                                            'bvre', NA)),
                  date = lubridate::as_datetime(paste0(format(DateTime, "%Y-%m-%d %H"), ":00:00"))) |>
    dplyr::group_by(date, Reservoir) |>
    dplyr::summarise(observation = mean(observation), .groups = 'drop') |>
    rename(site_id = Reservoir,
           datetime = date)
  message('EDI file ready')

  ## manipulate the data files to match each other


  ## bind the two files using row.bind()
  final_df <- bind_rows(historic_df, current_df) |>
    dplyr::mutate(variable = 'ThermistorTemp_C')
  ## Match data to flare targets file
  # Use pivot_longer to create a long-format table
  # for time specific - use midnight UTC values for daily
  # for hourly

  ## return dataframe formatted to match FLARE targets
  return(final_df)
}


