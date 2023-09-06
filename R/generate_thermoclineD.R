generate_thermocline_depth <- function(current_file, historic_file) {


  ## Read in current data
  # Github, Googlesheet, etc.
  current_df <- readr::read_csv(current_file, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp')) |>
    dplyr::rename_with(~gsub('ThermistorTemp_C', 'wtr', .x)) |>
    dplyr::rename_with(~gsub('surface', '0.1', .x))

  # what is the maximum depth?
  max_depth <- ifelse(current_df$Reservoir[1] == 'FCR',
                      9.3, ifelse(current_df$Reservoir[1] == 'BVR', 14.0, NA))

  # calculate the thermocline depth
  td_current <- rLakeAnalyzer::ts.thermo.depth(wtr = current_df[,-1],
                                               na.rm = F) |>
    dplyr::mutate(Reservoir = ifelse(unique(current_df$Reservoir) == 'FCR',
                                     'fcre',
                                     ifelse(unique(current_df$Reservoir) == 'BVR',
                                            'bvre', NA)),
                  date = lubridate::as_date(DateTime),
                  thermo.depth = ifelse(is.nan(thermo.depth), max_depth, thermo.depth)) |>
    na.omit() |>
    dplyr::group_by(date, Reservoir) |>
    dplyr::summarise(mean_thermo.depth = mean(thermo.depth, na.rm = T),
                     n = dplyr::n(),
                     .groups = 'drop') |>
    dplyr::mutate(mean_thermo.depth = ifelse(n < 144/2,
                                                  NA,
                                                  mean_thermo.depth)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
    dplyr::rename(site_id = Reservoir,
                  datetime = date) |>
    dplyr::select(-n)
  message('Current file ready')

  # read in historical data file
  # EDI
  infile <- tempfile()
  try(download.file(historic_file, infile, method="curl"))
  if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  historic_df <- readr::read_csv(historic_file, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp')) |>
    dplyr::rename_with(~gsub('ThermistorTemp_C', 'wtr', .x)) |>
    dplyr::rename_with(~gsub('surface', '0.1', .x))

   # calculate the thermocline depth
  td_historic <- rLakeAnalyzer::ts.thermo.depth(wtr = historic_df[,-1],
                                               na.rm = F) |>
    dplyr::mutate(Reservoir = ifelse(unique(historic_df$Reservoir) == 'FCR',
                                     'fcre',
                                     ifelse(unique(historic_df$Reservoir) == 'BVR',
                                            'bvre', NA)),
                  date = lubridate::as_date(DateTime),
                  thermo.depth = ifelse(is.nan(thermo.depth), max_depth, thermo.depth)) |>
    na.omit() |>
    dplyr::group_by(date, Reservoir) |>
    dplyr::summarise(mean_thermo.depth = mean(thermo.depth, na.rm = T),
                     n = dplyr::n(),
                     .groups = 'drop') |>
    dplyr::mutate(mean_thermo.depth = ifelse(n < 144/2,
                                             NA,
                                             mean_thermo.depth)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
    dplyr::rename(site_id = Reservoir,
                  datetime = date) |>
    dplyr::select(-n)
  message('EDI file ready')

  ## bind the two files using row.bind()
  final_df <- dplyr::bind_rows(td_historic, td_current) |>
    tidyr::pivot_longer(cols = mean_thermo.depth,
                        names_to = 'variable',
                        names_prefix = 'mean_',
                        values_to = 'observation')
  ## Match data to flare targets file
return(final_df)
}
