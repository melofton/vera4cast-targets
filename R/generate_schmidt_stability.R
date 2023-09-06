generate_schmidt_stability <- function(current_file, historic_file) {

  # Need bathymetry
  infile <- tempfile()
  try(download.file("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184",
                    infile, method="curl"))
  if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  bathymetry <- readr::read_csv(infile, show_col_types = F)  |>
    dplyr::select(Reservoir, Depth_m, SA_m2) |>
    dplyr::rename(depths = Depth_m,
                  areas = SA_m2)


  ## Read in current data
  # Github, Googlesheet, etc.
  current_df <- readr::read_csv(current_file, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp')) |>
    dplyr::rename_with(~gsub('ThermistorTemp_C', 'wtr', .x)) |>
    dplyr::rename_with(~gsub('surface', '0.1', .x))

  ss_current <- rLakeAnalyzer::ts.schmidt.stability(wtr = current_df[,-1],
                                                    bathy = subset(bathymetry,
                                                                   Reservoir == unique(current_df$Reservoir))[,-1]) |>
    dplyr::mutate(Reservoir = ifelse(unique(current_df$Reservoir) == 'FCR',
                                     'fcre',
                                     ifelse(unique(current_df$Reservoir) == 'BVR',
                                            'bvre', NA)),
                  date = lubridate::as_date(DateTime)) |>
    na.omit() |>
    dplyr::group_by(date, Reservoir) |>
    dplyr::summarise(mean_schmidt.stability = mean(schmidt.stability, na.rm = T),
                     n = dplyr::n(),
                     .groups = 'drop') |>
    dplyr::mutate(mean_schmidt.stability = ifelse(n < 144/2,
                                                  NA,
                                                  mean_schmidt.stability)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
    dplyr::rename(site_id = Reservoir,
                  datetime = date) |>
    dplyr::select(-n)
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

  ss_historic <- rLakeAnalyzer::ts.schmidt.stability(wtr = historic_df[,-1],
                                                     bathy = subset(bathymetry,
                                                                    Reservoir == unique(historic_df$Reservoir))[,-1]) |>
    dplyr::mutate(Reservoir = ifelse(unique(current_df$Reservoir) == 'FCR',
                                     'fcre',
                                     ifelse(unique(current_df$Reservoir) == 'BVR',
                                            'bvre', NA)),
                  date = lubridate::as_date(DateTime)) |>
    na.omit() |>
    dplyr::group_by(date, Reservoir) |>
    dplyr::summarise(mean_schmidt.stability = mean(schmidt.stability, na.rm = T),
                     n = dplyr::n(),
                     .groups = 'drop') |>
    dplyr::mutate(mean_schmidt.stability = ifelse(n < 144/2,
                                                  NA,
                                                  mean_schmidt.stability)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
    dplyr::rename(site_id = Reservoir,
                  datetime = date) |>
    dplyr::select(-n)
  message('EDI file ready')

  ## bind the two files using row.bind()
  final_df <- dplyr::bind_rows(ss_historic, ss_current) |>
    tidyr::pivot_longer(cols = mean_schmidt.stability,
                        names_to = 'variable',
                        names_prefix = 'mean_',
                        values_to = 'observation')
  ## Match data to flare targets file

}
