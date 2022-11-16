.reshape_meteo <- function(meteo, dictionary, complete) {
  
  # browser()
  
  processed_meteo <- meteo |>
    units::drop_units() |>
    # rename vars
    .rename_meteo_vars(dictionary) |>
    # aggregate subdaily if needed
    .aggregate_subdaily_meteo() |>
    # fix station geometries if needed (meteospain)
    .fix_station_geometries()
  
  if (complete) {
    processed_meteo <- processed_meteo |>
      complete_meteo()
  }
  
  return(processed_meteo)
  
}

.rename_meteo_vars <- function(meteo, dictionary) {
  
  # copy the meteo
  meteo_res <- meteo
  
  # walk through the dictionary and update meteo copy
  purrr::iwalk(
    dictionary,
    .f = function(var, name) {
      if (var %in% names(meteo)) {
        meteo_res <<- meteo_res |>
          dplyr::rename(!!name := !!var)
      }
    }
  )
  
  return(meteo_res)
}

.meteospain_variables_dictionary <- function(subdaily = TRUE) {
  # daily
  res <- list(
    dates = "timestamp",
    stationID = "station_id",
    elevation = "altitude",
    MeanTemperature = "mean_temperature",
    MinTemperature = "min_temperature",
    MaxTemperature = "max_temperature",
    MeanRelativeHumidity = "mean_relative_humidity",
    MinRelativeHumidity = "min_relative_humidity",
    MaxRelativeHumidity = "max_relative_humidity",
    Precipitation = "precipitation",
    WindDirection = "mean_wind_direction",
    WindSpeed = "mean_wind_speed",
    Radiation = "global_solar_radiation"
  )
  
  if (subdaily) {
    # subdaily
    res <- list(
      dates = "timestamp",
      stationID = "station_id",
      elevation = "altitude",
      Temperature = "temperature",
      MinTemperature = "min_temperature",
      MaxTemperature = "max_temperature",
      RelativeHumidity = "relative_humidity",
      Precipitation = "precipitation",
      WindDirection = "wind_direction",
      WindSpeed = "wind_speed",
      Radiation = "global_solar_radiation"
    )
  }
  
  return(res)
  
}


.aggregate_subdaily_meteo <- function(meteo) {
  
  # browser()
  # check if data is subdaily
  
  grouped_meteo <- meteo |>
    dplyr::as_tibble() |>
    dplyr::select(-geometry) |>
    dplyr::mutate(
      dates = lubridate::floor_date(dates, unit = "day")
    ) |>
    dplyr::group_by(dates, stationID)
  
  station_date_values <- grouped_meteo |>
    dplyr::count() |>
    dplyr::pull(n)
  
  # if only one value per station and date exists for all combinations,
  # no need of summarising
  if (!any(station_date_values > 1)) {
    return(meteo)
  }
  
  usethis::ui_info(
    "Provided meteo data seems to be in subdaily time steps, aggregating to daily scale"
  )
  
  grouped_meteo |>
    .create_missing_vars() |>
    # ensure solar radiation is never negative
    dplyr::mutate(
      Radiation = dplyr::if_else(Radiation < 0, 0, Radiation)
    ) |>
    # summarise
    dplyr::summarise(
      stationID = .is_na_or_fun(stationID, dplyr::first),
      elevation = .is_na_or_fun(elevation, dplyr::first),
      MeanTemperature = .is_na_or_fun(Temperature, mean, na.rm = TRUE),
      MinTemperature = .is_na_or_fun(Temperature, min, MinTemperature, na.rm = TRUE),
      MaxTemperature = .is_na_or_fun(Temperature, max, MaxTemperature, na.rm = TRUE),
      MeanRelativeHumidity = .is_na_or_fun(RelativeHumidity, mean, na.rm = TRUE),
      MinRelativeHumidity = .is_na_or_fun(RelativeHumidity, min, na.rm = TRUE),
      MaxRelativeHumidity = .is_na_or_fun(RelativeHumidity, max, na.rm = TRUE),
      Precipitation = .is_na_or_fun(Precipitation, sum, na.rm = TRUE),
      WindSpeed = .is_na_or_fun(WindSpeed, mean, na.rm = TRUE),
      WindDirection = .is_na_or_fun(WindDirection, .summarise_wind_direction),
      Radiation = .is_na_or_fun(Radiation, sum, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      meteo |>
        dplyr::ungroup() |>
        dplyr::select(stationID, geometry) |>
        dplyr::distinct(),
      by = c('stationID')
    ) |>
    sf::st_as_sf()
  
}

.fix_station_geometries <- function(meteo) {
  distinct_rows <- meteo |>
    dplyr::ungroup() |>
    dplyr::arrange(dates) |>
    dplyr::select(stationID, geometry) |>
    dplyr::distinct()
  
  # If more geometries than station IDs, issue a warning and filter by the last
  # geometry to remove duplicates
  if (nrow(distinct_rows) != length(unique(distinct_rows[["stationID"]]))) {
    usethis::ui_warn(c(
      "Some stations have different metadata (elevation, coordinates...) for ",
      "different dates. Choosing the most recent metadata"
    ))
    
    distinct_rows <- distinct_rows |>
      dplyr::group_by(stationID) |>
      dplyr::filter(as.character(geometry) == dplyr::last(as.character(geometry)))
    
    meteo <- meteo |>
      dplyr::as_tibble() |>
      dplyr::select(-geometry) |>
      dplyr::left_join(distinct_rows, by = 'stationID') |>
      sf::st_as_sf()
  }
  
  return(meteo)
  
}

# summarise wind function
.summarise_wind_direction <- function(wind_direction) {
  wind_direction <- as.numeric(wind_direction)
  y <- sum(cos(wind_direction * pi / 180), na.rm = TRUE) / length(wind_direction)
  x <- sum(sin(wind_direction * pi / 180), na.rm = TRUE) / length(wind_direction)
  dv <- (180 / pi) * atan2(x, y)
  
  if (dv < 0) {
    dv <- dv + 360
  }
  return(dv)
}

# na or fun, if all vector is NAs, return NAs, if not apply the function
.is_na_or_fun <- function(var, fun, ...) {
  if (all(is.na(var))) {
    return(NA_real_)
  }
  fun(var, ...)
}

# create missing vars
.create_missing_vars <- function(meteo) {
  meteo_names <- names(meteo)
  if (!"MinTemperature" %in% meteo_names) {
    meteo$MinTemperature <- NA_real_
  }
  if (!"MaxTemperature" %in% meteo_names) {
    meteo$MaxTemperature <- NA_real_
  }
  if (!"Radiation" %in% meteo_names) {
    meteo$Radiation <- NA_real_
  }
  return(meteo)
}
