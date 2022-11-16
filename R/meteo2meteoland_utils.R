.aggregate_subdaily_meteo <- function(subdaily, dictionary) {
  
  # browser()
  # check if data is subdaily
  
  grouped_meteo <- meteo |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      tmp_day_date = lubridate::floor_date(dictionary$date_var, unit = "day")
    ) |>
    dplyr::group_by(tmp_day_date, dictionary$station_id_var)
  
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
    .create_missing_vars()
  
}

meteo_dictionary <- function(
    date_var = "timestamp",
    station_id_var = "station_id"
    
) {
  return(list(
    date_var = date_var,
    station_id_var = station_id_var
  ))
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
  if (!"min_temperature" %in% meteo_names) {
    meteo$min_temperature <- NA_real_
  }
  if (!"max_temperature" %in% meteo_names) {
    meteo$max_temperature <- NA_real_
  }
  if (!"global_solar_radiation" %in% meteo_names) {
    meteo$global_solar_radiation <- NA_real_
  }
  return(meteo)
}
