#' Reshape meteo API
#'
#' Convert meteo to meteoland complying sf
#'
#' The logic of the API is simple. If we have a dictionary with the corresponding names
#' we can just drop units (just in case, looking at you meteospain) and rename variables with the
#' dictionary. Once we have renamed the variables, the rest of the logic is the same independently
#' of the origin of the data:
#'
#' (I) Aggregate meteo if we detect subdaily, (II) fix possible mismatches between stations
#' and coordinates (sometimes, coordinates get updated and you end with a station with two sets
#' of coordinates, looking at you AEMET), and (III), if complete is TRUE, then complete the meteo
#' vars if possible.
#'
#' @section dictionary:
#' A dictionary is a named list, in which each element is the meteo object variable names. The list
#' is named with the corresponding official meteoland meteo names. For example, for worldmet
#' datasets, the \code{link{worldmet2meteoland}} function uses as dictionary the follwing list:
#'
#' \code{
#'    list(
#'      dates = "date",
#'      stationID = "code",
#'      elevation = "elev",
#'      WindDirection = "wd",
#'      WindSpeed = "ws",
#'      Temperature = "air_temp",
#'      RelativeHumidity = "RH",
#'      Precipitation = "precip_6"
#'    )
#' }
#'
#' @param meteo Meteo sf object
#' @param dictionary A dictionary (named list) with the names of the meteo object variables.
#'   See details.
#'
#' @return A meteo sf object complying with meteoland \code{\link{with_meteo}} standard
#'
#' @noRd
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

#' Rename meteo object variables
#'
#' Rename meteo object variables to \code{\link{with_meteo}} standard
#'
#' @param meteo meteo sf object, unitless
#' @param dictionary A dictionary (named list) with the names of the meteo object variables.
#'
#' @return A meteo sf object compatible with \code{\link{with_meteo}}
#'
#' @noRd
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

#' Worldmet subdaily dictionary
#'
#' Helper for creating the variable dictionary for \code{\link{worldmet2meteoland}}
#'
#' @return A dictionary (named list) with the worldmet and meteoland variables correspondences
#' @noRd
.worldmet_variables_dictionary <- function() {
  res <- list(
    dates = "date",
    stationID = "code",
    elevation = "elev",
    WindDirection = "wd",
    WindSpeed = "ws",
    Temperature = "air_temp",
    RelativeHumidity = "RH",
    Precipitation = "precip_6"
  )

  return(res)
}

#' meteospain subdaily dictionary
#'
#' Helper for creating the variable dictionary for \code{\link{meteospain2meteoland}}
#'
#' @param subdaily Logical. If meteospain data is subdaily, should be set to TRUE. This is done
#'   automatically if called from \code{\link{meteospain2meteoland}}
#'
#' @return A dictionary (named list) with the meteospain and meteoland variables correspondences
#' @noRd
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

#' Aggregating subdaily meteo
#'
#' Aggregate subdaily meteo
#'
#' Function first check if data is subdaily by counting the ocurrences of station code in the same
#' day. If more than one, aggregating is enabled. Each variable is aggregated with the corresponding
#' function (\code{sum} for precipitation, \code{min} for minimums...)
#'
#' @param meteo meteo sf object with variables names already complying with \code{\link{with_meteo}}
#'
#' @return A daily aggregated meteo object ready to \code{\link{with_meteo}}
#' @noRd
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

#' Helper to fix geometries problems
#'
#' Get the latest geometry for the meteo station
#'
#' Sometimes (AEMET), coordinates get updated, and data ends having differents coordinates in
#' different dates for the same station. This helper ensures to get the latest coordinates values
#' for a station when more than one set of coordinates is found
#'
#' @param meteo A daily meteo object complying with \code{\link{with_meteo}}
#'
#' @return The same meteo object but with only one set of coordinates per station
#' @noRd
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

#' Helper for summarizing wind direction
#'
#' Helper for summarizing wind direction
#'
#' @param wind_direction Vector with subdaily values of wind direction
#'
#' @returns The "average" wind direction for the vector provided
#' @noRd
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
#' Helper for creating NA variables
#'
#' Helper for creating NA variables with double class instead of logical
#'
#' When applying a function to an all NA vector, things go bad. This helper ensures that the
#' summarising funcion can be applied, and if all the vector is NA, then returns a NA_real_ to
#' maintaing the vector class in the tibble.
#'
#' @param var Variable vector
#' @param fun Function to apply
#' @param ... Arguments for fun
#'
#' @return The result of fun(var) if possible, NA_real_ if not
#' @noRd
.is_na_or_fun <- function(var, fun, ...) {
  if (all(is.na(var))) {
    return(NA_real_)
  }
  fun(var, ...)
}

# create missing vars

#' Helper to create missing vars in meteo
#'
#' Helper to create missing vars in meteo
#'
#' If any variable needed for summarising is missing, errors ensue. So we made sure any needed
#' variable is there
#'
#' @param meteo meteo sf object complying with \code{\link{with_meteo}}
#'
#' @return meteo sf object with missing empty variables (NA_real_)
#' @noRd
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
  if (!"Precipitation" %in% meteo_names) {
    meteo$Precipitation <- NA_real_
  }
  if (!"RelativeHumidity" %in% meteo_names) {
    meteo$RelativeHumidity <- NA_real_
  }
  return(meteo)
}
