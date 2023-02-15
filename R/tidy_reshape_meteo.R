#' From meteospain to meteoland meteo objects
#'
#' Adapting meteospain meteo objects to meteoland meteo objects
#'
#' This function converts \code{meteospain} R package meteo objects to
#' compatible meteoland meteo objects by selecting the needed variables and
#' adapting the names to comply with meteoland requirements.
#'
#' @param meteo meteospain meteo object.
#' @param complete logical indicating if the meteo data missing variables
#' should be calculated (if possible). Default to FALSE.
#' @return a compatible meteo object to use with meteoland.
#'
#' @examples
#'
#' if (interactive()) {
#'   # meteospain data
#'   library(meteospain)
#'   mg_april_2022_data <- get_meteo_from(
#'     "meteogalicia",
#'     meteogalicia_options("daily", as.Date("2022-04-01"), as.Date("2022-04-30"))
#'   )
#'
#'   # just convert
#'   meteospain2meteoland(mg_april_2022_data)
#'   # convert and complete
#'   meteospain2meteoland(mg_april_2022_data, complete = TRUE)
#'
#' }
#'
#' @export meteospain2meteoland
meteospain2meteoland <- function(meteo, complete = FALSE) {

  # assertions
  assertthat::assert_that(
    "timestamp" %in% names(meteo),
    msg = "Provided data has no timestamp variable. Is this a meteospain dataset?"
  )
  assertthat::assert_that(
    is.logical(complete) && !is.na(complete),
    msg = "'complete' argument must be logical (FALSE or TRUE)"
  )

  # check dictionary
  dictionary <- .meteospain_variables_dictionary(TRUE)
  if ("mean_temperature" %in% names(meteo)) {
    dictionary <- .meteospain_variables_dictionary(FALSE)
  }

  .reshape_meteo(meteo, dictionary, complete)

}

#' From worldmet to meteoland meteo objects
#'
#' Adapting \code{\link[worldmet]{importNOAA}} meteo objects to meteoland meteo objects
#'
#' This function converts \code{\link[worldmet]{importNOAA}} meteo objects to
#' compatible meteoland meteo objects by selecting the needed variables and
#' adapting the names to comply with meteoland requirements. Also it aggregates
#' subdaily data as well as complete missing variables if possible (setting
#' \code{complete = TRUE})
#'
#' @param meteo worldmet meteo object.
#' @param complete logical indicating if the meteo data missing variables
#' should be calculated (if possible). Default to FALSE.
#' @return a compatible meteo object to use with meteoland.
#'
#' @examples
#'
#' if (interactive()) {
#'   # worldmet data
#'   library(worldmet)
#'   worldmet_stations <- worldmet::getMeta(lat = 42, lon = 0, n = 2, plot = FALSE)
#'   worldmet_subdaily_2022 <-
#'     worldmet::importNOAA(worldmet_stations$code, year = 2022, hourly = TRUE)
#'
#'   # just convert
#'   worldmet2meteoland(worldmet_subdaily_2022)
#'   # convert and complete
#'   worldmet2meteoland(worldmet_subdaily_2022, complete = TRUE)
#'
#' }
#'
#' @export
worldmet2meteoland <- function(meteo, complete = FALSE) {

  # assertions
  assertthat::assert_that(
    all(c("date", "code") %in% names(meteo)),
    msg = "Provided data has no date or code variables. Is this a worldmet dataset?"
  )
  assertthat::assert_that(
    is.logical(complete) && !is.na(complete),
    msg = "'complete' argument must be logical (FALSE or TRUE)"
  )

  meteo |>
    # Sometimes, on worldmet, some points lack data (inclided spatial), so we remove those
    dplyr::filter(!is.na(.data$latitude)) |>
    # We convert to sf
    sf::st_as_sf(
      coords = c("longitude", "latitude"), crs = sf::st_crs(4326)
    ) |>
    # And reshape
    .reshape_meteo(
      dictionary = .worldmet_variables_dictionary(),
      complete = complete
    )

}

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
    # before anything else, we need to convert stationID to character (worldmet returns a factor)
    dplyr::mutate(stationID = as.character(.data$stationID)) |>
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
          # seems like a recurrent error in sf that renaming variables leads to
          # bad sf objects. With this we ensure a correct sf each loop.
          sf::st_sf() |>
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
  geometry_name <- .get_geometry_name(meteo)

  grouped_meteo <- meteo |>
    dplyr::as_tibble() |>
    dplyr::select(-!!geometry_name) |>
    dplyr::mutate(
      dates = lubridate::floor_date(.data$dates, unit = "day")
    ) |>
    dplyr::group_by(.data$dates, .data$stationID)

  station_date_values <- grouped_meteo |>
    dplyr::count() |>
    dplyr::pull("n")

  # if only one value per station and date exists for all combinations,
  # no need of summarising
  if (!any(station_date_values > 1)) {
    return(meteo)
  }

  cli::cli_warn(c(
    "Provided meteo data seems to be in subdaily time steps, aggregating to daily scale"
  ))

  grouped_meteo |>
    .create_missing_vars() |>
    # ensure solar radiation is never negative
    dplyr::mutate(
      Radiation = dplyr::if_else(.data$Radiation < 0, 0, .data$Radiation)
    ) |>
    # summarise
    dplyr::summarise(
      stationID = .is_na_or_fun(.data$stationID, dplyr::first),
      elevation = .is_na_or_fun(.data$elevation, dplyr::first),
      MeanTemperature = .is_na_or_fun(.data$Temperature, mean, na.rm = TRUE),
      MinTemperature = .is_na_or_fun(.data$Temperature, min, .data$MinTemperature, na.rm = TRUE),
      MaxTemperature = .is_na_or_fun(.data$Temperature, max, .data$MaxTemperature, na.rm = TRUE),
      MeanRelativeHumidity = .is_na_or_fun(.data$RelativeHumidity, mean, na.rm = TRUE),
      MinRelativeHumidity = .is_na_or_fun(.data$RelativeHumidity, min, na.rm = TRUE),
      MaxRelativeHumidity = .is_na_or_fun(.data$RelativeHumidity, max, na.rm = TRUE),
      Precipitation = .is_na_or_fun(.data$Precipitation, sum, na.rm = TRUE),
      WindSpeed = .is_na_or_fun(.data$WindSpeed, mean, na.rm = TRUE),
      WindDirection = .is_na_or_fun(.data$WindDirection, .summarise_wind_direction),
      Radiation = .is_na_or_fun(.data$Radiation, sum, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      meteo |>
        dplyr::ungroup() |>
        dplyr::select("stationID", !!geometry_name) |>
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

  geometry_name <- .get_geometry_name(meteo)

  distinct_rows <- meteo |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$dates) |>
    dplyr::select("stationID", !!geometry_name) |>
    dplyr::distinct()

  # If more geometries than station IDs, issue a warning and filter by the last
  # geometry to remove duplicates
  if (nrow(distinct_rows) != length(unique(distinct_rows[["stationID"]]))) {
    cli::cli_warn(c(
      "Some stations have different metadata (elevation, coordinates...) for different dates.",
      "i" = "Choosing the most recent metadata"
    ))

    distinct_rows <- distinct_rows |>
      dplyr::group_by(.data$stationID) |>
      dplyr::filter(as.character(.data[[geometry_name]]) == dplyr::last(as.character(.data[[geometry_name]])))

    meteo <- meteo |>
      dplyr::as_tibble() |>
      dplyr::select(-!!geometry_name) |>
      dplyr::left_join(distinct_rows, by = 'stationID') |>
      # duplicated rows has been generated in the aggregation step due to different geometries for
      # the same stationID. We remove those duplicates
      dplyr::distinct() |>
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


#' Complete missing meteo variables
#'
#' Calculates missing meteo variables
#'
#' This function takes a meteo object (with meteoland names) and complete any
#' missing variable if it is possible
#'
#' @param meteo meteoland meteo data
#' @param verbose Logical indicating if the function must show messages and info.
#' Default value checks \code{"meteoland_verbosity"} option and if not set, defaults
#' to TRUE. It can be turned off for the function with FALSE, or session wide with
#' \code{options(meteoland_verbosity = FALSE)}
#' @return the same \code{meteo} data provided with the the variables completed
#'
#' @examples
#'
#' \donttest{
#' # example data
#' data("meteoland_meteo_example")
#'
#' # remove MinRelativeHumidity
#' meteoland_meteo_example$MinRelativeHumidity <- NULL
#' # complete vars
#' completed_meteo <- complete_meteo(meteoland_meteo_example)
#' # check MinRelativeHumidity
#' completed_meteo$MinRelativeHumidity
#' }
#'
#' @export complete_meteo
complete_meteo <- function(meteo, verbose = getOption("meteoland_verbosity", TRUE)) {

  # assertions
  assertthat::assert_that(has_meteo(meteo))
  assertthat::assert_that(has_topo(meteo))

  .verbosity_control(
    cli::cli_alert_info("Completing missing variables if possible:"),
    verbose
  )

  # This no!!! We need a function for each row
  .complete_relative_humidity <- function(RelativeHumidity, MinTemperature, MeanTemperature, verbose) {

    .verbosity_control(
      cli::cli_ul("RelativeHumidity"),
      verbose
    )

    purrr::pmap_dbl(
      list(RelativeHumidity, MinTemperature, MeanTemperature),
      ~ dplyr::if_else(
        is.na(..1),
        max(min(100 * utils_saturationVP(..2) / utils_saturationVP(..3), 100), 0),
        ..1
      )
    )
  }

  .complete_min_relative_humidity <- function(MinRelativeHumidity, MinTemperature, MaxTemperature, verbose) {

    .verbosity_control(
      cli::cli_ul("MinRelativeHumidity"),
      verbose
    )

    purrr::pmap_dbl(
      list(MinRelativeHumidity, MinTemperature, MaxTemperature),
      ~ dplyr::if_else(
        is.na(..1),
        max(min(100 * utils_saturationVP(..2) / utils_saturationVP(..3), 100), 0),
        ..1
      )
    )
  }

  .complete_max_relative_humidity <- function(MaxRelativeHumidity, verbose) {
    .verbosity_control(
      cli::cli_ul("MaxRelativeHumidity"),
      verbose
    )

    dplyr::if_else(
      is.na(MaxRelativeHumidity), 100, MaxRelativeHumidity
    )
  }

  .complete_radiation <- function(
    Radiation, dates, MinTemperature, MaxTemperature,
    MinRelativeHumidity, MaxRelativeHumidity,
    Precipitation,
    elevation, slope, aspect,
    geometry, verbose
  ) {
    .verbosity_control(
      cli::cli_ul("Radiation"),
      verbose
    )

    julian_day <- purrr::map_int(
      dates,
      ~ radiation_julianDay(
        as.numeric(format(.x, "%Y")),
        as.numeric(format(.x, "%m")),
        as.numeric(format(.x, "%d"))
      )
    )
    solar_constant <- purrr::map_dbl(julian_day, radiation_solarConstant)
    declination <- purrr::map_dbl(julian_day, radiation_solarDeclination)
    average_vapour_pressure <- purrr::pmap_dbl(
      list(MinTemperature, MaxTemperature, MinRelativeHumidity, MaxRelativeHumidity),
      utils_averageDailyVP
    )
    slope_rad <- dplyr::if_else(
      is.na(slope), 0, slope*(pi/180)
    )
    aspect_rad <- dplyr::if_else(
      is.na(aspect), 0, aspect*(pi/180)
    )
    latitude_rad <-
      sf::st_coordinates(sf::st_transform(geometry, 4326))[,2] * (pi/180)

    diff_temp <- MaxTemperature - MinTemperature

    purrr::pmap_dbl(
      list(
        solar_constant, latitude_rad, elevation, slope_rad,
        aspect_rad, declination, diff_temp, diff_temp,
        average_vapour_pressure, Precipitation
      ),
      radiation_solarRadiation
    )
  }

  if (is.null(meteo[["MeanTemperature"]])) {
    meteo[["MeanTemperature"]] <- NA_real_
  }

  if (is.null(meteo[["MeanRelativeHumidity"]])) {
    meteo[["MeanRelativeHumidity"]] <- NA_real_
  }

  if (is.null(meteo[["MinRelativeHumidity"]])) {
    meteo[["MinRelativeHumidity"]] <- NA_real_
  }

  if (is.null(meteo[["MaxRelativeHumidity"]])) {
    meteo[["MaxRelativeHumidity"]] <- NA_real_
  }

  if (is.null(meteo[["Radiation"]])) {
    meteo[["Radiation"]] <- NA_real_
  }

  if (is.null(meteo[["aspect"]])) {
    meteo[["aspect"]] <- NA_real_
  }

  if (is.null(meteo[["slope"]])) {
    meteo[["slope"]] <- NA_real_
  }

  meteo_completed <- meteo |>
    dplyr::mutate(
      MeanRelativeHumidity = .complete_relative_humidity(
        .data$MeanRelativeHumidity, .data$MinTemperature,
        .data$MeanTemperature, verbose
      ),
      MinRelativeHumidity = .complete_min_relative_humidity(
        .data$MinRelativeHumidity, .data$MinTemperature,
        .data$MaxTemperature, verbose
      ),
      MaxRelativeHumidity = .complete_max_relative_humidity(.data$MaxRelativeHumidity, verbose),
      Radiation = .complete_radiation(
        .data$Radiation, .data$dates, .data$MinTemperature, .data$MaxTemperature,
        .data$MinRelativeHumidity, .data$MaxRelativeHumidity,
        .data$Precipitation,
        .data$elevation, .data$slope, .data$aspect,
        sf::st_geometry(meteo), verbose
      )
    )

  .verbosity_control(
    cli::cli_alert_success("Done"),
    verbose
  )

  return(meteo_completed)
}
















