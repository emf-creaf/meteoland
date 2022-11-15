worldmet2meteoland <- function(meteo, complete = FALSE) {
  # assertions
  assertthat::assert_that(
    "date" %in% names(meteo),
    msg = "Provided data has no date variable. Is this a worldmet dataset?"
  )
  assertthat::assert_that(
    is.logical(complete) && !is.na(complete),
    msg = "'complete' argument must be logical (FALSE or TRUE)"
  )

  # Renaming mandatory variables
  meteo_temp <- meteo |>
    # .fix_station_geometries() |>
    # .aggregate_subdaily_worldmet() |>
    dplyr::select(
      dates = date, stationID = code,
      elevation = elev,
      WindDirection = wd, WindSpeed = ws,
      MeanTemperature = mean_air_temp,
      MinTemperature = min_air_temp, MaxTemperature = max_air_temp,
      MeanRelativeHumidity = mean_RH,
      MinRelativeHumidity = min_RH, MaxRelativeHumidity = max_RH,
      Precipitation = precipitation,
      everything()
    )
}

#' From meteospain to meteoland meteo objects
#'
#' Adapting meteospain meteo objects to meteoland meteo objects
#'
#' This function converts \code{meteospain} R package meteo objects to
#' compatible meteoland meteo objects by seelcting the needed variables and
#' adapting the names to comply with meteoland requirements.
#'
#' @param meteo meteospain meteo object.
#' @param complete logical indicating if the meteo data missing variables
#' should be calculated (if possible). Default to FALSE.
#' @return a compatible meteo object to use with meteoland.
#'
#' @examples
#'
#' # meteospain data
#' library(meteospain)
#' mg_april_2022_data <- get_meteo_from(
#'   "meteogalicia",
#'   meteogalicia_options("daily", as.Date("2022-04-01"), as.Date("2022-04-30"))
#' )
#'
#' # just convert
#' meteospain2meteoland(mg_april_2022_data)
#' # convert and complete
#' meteospain2meteoland(mg_april_2022_data, complete = TRUE)
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

  # Renaming mandatory variables
  meteo_temp <- meteo |>
    units::drop_units() |>
    .fix_station_geometries() |>
    .aggregate_subdaily_meteospain() |>
    dplyr::select(
      dates = timestamp, stationID = station_id,
      elevation = altitude,
      MinTemperature = min_temperature, MaxTemperature = max_temperature,
      Precipitation = precipitation,
      everything()
    )

  # Renaming optional
  if ("mean_temperature" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(MeanTemperature = mean_temperature)
  }

  if ("mean_relative_humidity" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(RelativeHumidity = mean_relative_humidity)
  }

  if ("min_relative_humidity" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(MinRelativeHumidity = min_relative_humidity)
  }

  if ("max_relative_humidity" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(MaxRelativeHumidity = max_relative_humidity)
  }

  if ("mean_wind_speed" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(WindSpeed = mean_wind_speed)
  }

  if ("mean_wind_direction" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(WindDirection = mean_wind_direction)
  }

  # if ("radiation" %in% names(meteo_temp)) {
  #   meteo_temp <- meteo_temp |>
  #     dplyr::rename(Radiation = radiation)
  # }

  if ("solar_radiation" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(Radiation = solar_radiation)
  }

  if ("global_solar_radiation" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(Radiation = global_solar_radiation)
  }

  res <- meteo_temp |>
    dplyr::select(dplyr::any_of(
      c(
        "dates", "stationID", "elevation", "aspect", "slope",
        "MeanTemperature", "MinTemperature", "MaxTemperature",
        "Precipitation",
        "RelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
        "WindDirection", "WindSpeed",
        "Radiation"
      )
    ))

  # complete step
  if (isTRUE(complete)) {
    res <- complete_meteo(res)
  }

  return(res)
}

.aggregate_subdaily_meteospain <- function(meteo) {

  # browser()
  # check if data is subdaily
  grouped_meteo <- meteo |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      timestamp = lubridate::floor_date(timestamp, unit = "day")
    ) |>
    dplyr::group_by(timestamp, station_id)

  station_date_values <- grouped_meteo |>
    dplyr::count() |>
    dplyr::pull(n)

  # if only one value per station and date exists for all combinations, no need of summarising
  if (!any(station_date_values > 1)) {
    return(meteo)
  }

  usethis::ui_info("Provided meteospain data seems to be in subdaily time steps, aggregating to daily scale")

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

  grouped_meteo |>
    .create_missing_vars() |>
    dplyr::mutate(
      global_solar_radiation = dplyr::if_else(
        global_solar_radiation < 0, 0, global_solar_radiation
      )
    ) |>
    dplyr::summarise(
      # service = .is_na_or_fun(service, dplyr::first),
      station_id = .is_na_or_fun(station_id, dplyr::first),
      station_name = .is_na_or_fun(station_name, dplyr::first),
      # station_province = .is_na_or_fun(station_province, dplyr::first),
      altitude = .is_na_or_fun(altitude, dplyr::first),
      mean_temperature = .is_na_or_fun(temperature, mean, na.rm = TRUE),
      min_temperature = .is_na_or_fun(temperature, min, min_temperature, na.rm = TRUE),
      max_temperature = .is_na_or_fun(temperature, max, max_temperature, na.rm = TRUE),
      mean_relative_humidity = .is_na_or_fun(relative_humidity, mean, na.rm = TRUE),
      min_relative_humidity = .is_na_or_fun(relative_humidity, min, na.rm = TRUE),
      max_relative_humidity = .is_na_or_fun(relative_humidity, max, na.rm = TRUE),
      precipitation = .is_na_or_fun(precipitation, sum, na.rm = TRUE),
      mean_wind_speed = .is_na_or_fun(wind_speed, mean, na.rm = TRUE),
      mean_wind_direction = .is_na_or_fun(wind_direction, .summarise_wind_direction),
      global_solar_radiation = .is_na_or_fun(global_solar_radiation, sum, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      meteo |>
        dplyr::ungroup() |>
        dplyr::select(station_id, geometry) |>
        dplyr::distinct(),
      by = c('station_id')
    ) |>
    sf::st_as_sf()
}



#' Complete missing meteo variables
#'
#' Calculates missing meteo variables
#'
#' This function takes a meteo object (with meteoland names) and complete any
#' missing variable if it is possible
#'
#' @param meteo meteoland meteo data
#' @return the same \code{meteo} data provided with the the variables completed
#'
#' @examples
#'
#' # example data
#' data("meteoland_meteo_example")
#'
#' # remove MinRelativeHumidity
#' meteoland_meteo_example$MinRelativeHumidity <- NULL
#' # complete vars
#' completed_meteo <- complete_meteo(meteoland_meteo_example)
#' # check MinRelativeHumidity
#' completed_meteo$MinRelativeHumidity
#'
#' @export complete_meteo
complete_meteo <- function(meteo) {

  # assertions
  assertthat::assert_that(has_meteo(meteo))
  assertthat::assert_that(has_topo(meteo))

  usethis::ui_info(
    "Completing missing variables if possible:"
  )

  # This no!!! We need a function for each row
  .complete_relative_humidity <- function(RelativeHumidity, MinTemperature, MeanTemperature) {

    usethis::ui_todo("RelativeHumidity")

    purrr::pmap_dbl(
      list(RelativeHumidity, MinTemperature, MeanTemperature),
      ~ dplyr::if_else(
        is.na(..1),
        max(min(100 * utils_saturationVP(..2) / utils_saturationVP(..3), 100), 0),
        ..1
      )
    )
  }

  .complete_min_relative_humidity <- function(MinRelativeHumidity, MinTemperature, MaxTemperature) {
    usethis::ui_todo("MinRelativeHumidity")

    purrr::pmap_dbl(
      list(MinRelativeHumidity, MinTemperature, MaxTemperature),
      ~ dplyr::if_else(
        is.na(..1),
        max(min(100 * utils_saturationVP(..2) / utils_saturationVP(..3), 100), 0),
        ..1
      )
    )
  }

  .complete_max_relative_humidity <- function(MaxRelativeHumidity) {
    usethis::ui_todo("MaxRelativeHumidity")

    dplyr::if_else(
      is.na(MaxRelativeHumidity), 100, MaxRelativeHumidity
    )
  }

  .complete_radiation <- function(
    Radiation, dates, MinTemperature, MaxTemperature,
    MinRelativeHumidity, MaxRelativeHumidity,
    Precipitation,
    elevation, slope, aspect,
    geometry
  ) {
    usethis::ui_todo("Radiation")

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

  # browser()

  if (is.null(meteo[["MeanTemperature"]])) {
    meteo[["MeanTemperature"]] <- NA_real_
  }

  if (is.null(meteo[["RelativeHumidity"]])) {
    meteo[["RelativeHumidity"]] <- NA_real_
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
    # dplyr::as_tibble() |>
    dplyr::mutate(
      RelativeHumidity = .complete_relative_humidity(
        RelativeHumidity, MinTemperature, MeanTemperature
      ),
      MinRelativeHumidity = .complete_min_relative_humidity(
        MinRelativeHumidity, MinTemperature, MaxTemperature
      ),
      MaxRelativeHumidity = .complete_max_relative_humidity(MaxRelativeHumidity),
      Radiation = .complete_radiation(
        Radiation, dates, MinTemperature, MaxTemperature,
        MinRelativeHumidity, MaxRelativeHumidity,
        Precipitation,
        elevation, slope, aspect,
        sf::st_geometry(meteo)
      )
    )

  usethis::ui_done("Done")

  return(meteo_completed)
}
















