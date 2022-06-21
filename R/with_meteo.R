#### check_meteo ####
has_meteo_names <- function(meteo) {
  # meteo names
  mandatory_meteo_names <- c("MinTemperature", "MaxTemperature")

  all(mandatory_meteo_names %in% names(meteo))
}

assertthat::on_failure(has_meteo_names) <- function(call, env) {
  paste0(
    "Names found in ", deparse(call$meteo), " don't comply with the required names:\n",
    deparse(call$meteo),
    " should have the following meteorology variables:\n",
    "  - MinTemperature ***\n", "  - MaxTemperature ***\n",
    "  - Precipitation\n", "  - RelativeHumidity\n",
    "  - Radiation\n", "  - WindSpeed\n", "  - WindDirection\n",
    "\n ***: mandatory variables"
  )
}

has_topo_names <- function(meteo) {
  # topology names
  mandatory_topo_names <- c("elevation")

  all(mandatory_topo_names %in% names(meteo))
}

assertthat::on_failure(has_topo_names) <- function(call, env) {
  paste0(
    "Names found in ", deparse(call$meteo), " don't comply with the required names:\n",
    deparse(call$meteo),
    " should have the following topology variables:\n",
    "  - elevation ***\n",
    "  - aspect\n", "  - orientation\n",
    "\n ***: mandatory variables\n"
  )
}

has_meteo <- function(meteo) {
  # sf object
  assertthat::assert_that(inherits(meteo, 'sf'), msg = "meteo must be an sf object")
  assertthat::assert_that(all(sf::st_is(meteo, 'POINT')), msg = "meteo station geometries must be POINT")

  # names
  assertthat::assert_that(has_meteo_names(meteo))

  # number when it matters
  assertthat::assert_that(
    is.numeric(meteo[["MinTemperature"]]),
    is.numeric(meteo[["MaxTemperature"]])
  )

  # dates
  assertthat::assert_that(
    "dates" %in% names(meteo),
    msg = "meteo must have a variable called dates with the interpolation time span"
  )
  assertthat::assert_that(
    assertthat::is.date(meteo$dates) || assertthat::is.time(meteo$dates),
    msg = "dates variable must be a date or time class"
  )

  assertthat::assert_that(
    "stationID" %in% names(meteo),
    msg = "meteo must have a stationID variable identifying the meteorological stations"
  )
}

has_topo <- function(topo, ...) {

  # dataframe or similar with station_id or geometry
  assertthat::assert_that(
    inherits(topo, 'data.frame'), msg = "topo must be a data.frame/tibble"
  )

  # var names
  assertthat::assert_that(has_topo_names(topo))

  # numeric what it has to be numeric
  assertthat::assert_that(is.numeric(topo[["elevation"]]))

  assertthat::assert_that(
    "stationID" %in% names(topo),
    msg = "topo must have a stationID variable identifying the meteorological stations"
  )
}

with_meteo <- function(meteo) {
  usethis::ui_info("Checking meteorology object...")
  assertthat::assert_that(has_meteo(meteo))
  usethis::ui_done("meteorology object ok")
  return(invisible(meteo))
}

add_topo <- function(meteo, topo) {

  assertthat::assert_that(has_meteo(meteo))
  usethis::ui_info("Checking topology object...")
  assertthat::assert_that(has_topo(topo))
  usethis::ui_done("topology object ok")

  usethis::ui_info("Adding topology to meteo (by station ID)...")
  res <- dplyr::left_join(meteo, dplyr::as_tibble(topo), by = 'stationID')
  usethis::ui_done("topology added")

  return(res)
}

create_meteo_interpolator <- function(meteo_with_topo, params = NULL, ...) {

  ## TODO messaging
  assertthat::assert_that(has_meteo(meteo_with_topo))
  assertthat::assert_that(has_topo(meteo_with_topo))

  assertthat::assert_that(
    is.null(params) || inherits(params, "list"),
    msg = "params must be NULL or a named list with the interpolation parameters"
  )

  usethis::ui_info("Creating interpolator...")

  # get params
  if (is.null(params)) {
    usethis::ui_warn("No interpolation parameters provided, using defaults")
    params <- defaultInterpolationParams()
  }

  # data preparation
  meteo_arranged <- meteo_with_topo |>
    dplyr::arrange(dates, stationID)

  stations <- meteo_arranged |>
    dplyr::select(stationID) |>
    dplyr::distinct() |>
    sf::st_geometry()
  dates <- unique(meteo_arranged$dates)

  interpolator_dims <- stars::st_dimensions(
    station = stations, date = dates
  )

  # helper
  .interpolator_arrays_creator <- function(variable, arranged_data, dims) {
    if (! variable %in% names(arranged_data)) {
      return(array(NA_integer_, dim = dims))
    }
    arranged_data |>
      # very important step, as we need all combinations to fill the arrays
      tidyr::complete(dates, stationID) |>
      dplyr::pull(!!variable) |>
      array(dim = dims)
  }

  stars_interpolator <-
    list(
      MinTemperature = "MinTemperature",
      MaxTemperature = "MaxTemperature",
      RelativeHumidity = "RelativeHumidity",
      Precipitation = "Precipitation",
      Radiation = "Radiation",
      WindDirection = "WindDirection",
      WindSpeed = "WindSpeed",
      elevation = "elevation",
      aspect = "aspect",
      orientation = "orientation",
      stationID = "stationID"
    ) %>%
    purrr::map(
      .interpolator_arrays_creator,
      arranged_data = meteo_arranged,
      dims = dim(interpolator_dims)
    ) |>
    purrr::discard(is.null) |>
    stars::st_as_stars(dimensions = interpolator_dims)


  # add smoothed vars
  usethis::ui_todo("Calculating smoothed variables...")
  stars_interpolator[["SmoothedPrecipitation"]] <-
    .temporalSmoothing(
      stars_interpolator[["Precipitation"]],
      params$St_Precipitation,
      TRUE
    )

  stars_interpolator[["SmoothedTemperatureRange"]] <-
    .temporalSmoothing(
      stars_interpolator[["MaxTemperature"]] - stars_interpolator[["MinTemperature"]],
      params$St_TemperatureRange,
      FALSE
    )

  # update initial Rp in params
  usethis::ui_todo("Updating intial_Rp parameter with the actual stations mean distance...")
  params$initial_Rp <- sf::st_distance(meteo_with_topo) |>
    as.numeric() |>
    mean(na.rm = TRUE)

  # set the params as an attribute of stars_interpolator
  attr(stars_interpolator, "params") <- params

  # return the interpolator
  usethis::ui_done("Interpolator created.")
  return(stars_interpolator)
}


# library(meteospain)
# library(sf)
# #### get meteo ####
# service_options <- aemet_options(
#   'daily', as.Date("2022-04-01"), as.Date("2022-04-30"),
#   api_key = keyring::key_get('aemet')
# )
#
# meteo_test <- get_meteo_from('aemet', service_options)
# # stations_info_test <- get_stations_info_from('aemet', service_options)
#
# meteo_test
#
# meteo_test |>
#   meteoland:::has_meteo()
#
# foo <- meteo_test |>
#   dplyr::rename(
#     elevation = altitude,
#     MinTemperature = min_temperature,
#     MaxTemperature = max_temperature,
#     stationID = station_id
#   )
#
# foo |>
#   meteoland:::has_meteo()
#
# bar <- foo |>
#   dplyr::rename(
#     dates = timestamp
#   )
#
# baz <- bar |>
#   dplyr::mutate(
#     dates = as.character(dates)
#   )
#
# xyz <- bar |>
#   dplyr::mutate(
#     MinTemperature = as.character(MinTemperature)
#   )
#
# xyzz <- bar |>
#   dplyr::mutate(elevation = as.character(elevation))
#
# fooz <- bar |>
#   dplyr::select(-stationID)
#
# meteoland:::with_meteo(meteo_test) # error meteo vars
# meteoland:::with_meteo(foo) # error no dates
# meteoland:::with_meteo(fooz) # error no stationID
# meteoland:::with_meteo(baz) # error bad dates
# meteoland:::with_meteo(xyz) # error bad meteo vars
# meteoland:::with_meteo(xyzz) # No error with bad topo, because we dont check topo
# meteoland:::with_meteo(bar) # no error, everything ok
#
#
# ## add topo tests ##
#
#
# meteo_ok <- bar |>
#   dplyr::select(-elevation)
#
# topo_tibble_ok <- bar |>
#   dplyr::as_tibble() |>
#   dplyr::select(
#     stationID, elevation
#   ) |>
#   dplyr::mutate(aspect = NA_integer_, orientation = NA_integer_) |>
#   dplyr::distinct()
#
# topo_sf_ok <- bar |>
#   dplyr::select(
#     stationID, elevation
#   ) |>
#   dplyr::distinct() |>
#   dplyr::mutate(aspect = NA_integer_, orientation = NA_integer_)
#
# topo_no_stations <- topo_tibble_ok |>
#   dplyr::select(-stationID)
#
# topo_bad_elevation <- topo_tibble_ok |>
#   dplyr::mutate(elevation = as.character(elevation))
#
# meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(topo_no_stations) # error because no stationID
#
# meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(topo_bad_elevation) # error beacuse elevation is not numeric
#
# meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(as.matrix(topo_bad_elevation)) # error because topo is not a tibble
#
# meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(topo_tibble_ok) # ok
#
# meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(topo_sf_ok) # ok for topos with sf, but geometries get replicated
#
# ## interpolator creation ##
interpolator <-
  meteoland:::with_meteo(meteo_ok) |>
  meteoland:::add_topo(topo_tibble_ok) |>
  meteoland:::create_meteo_interpolator()
#
# attr(interpolator, 'params')
