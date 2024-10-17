#' Check if meteo data has the mandatory variables
#'
#' Check if meteo data has the mandatory variables
#'
#' This function ensures that meteo object have the mandatory variables, and is
#' used in combination with \code{\link[assertthat]{on_failure}} to produce a
#' meaningful and human readable error when no mandatory variables are present.
#'
#' @param meteo meteo object
#'
#' @return TRUE if mandatory variables are present, FALSE otherwise
#'
#' @family Argument checks
#' @noRd
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

#' Check if meteo data has the mandatory topography variables
#'
#' Check if meteo data has the mandatory topography variables
#'
#' This function ensures that meteo object have the mandatory topography names,
#' and is used in combination with \code{\link[assertthat]{on_failure}} to produce
#' a meaningful and human readable error when no mandatory variables are present.
#'
#' @param meteo meteo object
#'
#' @return TRUE if mandatory variables are present, FALSE otherwise
#'
#' @family Argument checks
#' @noRd
has_topo_names <- function(meteo) {
  # topography names
  mandatory_topo_names <- c("elevation")
  # check
  all(mandatory_topo_names %in% names(meteo))
}

assertthat::on_failure(has_topo_names) <- function(call, env) {
  paste0(
    "Names found in ", deparse(call$meteo), " don't comply with the required names:\n",
    deparse(call$meteo),
    " should have the following topography variables:\n",
    "  - elevation ***\n",
    "  - aspect\n", "  - slope\n",
    "\n ***: mandatory variables\n"
  )
}

#' Check if meteo data has unique stations ID
#'
#' Check if meteo data has unique stations ID
#'
#' This function ensures that meteo object have unique stations ID with only
#' one geometry per station code
#'
#' @param meteo meteo object
#'
#' @return TRUE if station IDs are unique, FALSE otherwise
#'
#' @family Argument checks
#' @noRd
has_unique_ids <- function(meteo) {
  distinct_rows <- meteo |>
    dplyr::as_tibble() |>
    dplyr::select("stationID", attr(meteo, "sf_column")) |>
    dplyr::distinct()

  nrow(distinct_rows) == length(unique(distinct_rows[["stationID"]]))

}

assertthat::on_failure(has_unique_ids) <- function(call, env) {

  duplicated_stations <- env$meteo |>
    dplyr::select("stationID") |>
    dplyr::distinct() |>
    dplyr::filter(duplicated(.data$stationID)) |>
    dplyr::pull("stationID")

  paste0(
    "There are more geometries in the data than unique station IDs. ",
    "Duplicated stations IDs are:\n",
    paste(duplicated_stations, collapse = '\n')
  )
}


#' Checks for meteo
#'
#' Checks for meteo
#'
#' This function checks that meteo object has everything is needed to create the
#' interpolator. Checks done include being a sf POINT object, correct variables,
#' numeric variables, dates and meteo stations names.
#'
#' @param meteo meteo object
#'
#' @family Argument checks
#'
#' @return Informative error when any check fails, invisible TRUE otherwise
#' @noRd
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

  # stations
  assertthat::assert_that(
    "stationID" %in% names(meteo),
    msg = "meteo must have a stationID variable identifying the meteorological stations"
  )

  assertthat::assert_that(
    has_unique_ids(meteo)
  )
}

#' Checks for topo
#'
#' Checks for topo
#'
#' This function checks that topo object has everything is needed to create the
#' interpolator. Checks done include being a dataframe, correct variables,
#' numeric variables, and stations names.
#'
#' @param topo topo object
#'
#' @family Argument checks
#'
#' @return Informative error when any check fails, invisible TRUE otherwise
#' @noRd
has_topo <- function(topo) {

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

#' Check class of provided spatial data
#'
#' Check class of provided spatial data
#'
#' This function checks the class of the provided spatial data is in the allowed
#' classes (stars and sf)
#'
#' @param spatial_data spatial data provided
#'
#' @return TRUE if spatial data is a stars or sf object, FALSE otherwise
#'
#' @noRd
.is_spatial_data <- function(spatial_data) {
  inherits(spatial_data, c('sf', 'stars'))
}


assertthat::on_failure(.is_spatial_data) <- function(call, env) {
  paste0(
    "Spatial data provided (", deparse(call$spatial_data), ")\n",
    "must be an sf object (for points) or an stars object (for raster and grids).\n",
    "Other spatial classes (from sp, terra... packages) must be converted first."
  )
}

#' Check if spatial data stars is a raster
#'
#' Check if spatial data stars is a raster
#'
#' This function checks if the provided stars object is a raster, not a vector
#' data cube (which is not allowed)
#'
#' @param spatial_data stars spatial data provided
#'
#' @return TRUE when spatial data is a raster, FALSE when is a vector data cube
#'
#' @noRd
.is_raster <- function(spatial_data) {
  spatial_dimension <-
    purrr::map_lgl(sf::st_coordinates(spatial_data), inherits, what = 'sfc') |>
    any()
  !spatial_dimension
}

assertthat::on_failure(.is_raster) <- function(call, env) {
  paste0(
    "stars object provided (", deparse(call$spatial_data), ")\n",
    "is a vector data cube. Please convert to sf points data or raster data\n",
    "before the interpolatation."
  )
}

#' Assertions to check if the object provided is an interpolator
#'
#' Assertions to check if the object provided is an interpolator
#'
#' This function checks all the mandatory characteristics of an interpolator
#' object (dimensions, parameters, variables).
#'
#' @param interpolator meteoland interpolator object, as created by
#'   \code{\link{create_meteo_interpolator}}
#'
#' @return invisible TRUE if the object is a meteoland complying interpolator,
#'   an informative error otherwise
#'
#' @noRd
.is_interpolator <- function(interpolator) {
  has_params <- !is.null(get_interpolation_params(interpolator))
  has_correct_dimensions <-
    all(c('date', 'station') %in% names(stars::st_dimensions(interpolator)))

  assertthat::assert_that(
    has_params,
    msg = "interpolator object is missing the interpolation parameters attribute."
  )
  assertthat::assert_that(
    has_correct_dimensions,
    msg = "interpolator object is missing the correct dimensions (date and station)"
  )
  assertthat::assert_that(has_meteo_names(interpolator))
  assertthat::assert_that(has_topo_names(interpolator))
}
