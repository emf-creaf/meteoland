.station_indexes_converter <- function(stations, interpolator) {

  res <- stations

  if (is.null(stations)) {
    res <- 1:length(stars::st_get_dimension_values(interpolator, "station"))
  }

  if (is.character(stations)) {
    res <- which(colnames(interpolator[[1]]) %in% stations)
    if (length(res) < 1) {
      cli::cli_abort("Station names not found in interpolator")
    }
  }

  return(res)
}

#' Precipitation rainfall erosivity
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Function \code{precipitation_rainfall_erosivity()} calculates a multi-year
#' average of monthly rainfall erosivity using the MedREM model proposed by
#' Diodato and Bellochi (2010) for the Mediterranean area (see also Guerra et
#' al. 2016).
#'
#' @details
#' MedREM model is: Rm = b0·P·sqrt(d)·(alpha + b1*longitude), where P is
#' accumulated precipitation and d is maximum daily precipitation. Parameters
#' used for the MedREM model are b0 = 0.117, b1 = -0.015, alpha = 2. Note that
#' there is a mistake in Guerra et al. (2016) regarding parameters b1 and a.
#'
#' @param meteo_data A meteo tibble as with the dates and meteorological variables
#'   as returned by \code{\link{interpolate_data}} in the "interpolated_data"
#'   column.
#' @param longitude Longitude in degrees.
#' @param scale Character, either 'month' or 'year'. Default to 'month'
#' @param average Boolean flag to calculate multi-year averages before applying
#'   MedREM's formula.
#'
#' @return A vector of values for each month (in MJ·mm·ha-1·h-1·month-1) or each
#'   year (in MJ·mm·ha-1·h-1·yr-1), depending on the scale
#'
#' @author
#'   Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#'
#'   \enc{Víctor}{Victor} Granda \enc{García}{Garcia}, CREAF.
#'
#' @references
#' Diodato, N., Bellocchi, G., 2010. MedREM, a rainfall erosivity
#' model for the Mediterranean region. J. Hydrol. 387, 119–127,
#' doi:10.1016/j.jhydrol.2010.04.003.
#'
#' Guerra CA, Maes J, Geijzendorffer I, Metzger MJ (2016) An assessment of soil
#' erosion prevention by vegetation in Mediterranean Europe: Current trends of
#' ecosystem service provision. Ecol Indic 60:213–222. doi:
#' 10.1016/j.ecolind.2015.06.043.
#'
#' @examples
#' \donttest{
#' interpolated_example <-
#'   interpolate_data(points_to_interpolate_example, meteoland_interpolator_example)
#'
#' precipitation_rainfall_erosivity(
#'   meteo_data = interpolated_example$interpolated_data[[1]],
#'   longitude = 2.32,
#'   scale = "month",
#'   average = TRUE
#' )
#' }
#'
#' @export
precipitation_rainfall_erosivity <- function(
  meteo_data,
  longitude,
  scale = c("month", "year"),
  average = TRUE
) {

  # recursive call to be able to call precipitation_rainfall_erosivity in mutates or
  # with lists of meteo_data and longitudes
  if ((!is.data.frame(meteo_data) & is.list(meteo_data)) && length(meteo_data) == length(longitude)) {
    res <- purrr::map2(
      .x = meteo_data, .y = longitude,
      .f = precipitation_rainfall_erosivity,
      scale = scale, average = average
    )

    return(res)
  }

  # assertions
  assertthat::assert_that(
    assertthat::is.number(longitude), msg = "longitude must be numeric"
  )
  # assertthat::assert_that(
  #   assertthat::is.string(scale), length(scale) == 1,
  #   msg = "scale must be a string of length 1"
  # )
  assertthat::assert_that(
    assertthat::is.flag(average), msg = "average must be a logical value"
  )

  # match.arg
  scale <- match.arg(scale)

  # constants
  b0 <- 0.117
  b1 <- -0.015
  a <- 2.000

  precipitation_sum <- summarise_interpolated_data(
    meteo_data, fun = 'sum', frequency = scale,
    vars_to_summary = "Precipitation", na.rm = TRUE
  )

  precipitation_max <- summarise_interpolated_data(
    meteo_data, fun = 'max', frequency = scale,
    vars_to_summary = "Precipitation", na.rm = TRUE
  )

  if (isTRUE(average)) {
    ## TODO there is a problem in multiyear datasets and yearly scale, because in theory
    ## in those situations all years must be summarised together, but that's not
    ## happening with the actual code

    # average by scale.
    # This depends on the scale and is done for compatibility with older versions of meteoland
    # If monthly, group by month and average.
    if (scale == 'month') {
      precipitation_sum <- dplyr::group_by(precipitation_sum, .data[[scale]]) |>
        dplyr::summarise(p_s = mean(.data$Precipitation, na.rm = TRUE))
      precipitation_max <- dplyr::group_by(precipitation_max, .data[[scale]]) |>
        dplyr::summarise(d_s = mean(.data$Precipitation, na.rm = TRUE))
    } else {
      # If yearly, average without grouping
      precipitation_sum <- precipitation_sum |>
        dplyr::summarise(p_s = mean(.data$Precipitation, na.rm = TRUE))
      precipitation_max <- precipitation_max |>
        dplyr::summarise(d_s = mean(.data$Precipitation, na.rm = TRUE))
    }

  } else {
    # no average, just rename the Precipitation var for the next steps
    precipitation_sum <- dplyr::rename(precipitation_sum, p_s = "Precipitation")
    precipitation_max <- dplyr::rename(precipitation_max, d_s = "Precipitation")
  }

  p_s <- precipitation_sum |>
    dplyr::pull(.data$p_s) |>
    purrr::set_names(unique(precipitation_sum[[scale]]))

  d_s <- precipitation_max |>
    dplyr::pull(.data$d_s) |>
    purrr::set_names(unique(precipitation_max[[scale]]))

  # build the result with the formula and return it
  res <- b0 * p_s * sqrt(d_s) * (a + b1 * longitude)
  return(res)
}

#' Helper to check for nullness or specific character vector
#' @noRd
is_null_or_variable <- function(object, variable_names) {
  is.null(object) || any(variable_names %in% object)
}

#' Helper to check for nullness and return a numeric NA if it is
#' @noRd
null2na <- function(object) {
  if (is.null(object)) {
    return(NA_real_)
  }
  return(object)
}

#' Verbosity control
#'
#' Verbose check for meteoland messages
#'
#' Function print to console the usethis expression if the verbose control is TRUE. Always returns
#' \code{invisible()}
#'
#' @param expr usethis expression to print if verbose is TRUE
#' @param verbose verbosity user control
#'
#' @return invisible()
#' @noRd
.verbosity_control <- function(expr, verbose) {
  if (verbose) {
    expr
  }
  return(invisible())
}

.get_geometry_name <- function(sf_object) {
  return(attr(sf_object, "sf_column"))
}
