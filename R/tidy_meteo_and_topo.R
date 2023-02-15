#' Ensure meteo object is ready to create an interpolator object
#'
#' Check integrity of meteo objects
#'
#' This function is the first step in the creation of a meteoland interpolator,
#' ensuring the meteo provided contains all the required elements
#'
#' @param meteo meteo object
#' @param verbose Logical indicating if the function must show messages and info.
#' Default value checks \code{"meteoland_verbosity"} option and if not set, defaults
#' to TRUE. It can be turned off for the function with FALSE, or session wide with
#' \code{options(meteoland_verbosity = FALSE)}
#' @return invisible meteo object ready to pipe in the interpolator creation
#' @seealso Other interpolator functions: \code{\link{add_topo}()},
#' \code{\link{create_meteo_interpolator}()},
#' \code{\link{get_interpolation_params}()}, \code{\link{read_interpolator}()},
#' \code{\link{set_interpolation_params}()}, \code{\link{write_interpolator}()}
#'
#' @examples
#' # example meteo
#' data(meteoland_meteo_example)
#' with_meteo(meteoland_meteo_example)
#'
#' @export with_meteo
with_meteo <- function(meteo, verbose = getOption("meteoland_verbosity", TRUE)) {
  .verbosity_control(
    cli::cli_alert_info("Checking meteorology object..."),
    verbose
  )
  assertthat::assert_that(has_meteo(meteo))
  .verbosity_control(
    cli::cli_alert_success("meteorology object ok"),
    verbose
  )
  return(invisible(meteo))
}

#' Add topography data to meteo object
#'
#' Add topography data to meteo object
#'
#' When using meteo data without topography info to create an interpolator,
#' topography must be added
#'
#' @param meteo meteo object
#' @param topo topo object
#' @param verbose Logical indicating if the function must show messages and info.
#' Default value checks \code{"meteoland_verbosity"} option and if not set, defaults
#' to TRUE. It can be turned off for the function with FALSE, or session wide with
#' \code{options(meteoland_verbosity = FALSE)}
#' @return meteo with the topography info added
#' @seealso Other interpolator functions:
#' \code{\link{create_meteo_interpolator}()},
#' \code{\link{get_interpolation_params}()}, \code{\link{read_interpolator}()},
#' \code{\link{set_interpolation_params}()}, \code{\link{with_meteo}()},
#' \code{\link{write_interpolator}()}
#'
#' @examples
#' # example meteo
#' data(meteoland_meteo_no_topo_example)
#' # example topo
#' data(meteoland_topo_example)
#' # add topo
#' with_meteo(meteoland_meteo_no_topo_example) |>
#'   add_topo(meteoland_topo_example)
#'
#' @export add_topo
add_topo <- function(meteo, topo, verbose = getOption("meteoland_verbosity", TRUE)) {

  assertthat::assert_that(has_meteo(meteo))
  .verbosity_control(
    cli::cli_alert_info("Checking topography object..."),
    verbose
  )
  assertthat::assert_that(has_topo(topo))
  .verbosity_control(
    cli::cli_alert_success("topography object ok"),
    verbose
  )

  .verbosity_control(
    cli::cli_alert_info("Adding topography to meteo (by station ID)..."),
    verbose
  )

  # check if meteo has topo already
  if (any(c("elevation", "aspect", "slope") %in% names(meteo))) {
    cli::cli_warn(c(
      "Topography variables found in the meteo object.",
      "They will be ignored as a new topography is provided."
    ))
    meteo <- meteo |>
      dplyr::select(!dplyr::any_of(c("elevation", "aspect", "slope")))
  }

  res <- dplyr::left_join(
    meteo,
    # ensure topo is a tibble, with unique rows
    topo |>
      dplyr::as_tibble() |>
      dplyr::select(
        dplyr::any_of(c("stationID", "elevation", "aspect", "slope"))
      ) |>
      dplyr::distinct(),
    by = 'stationID'
  ) |>
    sf::st_as_sf()

  .verbosity_control(
    cli::cli_alert_success("Topography added"),
    verbose
  )

  return(res)
}
