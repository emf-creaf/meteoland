#' Ensure meteo object is ready to create an interpolator object
#'
#' Check integrity of meteo objects
#'
#' This function is the first step in the creation of a meteoland interpolator,
#' ensuring the meteo provided contains all the required elements
#'
#' @param meteo meteo object
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
with_meteo <- function(meteo) {
  usethis::ui_info("Checking meteorology object...")
  assertthat::assert_that(has_meteo(meteo))
  usethis::ui_done("meteorology object ok")
  return(invisible(meteo))
}

#' Add topology data to meteo object
#'
#' Add topology data to meteo object
#'
#' When using meteo data without topology info to create an inteprolator,
#' topology must be addded
#'
#' @param meteo meteo object
#' @param topo topo object
#' @return meteo with the topology info added
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
add_topo <- function(meteo, topo) {

  assertthat::assert_that(has_meteo(meteo))
  usethis::ui_info("Checking topology object...")
  assertthat::assert_that(has_topo(topo))
  usethis::ui_done("topology object ok")

  usethis::ui_info("Adding topology to meteo (by station ID)...")

  # check if meteo has topo already
  if (any(c("elevation", "aspect", "slope") %in% names(meteo))) {
    usethis::ui_warn(
      "Topology variables found in the meteo object.\nThey will be ignored as a new topology is provided."
    )
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
  # res <- dplyr::left_join(meteo, dplyr::as_tibble(topo), by = 'stationID')
  usethis::ui_done("topology added")

  return(res)
}