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
#' \code{\link{read_interpolator}()}, \code{\link{write_interpolator}()}