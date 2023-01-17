#' Write the interpolator object
#' 
#' Write the interpolator object to a file
#' 
#' This function writes the interpolator object created with
#' \code{\link{create_meteo_interpolator}} in a NetCDF-CF standard
#' compliant format, as specified in
#' http://cfconventions.org/cf-conventions/cf-conventions.html
#' 
#' @param interpolator meteoland interpolator object, as created by
#' \code{\link{create_meteo_interpolator}}
#' @param filename file name for the interpolator nc file
#' @param .overwrite logical indicating if the file should be overwrited if it
#' already exists
#' @return invisible interpolator object, to allow using this function as a
#' step in a pipe
#' @seealso Other interpolator functions: \code{\link{add_topo}()},
#' \code{\link{create_meteo_interpolator}()},
#' \code{\link{read_interpolator}()}, \code{\link{with_meteo}()}
