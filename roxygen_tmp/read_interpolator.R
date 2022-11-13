#' Read interpolator files
#' 
#' Read interpolator files created with \code{\link{write_interpolator}}
#' 
#' This function takes the file name of the nc file storing an interpolator
#' object and load it into the work environment
#' 
#' @param filename interpolator file name
#' @return an interpolator (stars) object
#' @seealso Other interpolator functions: \code{\link{add_topo}()},
#' \code{\link{create_meteo_interpolator}()}, \code{\link{with_meteo}()},
#' \code{\link{write_interpolator}()}