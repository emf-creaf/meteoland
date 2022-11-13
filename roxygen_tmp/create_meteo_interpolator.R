#' Meteoland interpolator creation
#' 
#' Function to create the meteoland interpolator
#' 
#' This function takes meteorology information and a list of interpolation
#' parameters and creates the interpolator object to be ready to use.
#' 
#' @param meteo_with_topo meteo object, as returned by \code{\link{with_meteo}}
#' @return an interpolator object (stars)
#' @seealso Other interpolator functions: \code{\link{add_topo}()},
#' \code{\link{read_interpolator}()}, \code{\link{with_meteo}()},
#' \code{\link{write_interpolator}()}