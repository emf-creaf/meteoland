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
#' \code{\link{read_interpolator}()}, \code{\link{with_meteo}()},
#' \code{\link{write_interpolator}()}