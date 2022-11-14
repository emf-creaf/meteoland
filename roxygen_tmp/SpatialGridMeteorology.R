#' Creates a 'SpatialGridMeteorology'
#' 
#' Initializes an object of class \code{SpatialGridMeteorology-class}
#' 
#' 
#' @param grid An object of class \code{\link{GridTopology-class}}
#' @param proj4string Object of class \code{"CRS"} with the projection string.
#' @param data A vector of data frames (one per date).
#' @param dates Object of class \code{"Date"} describing the time period of
#' meteorological estimates.
#' @return An object of class \code{\link{SpatialGridMeteorology-class}}
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialGridMeteorology-class}}
