#' Creates a 'SpatialPixelsMeteorology'
#' 
#' Initializes an object of class \code{SpatialPixelsMeteorology-class}
#' 
#' 
#' @param points An object of class \code{\link{SpatialPoints-class}}.
#' @param data A vector of data frames (one per date).
#' @param dates Object of class \code{"Date"} describing the time period of
#' meteorological estimates.
#' @param tolerance Precision up to which extent points should be exactly on a
#' grid.
#' @param proj4string Object of class \code{\linkS4class{CRS}} in the first
#' form only used when points does not inherit from
#' \code{\linkS4class{Spatial}}.
#' @param round default \code{NULL}, otherwise a value passed to as the digits
#' argument to \code{\link{round}} for setting cell size.
#' @param grid Grid topology using an object of class
#' \code{\linkS4class{GridTopology}}; a value of \code{NULL} implies that this
#' will be derived from the point coordinates.
#' @return An object of class \code{\link{SpatialPixelsMeteorology-class}}
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPixelsMeteorology-class}}
