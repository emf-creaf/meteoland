#' Averages area weather
#' 
#' Averages the weather data series of points or grid pixels.
#' 
#' Assumes that all points/pixels represent the same area.
#' 
#' @param object An object of class
#' \code{\link{SpatialPointsMeteorology-class}},
#' \code{\link{SpatialGridMeteorology-class}} or
#' \code{\link{SpatialPixelsMeteorology-class}}.
#' @param na.rm Boolean flag to indicate that missing values should be excluded
#' from averages.
#' @return An object of class as the input
#' \code{SpatialPointsMeteorology-class} with weather series corresponding to
#' the spatial average of the input.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{weathergeneration}}
#' @examples
#' 
#' data(examplegridtopography)
#' data(exampleinterpolationdata)
#' 
#' #Interpolation of meteorology over a grid for two days
#' ml = interpolationgrid(exampleinterpolationdata, examplegridtopography,
#'                        as.Date(c("2001-02-03", "2001-06-03")))
#' 
#' #Call averaging function
#' pa = averagearea(ml)
#' 
#' #Spatial information
#' pa
#' 
#' #Weather data
#' pa@data[[1]]
#' 