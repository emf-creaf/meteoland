#' Spatial and temporal coverage of interpolation data
#' 
#' Function \code{interpolation.coverage} calculates, for each meteorological
#' variable, the number of stations with data per date or the number of dates
#' with data per station in an object of class
#' \code{\link{MeteorologyInterpolationData-class}}.
#' 
#' 
#' @param object An object of class
#' \code{\link{MeteorologyInterpolationData-class}}.
#' @param type A string with the coverage summary to be produced (either
#' "spatial" or "temporal").
#' @param percent A boolean flag to indicate that percentages should be
#' returned instead of counts.
#' @return If \code{type = "spatial"} the function returns an object of class
#' \code{SpatialPointsDataFrame} with the number (or percentage) of dates with
#' data per station and meteorological variable. If \code{type = "temporal"}
#' the function returns an object of class \code{data.frame} with the number
#' (or percentage) of stations with data per day and meteorological variable.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{MeteorologyInterpolationData}}
#' @references De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018)
#' Estimating daily meteorological data and downscaling climate models over
#' landscapes. Environmental Modelling and Software 108: 186-196.
#' @examples
#' 
#' data(exampleinterpolationdata)
#' 
#' #Number of days with data per station
#' head(interpolation.coverage(exampleinterpolationdata))
#' 
#' #Number of stations with data per day
#' head(interpolation.coverage(exampleinterpolationdata, type = "temporal"))
#' 
