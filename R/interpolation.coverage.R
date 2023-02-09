#' Spatial and temporal coverage of interpolation data
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
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
#' @export
interpolation.coverage<-function(object, type="spatial", percent = FALSE) {
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "interpolation.coverage()", with = "create_meteo_interpolator()",
    details = "MeteorologyInterpolationData class is soft deprecated.
    A summary of the variables can be glanced calling the interpolator object"
  )

  if(!inherits(object, "MeteorologyInterpolationData")) stop("'object' has to be of class 'MeteorologyInterpolationData'")
  type = match.arg(type, c("spatial","temporal"))
  if(type=="spatial") {
    cov_df = data.frame(MinTemperature=rowSums(!is.na(object@MinTemperature)),
                      MaxTemperature=rowSums(!is.na(object@MaxTemperature)),
                      Precipitation=rowSums(!is.na(object@Precipitation)),
                      RelativeHumidity=rowSums(!is.na(object@RelativeHumidity)),
                      Radiation=rowSums(!is.na(object@Radiation)),
                      WindSpeed=rowSums(!is.na(object@WindSpeed)),
                      WindDirection=rowSums(!is.na(object@WindDirection)))
    if(percent) cov_df = 100*cov_df/length(object@dates)
    spdf =  SpatialPointsDataFrame(object@coords, data = cov_df, proj4string = object@proj4string)
    return(spdf)
  } else if(type=="temporal") {
    cov_df = data.frame(MinTemperature=colSums(!is.na(object@MinTemperature)),
                        MaxTemperature=colSums(!is.na(object@MaxTemperature)),
                        Precipitation=colSums(!is.na(object@Precipitation)),
                        RelativeHumidity=colSums(!is.na(object@RelativeHumidity)),
                        Radiation=colSums(!is.na(object@Radiation)),
                        WindSpeed=colSums(!is.na(object@WindSpeed)),
                        WindDirection=colSums(!is.na(object@WindDirection)))
    if(percent) cov_df = 100*cov_df/nrow(object@coords)
    row.names(cov_df) = as.character(object@dates)
    return(cov_df)
  }
}
