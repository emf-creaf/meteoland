#' Averages area weather
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Averages the weather data series of points or grid pixels.
#'
#' @details
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
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{weathergeneration}}
#'
#' @export
averagearea<-function(object, na.rm=TRUE) {

  # deprecation warning
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "averagearea()", with = NULL,
    details = "Spatial_*_Meteorology classes are deprecated. To average meteo data series is better to use the corresponding functions in the sf package"
  )

  # Calculate mean area series
  # if((!inherits(object,"SpatialPointsMeteorology"))
  #    && (!inherits(object,"SpatialGridMeteorology"))
  #    && (!inherits(object,"SpatialPixelsMeteorology"))) stop("'object' has to be of class 'Spatial_*_Meteorology'.")
  # #Average coordinates
  # cc = matrix(apply(coordinates(object), 2, mean), nrow=1, ncol=2)
  # dates = object@dates
  # df = data.frame(row.names=as.character(dates))
  # cn = names(object@data[[1]])
  # if(!("DOY" %in% cn)) cn = c("DOY", cn)
  # df[cn] = NA
  # if(inherits(object,"SpatialPointsMeteorology")) {
  #   for(i in 1:length(dates)) {
  #     m = matrix(NA, nrow=length(object@data), ncol = length(cn))
  #     for(j in 1:length(object@data)) {
  #       m[j,] = as.numeric(object@data[[j]][i,])
  #     }
  #     df[i,] = apply(m, 2, mean, na.rm=na.rm)
  #   }
  # } else {
  #   for(i in 1:length(dates)) {
  #     df[i,-1] = apply(object@data[[i]], 2, mean, na.rm=na.rm)
  #   }
  #   df$DOY = as.numeric(format(dates,"%j"))
  # }
  # data = list(df)
  # return(SpatialPointsMeteorology(SpatialPoints(cc, proj4string = object@proj4string),data = data, dates))
}
