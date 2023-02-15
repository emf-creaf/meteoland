#' Plots point meteorological series
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Simple plotting of a meteorological series for a given point.
#' 
#' @details
#' Daily precipitation is plotted using bars (i.e. \code{type = "h"} when
#' calling \code{\link{plot}}). Otherwise the function draws lines (i.e.
#' \code{type = "l"} when calling \code{\link{plot}}). If \code{object} is of
#' class \code{\link{SpatialPointsDataFrame-class}} then the function reads the
#' meteorological data to be plotted from the disk.
#' 
#' @param object A data frame with daily meteorological data (in this case
#' \code{index} is not used) or an object of class
#' \code{\linkS4class{SpatialPointsMeteorology}}. Alternatively, an object of
#' class \code{\linkS4class{SpatialPointsDataFrame}} containing the meta data
#' (columns \code{dir}, \code{filename} and possibly \code{format}) of
#' meteorological files.
#' @param index An integer to indicate the point in the
#' \code{\linkS4class{SpatialPointsMeteorology}} object (or the
#' \code{\linkS4class{SpatialPointsDataFrame}} object).
#' @param var The meteorological variable to be plotted.
#' @param fun The name of a function to be calculated for summaries (only valid
#' if \code{freq} is specified).
#' @param freq A string giving an interval specification for summaries (e.g.,
#' \code{"week"}, \code{"month"}, \code{"quarter"} or \code{"year"}).
#' @param dates An object of class \code{\link{Date}} to define the period to
#' be plotted. If \code{dates = NULL} then all dates in \code{object} are
#' processed.
#' @param months A numeric vector to indicate the subset of months for which
#' plotting is desired (e.g. \code{c(7,8)} for July and August). When combined
#' with \code{fun} and \code{freq}, this parameter allows plotting summaries
#' for particular seasons. For example \code{fun = "sum"} \code{freq = "years"}
#' and \code{months = 6:8} leads to plotting the sum over summer months of each
#' year.
#' @param add A flag to indicate wether drawing should be done on the current
#' plot (using function \code{lines}).
#' @param ... Additional parameters for functions \code{plot} or \code{lines}.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{summarypoints}}
#' @examples
#' 
#' \donttest{
#' data(examplegridtopography)
#' data(exampleinterpolationdata)
#' 
#' #Creates spatial topography points from the grid
#' p = 1:2
#' spt = as(examplegridtopography, "SpatialPointsTopography")[p]
#' 
#' #Interpolation of two points for the whole time period (2000-2003)
#' mp = interpolationpoints(exampleinterpolationdata, spt)
#' 
#' #Plot interpolated meteorological series
#' meteoplot(mp,1, ylab="Daily mean temperature")
#' 
#' meteoplot(mp,1, ylab="Monthly mean temperature", fun=mean, freq="months")
#' }
#' 
#' @export
meteoplot<-function(object, index=1, var="MeanTemperature",
                    fun=NULL, freq=NULL, dates = NULL, months = NULL,
                    add = FALSE, ...){

  # deprecation warning
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "meteoplot()", with = NULL,
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Meteo objects are now sf objects and can be plotted as any other data.frame"
  )

  if(!inherits(object,"data.frame") && !inherits(object,"SpatialPointsMeteorology") && !inherits(object,"SpatialPointsDataFrame"))
    stop("'object' should be of class 'data.frame', SpatialPointsMeteorology' or 'SpatialPointsDataFrame'.")

  VARS = c("MeanTemperature", "MinTemperature","MaxTemperature","MaxMinTemperatureDiff","MaxMeanTemperatureDiff", "MinMeanTemperatureDiff","Precipitation",
           "SpecificHumidity", "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
           "Radiation", "WindSpeed", "WindDirection", "PET")
  var = match.arg(var, VARS)

  if(inherits(object,"SpatialPointsDataFrame")) {
    f = paste(object@data$dir[index], object@data$filename[index], sep="/")
    cat(paste("Data read from file:", f, "\n"))
    if("format" %in% names(object@data)) { ##Format specified
      df = readmeteorologypoint(f, format=object@data$format[index])
    } else {
      df = readmeteorologypoint(f)
    }
    if(is.null(dates)) dates = as.Date(rownames(df))
  } else if(inherits(object, "data.frame")) {
    if(is.null(dates)) dates = as.Date(row.names(object))
  } else {
    if(is.null(dates)) dates = object@dates
  }
  if(!is.null(months)) {
    m = as.numeric(format(dates,"%m"))
    dates = dates[m %in% months]
  }

  if(inherits(object,"data.frame")) {
    if(!(var %in% c("MaxMinTemperatureDiff","MaxMeanTemperatureDiff", "MinMeanTemperatureDiff"))) {
      vec = object[as.character(dates),var]
    } else if(var=="MaxMinTemperatureDiff") {
      vec = object[as.character(dates),"MaxTemperature"] - object[as.character(dates),"MinTemperature"]
    } else if(var=="MaxMeanTemperatureDiff") {
      vec = object[as.character(dates),"MaxTemperature"] - object[as.character(dates),"MeanTemperature"]
    } else if(var=="MinMeanTemperatureDiff") {
      vec = object[as.character(dates),"MinTemperature"] - object[as.character(dates),"MeanTemperature"]
    }
  } else if(inherits(object,"SpatialPointsMeteorology")) {
    if(!(var %in% c("MaxMinTemperatureDiff","MaxMeanTemperatureDiff", "MinMeanTemperatureDiff"))) {
      vec = object@data[[index]][as.character(dates),var]
    } else if(var=="MaxMinTemperatureDiff") {
      vec = object@data[[index]][as.character(dates),"MaxTemperature"] - object@data[[index]][as.character(dates),"MinTemperature"]
    } else if(var=="MaxMeanTemperatureDiff") {
      vec = object@data[[index]][as.character(dates),"MaxTemperature"] - object@data[[index]][as.character(dates),"MeanTemperature"]
    } else if(var=="MinMeanTemperatureDiff") {
      vec = object@data[[index]][as.character(dates),"MinTemperature"] - object@data[[index]][as.character(dates),"MeanTemperature"]
    }
  } else if(inherits(object, "SpatialPointsDataFrame")) {
    if(!(var %in% c("MaxMinTemperatureDiff","MaxMeanTemperatureDiff", "MinMeanTemperatureDiff"))) {
      vec = df[as.character(dates),var]
    } else if(var=="MaxMinTemperatureDiff") {
      vec = df[as.character(dates),"MaxTemperature"] - df[as.character(dates),"MinTemperature"]
    } else if(var=="MaxMeanTemperatureDiff") {
      vec = df[as.character(dates),"MaxTemperature"] - df[as.character(dates),"MeanTemperature"]
    } else if(var=="MinMeanTemperatureDiff") {
      vec = df[as.character(dates),"MinTemperature"] - df[as.character(dates),"MeanTemperature"]
    }
  }
  if((!is.null(fun)) && (!is.null(freq))) {
    date.factor = cut(dates, breaks=freq)
    vec = tapply(vec,INDEX=date.factor, FUN=fun, na.rm=TRUE)
    dates = as.Date(names(vec))
  }
  if(add) {
    lines(dates,vec, ...)
  } else {
    if((var=="Precipitation") && (is.null(fun))) plot(dates,vec, type="h",...)
    else plot(dates,vec, type="l",...)
  }
}
