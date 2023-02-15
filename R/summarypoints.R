.summaryvarpoint<-function(x, fun="mean", freq=NULL, dates = NULL, months= NULL, ...) {
  if(is.null(dates)) dates = as.Date(names(x))
  else {
    x = x[as.character(dates)]
  }
  if(!is.null(months)) {
    m = as.numeric(format(dates,"%m"))
    dates = dates[m %in% months]
  }
  if(is.null(freq)) {
    v =  do.call(fun, args=list(x,...))
  } else {
    date.factor = cut(dates, breaks=freq)
    v = tapply(x,INDEX=date.factor, FUN=fun,...)
  }
  return(v)
}

#' @describeIn summarypoints `r lifecycle::badge("deprecated")`
#' @export
summarypoint <- function(x, var, fun="mean", freq=NULL, dates = NULL, months= NULL, ...) {
  
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "summarypoint()", with = "summarise_interpolated_data()",
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Interpolation results are now sf or stars objects and can be summarised with summarise_interpolated_data()"
  )
  
  if(!inherits(x,"data.frame")) stop("'x' has to be a data.frame.")
  VARS = c("MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
           "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
           "Radiation", "WindSpeed", "WindDirection", "PET")
  var = match.arg(var, VARS)
  y = x[[var]]
  names(y) = row.names(x)
  return(.summaryvarpoint(y, fun=fun, freq=freq, dates = dates, months= months,...))
}



#' Summaries of meteorological data
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Summarizes the meteorology of a single location, a set of spatial points,
#' pixels in a grid, or weather stations of interpolation data.
#' 
#' @details
#' If \code{var="ALL"} then function \code{summarypoints} produces a summary of
#' all variables with default statistics and returns an object of class
#' \code{SpatialPointsMeteorology}.
#' 
#' @aliases summarypoint summarypoints summarygrid summarypixels
#' summaryinterpolationdata
#' @param x A data frame with dates in rows and meteorological variables in
#' columns.
#' @param points An object of class
#' \code{\link{SpatialPointsMeteorology-class}} with the coordinates and
#' meteorological data of the locations for which summaries are desired.
#' Alternatively, an object of class \code{\link{SpatialPointsDataFrame-class}}
#' containing the meta data (columns \code{dir}, \code{filename} and possibly
#' \code{format}) of meteorological files that will be sequentially read from
#' the disk. Finally, \code{points} can also be a string pointing to a netCDF.
#' @param var The name of the meteorological variable to be summarized.
#' @param fun The function to be calculated on values of each point. If
#' \code{freq} is specified, the function will be calculated by intervals.
#' @param freq A string giving an interval specification (e.g., \code{"week"},
#' \code{"month"}, \code{"quarter"} or \code{"year"}). If \code{NULL} then no
#' intervals are defined.
#' @param dates An object of class \code{\link{Date}} to define the period to
#' be summarized. If \code{dates = NULL} then all dates in \code{points} are
#' processed.
#' @param months A numeric vector to indicate the subset of months for which
#' summary is desired (e.g. \code{c(7,8)} for July and August). This parameter
#' allows studying particular seasons, when combined with \code{freq}. For
#' example \code{freq = "years"} and \code{months = 6:8} leads to summarizing
#' summer months of each year.
#' @param \dots Additional parameters to \code{fun}.
#' @param grid An object of class \code{\link{SpatialGridMeteorology-class}}
#' with the meteorological data for a grid, or a string pointing to a NetCDF.
#' @param pixels An object of class
#' \code{\link{SpatialPixelsMeteorology-class}} with the meteorological data
#' for grid pixels, or a string pointing to a NetCDF.
#' @param object An object of class
#' \code{\link{MeteorologyInterpolationData-class}}.
#' @return \itemize{ \item{Function \code{summarypoint} returns a named vector
#' of values with dates as names.} \item{Functions \code{summarypoints} and
#' \code{summaryinterpolationdata} return an object of class
#' \code{\link{SpatialPointsDataFrame}} containing summaries (either one
#' variable or several if \code{freq} is specified).} \item{Functions
#' \code{summarygrid} and \code{summarypixels} return an object of class
#' \code{\link{SpatialGridDataFrame}} and \code{\link{SpatialPixelsDataFrame}},
#' respectively, containing the summaries analogously to \code{summarypoints}.}
#' }
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' Antoine Cabon, CTFC
#' @seealso \code{\link{SpatialPointsMeteorology-class}}
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
#' #PET sums by months
#' mp.sum = summarypoints(mp, var="PET", freq="months", fun=sum)
#' 
#' mp.sum
#' }
#' @export
summarypoints<-function(points, var, fun=mean, freq=NULL, dates = NULL, months = NULL, ...) {
  
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "summarypoints()", with = "summarise_interpolated_data()",
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Interpolation results are now sf or stars objects and can be summarised with summarise_interpolated_data()"
  )
  
  if(!inherits(points,"SpatialPointsMeteorology") 
     && !inherits(points,"SpatialPointsDataFrame")
     && !inherits(points,"character")) stop("'points' has to be of class 'SpatialPointsMeteorology', 'SpatialPointsDataFrame' or a character string.")
  VARS = c("MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
           "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
           "Radiation", "WindSpeed", "WindDirection", "PET", "ALL")
  var = match.arg(var, VARS)
  
  if(inherits(points,"SpatialPointsMeteorology") || inherits(points,"SpatialPointsDataFrame")) {

    if(inherits(points,"SpatialPointsMeteorology")) {
      if(!is.null(names(points@data))) ids = names(points@data)
      else ids = 1:npoints
    } else if(inherits(points,"SpatialPointsDataFrame")) {
      if(!is.null(rownames(points@data))) ids = rownames(points@data)
      else ids = 1:npoints
    }
    ptsout = as(points,"SpatialPoints")
  } else {
    file = points
    ncin = .openreadpointsNetCDF(file)
    crs = .readCRSNetCDF(ncin)
    cc = .readpointcoordinatesNetCDF(ncin, crs)
    ids = rownames(cc)
    dates_file = .readdatesNetCDF(ncin)
    varmapping = .defaultMapping()
    ptsout = SpatialPoints(cc, crs)
  }
  
  npoints = length(ids)
  cat(paste("  Summarizing ", var, " in ", npoints," points...\n", sep=""))
  
  dfvec = vector("list",npoints)
  
  pb = txtProgressBar(0, npoints, 0, style = 3)
  for(i in 1:npoints) {
    setTxtProgressBar(pb, i)
    if(inherits(points,"SpatialPointsMeteorology")) {
      obs = points@data[[i]]
    } else if(inherits(points,"SpatialPointsDataFrame")) {
      f = paste(points@data$dir[i], points@data$filename[i],sep="/")
      if(!file.exists(f)) stop(paste("Observed file '", f,"' does not exist!", sep=""))
      if("format" %in% names(points@data)) { ##Format specified
        obs = readmeteorologypoint(f, format=points@data$format[i])
      } else {
        obs = readmeteorologypoint(f)
      }
    } else {
      obs = .readmeteorologypointNetCDF(ncin,i, dates_file, varmapping)
    }
    if(var!="ALL") {
      dfvec[[i]] = summarypoint(x=obs,var=var,fun=fun, freq=freq, dates=dates,months=months,...)
    } else {
      mean_temp = summarypoint(x=obs,var="MeanTemperature",fun="mean", freq=freq, dates=dates,months=months,...)
      min_temp = summarypoint(x=obs,var="MinTemperature",fun="mean", freq=freq, dates=dates,months=months,...)
      max_temp = summarypoint(x=obs,var="MaxTemperature",fun="mean", freq=freq, dates=dates,months=months,...)
      prec = summarypoint(x=obs,var="Precipitation",fun="sum", freq=freq, dates=dates,months=months,...)
      dfvec[[i]] = data.frame(MeanTemperature = mean_temp, 
                              MinTemperature = min_temp, 
                              MaxTemperature = max_temp, 
                              Precipitation = prec, 
                              row.names = names(mean_temp))
      if("MeanRelativeHumidity" %in% names(obs)) {
        dfvec[[i]]$MeanRelativeHumidity = summarypoint(x=obs,var="MeanRelativeHumidity",fun="mean", freq=freq, dates=dates,months=months,...)
      }
      if("MinRelativeHumidity" %in% names(obs)) {
        dfvec[[i]]$MinRelativeHumidity = summarypoint(x=obs,var="MinRelativeHumidity",fun="mean", freq=freq, dates=dates,months=months,...)
      }
      if("MaxRelativeHumidity" %in% names(obs)) {
        dfvec[[i]]$MaxRelativeHumidity = summarypoint(x=obs,var="MaxRelativeHumidity",fun="mean", freq=freq, dates=dates,months=months,...)
      }
      if("Radiation" %in% names(obs)) {
        dfvec[[i]]$Radiation = summarypoint(x=obs,var="Radiation",fun="mean", freq=freq, dates=dates,months=months,...)
      }
      if("WindSpeed" %in% names(obs)) {
        dfvec[[i]]$WindSpeed = summarypoint(x=obs,var="WindSpeed",fun="mean", freq=freq, dates=dates,months=months,...)
      }
      if("PET" %in% names(obs)) {
        dfvec[[i]]$PET = summarypoint(x=obs,var="PET",fun="sum", freq=freq, dates=dates,months=months,...)
      }
    }
  }
  cat("\n")
  if(inherits(points,"character")) .closeNetCDF(file,ncin)
    
  if(var!="ALL") {
    noutvars = length(dfvec[[1]])
    dfout = data.frame(matrix(NA,nrow=npoints, ncol=noutvars))
    rownames(dfout) = ids
    outvarnames = names(dfvec[[1]])
    if(!is.null(outvarnames)) names(dfout) = outvarnames
    cat(paste("  Arranging output...\n", sep=""))
    pb = txtProgressBar(0, npoints, 0, style = 3)
    for(i in 1:npoints) {
      setTxtProgressBar(pb, i)
      dfout[i,] = as.numeric(dfvec[[i]])
    }
    return(SpatialPointsDataFrame(ptsout,dfout))
  } else {
    datesout = as.Date(row.names(dfvec[[1]]))
    return(SpatialPointsMeteorology(ptsout, dfvec, datesout))
  }
}
