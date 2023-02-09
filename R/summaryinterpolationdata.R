#' @describeIn summarypoints `r lifecycle::badge("deprecated")`
#' @export
summaryinterpolationdata<-function(object, var, fun=mean, freq=NULL, dates = NULL, months = NULL, ...) {
  
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "summaryinterpolationdata()", with = "summarise_interpolator()",
    details = "MeteorologyInterpolationData class is soft deprecated.
    Interpolator objects are now stars data cube objects, and can be summarised with summarise_interpolator()"
  )
  
  if(!inherits(object,"MeteorologyInterpolationData")) stop("'object' has to be of class 'MeteorologyInterpolationData'.")
  VARS = c("MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
           "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
           "Radiation", "WindSpeed", "WindDirection", "PET")
  var = match.arg(var, VARS)
  
  codes = rownames(object@coords)
  if(is.null(codes)) codes = row.names(object@Precipitation)
  if(is.null(codes)) codes = 1:nrow(object@coords)
  points = SpatialPoints(object@coords, proj4string = object@proj4string, bbox = object@bbox)
  npoints = length(codes)
  
  cat(paste("  Summarizing ", var, " in ", npoints," weather stations...\n", sep=""))
  
  if(is.null(dates)) dates = object@dates
  
  obsdata = slot(object, var) # Get variable data
  dfvec = vector("list",npoints)
  pb = txtProgressBar(0, npoints, 0, style = 3)
  for(i in 1:npoints) {
    setTxtProgressBar(pb, i)
    if(!is.null(months)) {
      m = as.numeric(format(dates,"%m"))
      dates = dates[m %in% months]
    }
    if(is.null(freq)) {
      dfvec[[i]] =  do.call(fun, args=list(obsdata[i,as.character(dates)],...))
    } else {
      date.factor = cut(dates, breaks=freq)
      dfvec[[i]] = tapply(obsdata[i,as.character(dates)],INDEX=date.factor, FUN=fun,...)
    }
  }
  cat("\n")
  noutvars = length(dfvec[[1]])
  dfout = data.frame(matrix(NA,nrow=npoints, ncol=noutvars))
  rownames(dfout) = codes
  outvarnames = names(dfvec[[1]])
  if(!is.null(outvarnames)) names(dfout) = outvarnames
  cat(paste("  Arranging output...\n", sep=""))
  pb = txtProgressBar(0, npoints, 0, style = 3)
  for(i in 1:npoints) {
    setTxtProgressBar(pb, i)
    dfout[i,] = as.numeric(dfvec[[i]])
  }
  return(SpatialPointsDataFrame(as(points,"SpatialPoints"),dfout))
}
