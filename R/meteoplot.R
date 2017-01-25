meteoplot<-function(object, index, var="MeanTemperature", 
                    fun=NULL, freq=NULL, dates = NULL, months = NULL,
                    add = FALSE, ...){
  if(!inherits(object,"SpatialPointsMeteorology") && !inherits(object,"SpatialPointsDataFrame") &&  !inherits(object,"SpatialGridMeteorology")) stop("'object' should be of class 'SpatialPointsMeteorology' or 'SpatialGridMeteorology'.")
  
  VARS = c("MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
           "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
           "Radiation", "WindSpeed", "WindDirection", "PET")
  var = match.arg(var, VARS)
  
  if(inherits(object,"SpatialPointsDataFrame")) {
    f = paste(object@data$dir[index], object@data$filename[index], sep="/")
    cat(paste("Data read from file:", f, "\n"))
    df = readmeteorologypoint(f)
    if(is.null(dates)) dates = as.Date(rownames(df))
  } else if(is.null(dates)) dates = object@dates
  if(!is.null(months)) {
    m = as.numeric(format(dates,"%m"))
    dates = dates[m %in% months]
  }
  if(inherits(object,"SpatialPointsMeteorology")) {
    vec = object@data[[index]][as.character(dates),var]
  } else if(inherits(object, "SpatialPointsDataFrame")) {
    vec = df[as.character(dates),var]
  } else {
    vec = numeric(length(dates))
    for(i in 1:length(dates)) {
      idate = which(object@dates == dates[i])
      vec[i] = object@data[[idate]][index,var]
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
