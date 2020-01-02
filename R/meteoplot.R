meteoplot<-function(object, index=1, var="MeanTemperature", 
                    fun=NULL, freq=NULL, dates = NULL, months = NULL,
                    add = FALSE, ...){
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
