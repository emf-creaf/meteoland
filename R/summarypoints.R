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

summarypoint<-function(x, var, fun="mean", freq=NULL, dates = NULL, months= NULL, ...) {
  if(!inherits(x,"data.frame")) stop("'x' has to be a data.frame.")
  VARS = c("MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
           "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
           "Radiation", "WindSpeed", "WindDirection", "PET")
  var = match.arg(var, VARS)
  y = x[[var]]
  names(y) = row.names(x)
  return(.summaryvarpoint(y, fun=fun, freq=freq, dates = dates, months= months,...))
}

summarypoints<-function(points, var, fun=mean, freq=NULL, dates = NULL, months = NULL, ...) {
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