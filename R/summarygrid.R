summarygrid<-function(griddata, dates = NULL) {
  if(!inherits(griddata,"SpatialGridMeteorology") && !inherits(griddata,"data.frame")) stop("'grid' has to be of class 'SpatialGridMeteorology' or 'data.frame'.")
  if(inherits(griddata,"SpatialGridMeteorology")) gdates = griddata@dates
  else gdates = row.names(griddata)
  
  if(is.null(dates)) {
    dates = gdates
  } else {
    if(!inherits(dates, "Date")) stop("'dates' has to be of class 'Date'")
    if(sum(as.character(dates) %in% as.character(gdates))<length(dates)) stop("Some dates outside the available period.")
  }
  ndates = length(dates)
  
  cum = NULL
  gt = NULL
  proj4string = NULL
  pb = txtProgressBar(0, ndates, 0, style = 3)
  for(i in 1:ndates) {
    setTxtProgressBar(pb, i)
    if(inherits(griddata,"SpatialGridMeteorology")) {
      obs = griddata@data[[i]]
    } else {
      f = paste(griddata$dir[i], griddata$filename[i],sep="/")
      if(!file.exists(f)) stop(paste("Observed file '", f,"' does not exist!", sep=""))
      obs = readmeteorologygrid(f)
    }
    if(is.null(cum)) {
      cum = obs@data
      gt = obs@grid
      proj4string = obs@proj4string
    }else {
      cum = cum + obs@data
    }
  }
  cat("\n")
  cum$MeanTemperature = cum$MeanTemperature/ndates
  cum$MinTemperature = cum$MinTemperature/ndates
  cum$MaxTemperature = cum$MaxTemperature/ndates
  cum$MeanRelativeHumidity = cum$MeanRelativeHumidity/ndates
  cum$MinRelativeHumidity = cum$MinRelativeHumidity/ndates
  cum$MaxRelativeHumidity = cum$MaxRelativeHumidity/ndates
  cum$Radiation = cum$Radiation/ndates
  cum$WindSpeed = cum$WindSpeed/ndates
  return(SpatialGridDataFrame(gt,cum, proj4string))
}