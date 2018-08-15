summarypixels<-function(pixelsdata, dates = NULL) {
  if(!inherits(pixelsdata,"SpatialPixelsMeteorology") && !inherits(pixelsdata,"data.frame")) stop("'pixelsdata' has to be of class 'SpatialPixelsMeteorology' or 'data.frame'.")
  if(inherits(pixelsdata,"SpatialPixelsMeteorology")) gdates = pixelsdata@dates
  else gdates = row.names(pixelsdata)
  
  if(is.null(dates)) {
    dates = gdates
  } else {
    if(!inherits(dates, "Date")) stop("'dates' has to be of class 'Date'")
    if(sum(as.character(dates) %in% as.character(gdates))<length(dates)) stop("Some dates outside the available period.")
  }
  ndates = length(dates)
  
  cum = NULL
  gt = NULL
  points = NULL
  proj4string = NULL
  pb = txtProgressBar(0, ndates, 0, style = 3)
  for(i in 1:ndates) {
    setTxtProgressBar(pb, i)
    if(inherits(pixelsdata,"SpatialPixelsMeteorology")) {
      obs = pixelsdata@data[[i]]
      if(is.null(cum)) {
        cum = obs
        gt = pixelsdata@grid
        points = as(pixelsdata, "SpatialPoints")
        proj4string = pixelsdata@proj4string
      } else {
        cum = cum + obs
      }
    } else {
      f = paste(pixelsdata$dir[i], pixelsdata$filename[i],sep="/")
      if(!file.exists(f)) stop(paste("Observed file '", f,"' does not exist!", sep=""))
      obs = readmeteorologypixels(f)
      if(is.null(cum)) {
        cum = obs@data
        gt = obs@grid
        points = as(obs, "SpatialPoints")
        proj4string = obs@proj4string
      }else {
        cum = cum + obs@data
      }
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
  return(SpatialPixelsDataFrame(points=points, data=cum, proj4string=proj4string, grid = gt))
}