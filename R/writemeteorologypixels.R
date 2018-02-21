.writemeteorologypixelsNetCDF<-function(data, pixels, proj4string, date, file, format = "netCDF") {
  nx = pixels@grid@cells.dim[1]
  ny = pixels@grid@cells.dim[2]
  ccgrid = coordinates(pixels@grid)
  index = pixels@grid.index
  #Writes rows by decreasing order
  putvargriddata<-function(nc, var, datavec) {
    datavecfull = rep(NA, ny*nx)
    datavecfull[index] = datavec
    for(i in 1:ny) ncvar_put(nc, var, datavecfull[((i-1)*nx+1):(i*nx)], start=c(1,ny-i+1), count=c(nx,1))
  }
  if(format=="netCDF") {
    dimX <- ncdim_def( "X", "", sort(unique(ccgrid[,1])))
    dimY <- ncdim_def( "Y", "", sort(unique(ccgrid[,2])))
    varMeanTemp <- ncvar_def( "MeanTemperature", "Celsius", list(dimX,dimY))
    varMinTemp <- ncvar_def( "MinTemperature", "Celsius", list(dimX,dimY))
    varMaxTemp <- ncvar_def( "MaxTemperature", "Celsius", list(dimX,dimY))
    varPrec <- ncvar_def( "Precipitation", "L/m2", list(dimX,dimY))
    varMeanRH <- ncvar_def( "MeanRelativeHumidity", "%", list(dimX,dimY))
    varMinRH <- ncvar_def( "MinRelativeHumidity", "%", list(dimX,dimY))
    varMaxRH <- ncvar_def( "MaxRelativeHumidity", "%", list(dimX,dimY))
    varRad <- ncvar_def( "Radiation", "MJ/m2", list(dimX,dimY))
    varWindSpeed <- ncvar_def( "WindSpeed", "m/s", list(dimX,dimY))
    varWindDirection <- ncvar_def( "WindDirection", "Degrees", list(dimX,dimY))
    varPET <- ncvar_def( "PET", "L/m2", list(dimX,dimY), NA)
    nc <- nc_create(file, list(varMeanTemp,varMinTemp,varMaxTemp,varPrec,
                               varMeanRH, varMinRH,varMaxRH,
                               varRad, varWindSpeed, varWindDirection, varPET) )
    ncatt_put(nc, 0, "proj4string", as.character(proj4string))
    ncatt_put(nc, 0, "date", as.character(date))
    putvargriddata(nc,varMeanTemp, data$MeanTemperature)
    putvargriddata(nc,varMinTemp, data$MinTemperature)
    putvargriddata( nc, varMaxTemp, data$MaxTemperature)
    putvargriddata( nc, varPrec, data$Precipitation)
    putvargriddata( nc, varMeanRH, data$MeanRelativeHumidity)
    putvargriddata( nc, varMinRH, data$MinRelativeHumidity)
    putvargriddata( nc, varMaxRH, data$MaxRelativeHumidity)
    putvargriddata( nc, varRad, data$Radiation)
    putvargriddata( nc, varWindSpeed, data$WindSpeed)
    putvargriddata( nc, varWindDirection, data$WindDirection)
    putvargriddata( nc, varPET, data$PET)
    nc_close(nc)
  }
}
writemeteorologypixels<-function(object, date, file, format = "netCDF") {
  if((!inherits(date,"Date"))&&(!inherits(date,"character"))) stop("'date' must be a 'character' or 'Date'")
  date = as.character(date)
  pixels = as(object, "SpatialPixels")
  .writemeteorologypixelsNetCDF(object@data[[date]], pixels, proj4string(object), date, file, format)
}
writemeteorologypixelsfiles<-function(object, dir=getwd(), format ="netCDF", metadatafile="MG.txt") {
  if(!inherits(object,"SpatialPixelsMeteorology")) stop("'object' has to be of class 'SpatialPixelsMeteorology'.")
  ndates = length(object@data)
  dates = names(object@data)
  pixels = as(object, "SpatialPixels")
  
  # Define meta data frame
  dfout = data.frame(dir = rep(dir, ndates), filename=rep("", ndates))
  dfout$dir = as.character(dfout$dir)
  dfout$filename = as.character(dfout$filename)
  rownames(dfout) = dates

  # Write one grid per day
  for(i in 1:ndates) {
    if(format=="netCDF") dfout$filename[i] = paste(dates[i],".nc", sep="")
    if(dfout$dir[i]!="") f = paste(dfout$dir[i],dfout$filename[i], sep="/")
    else f = dfout$filename[i]
    .writemeteorologypixelsNetCDF(object@data[[i]],pixels,proj4string(object),dates[i],
                          f,format)
  }

  # Write metadata
  if(dir!="") f = paste(dir,metadatafile, sep="/")
  else f = metadatafile
  write.table(dfout,file= f,sep="\t", quote=FALSE)
  invisible(dfout)
}
