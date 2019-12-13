.writemeteorologygridNetCDF<-function(data, grid, proj4string, dates, file, format = "netCDF") {
  nx = grid@cells.dim[1]
  ny = grid@cells.dim[2]
  #Writes rows by decreasing order
  putvargriddataday<-function(nc, var, datavec, day) {
    for(i in 1:ny) ncvar_put(nc, varid=var, vals=datavec[((i-1)*nx+1):(i*nx)], start=c(1,ny-i+1, day), count=c(nx,1,1))
  }
  if(format=="netCDF") {
    tunits = "days since 1970-01-01 00:00:00.0 -0:00"
    dimX <- ncdim_def( "X", "meters", sort(unique(coordinates(grid)[,1])))
    dimY <- ncdim_def( "Y", "meters", sort(unique(coordinates(grid)[,2])))
    time <- ncdim_def("time", tunits, as.double(as.Date(dates)))
    varMeanTemp <- ncvar_def( "MeanTemperature", "Celsius", list(dimX,dimY, time), NA)
    varMinTemp <- ncvar_def( "MinTemperature", "Celsius", list(dimX,dimY, time), NA)
    varMaxTemp <- ncvar_def( "MaxTemperature", "Celsius", list(dimX,dimY, time), NA)
    varPrec <- ncvar_def( "Precipitation", "L/m2", list(dimX,dimY, time), NA)
    varMeanRH <- ncvar_def( "MeanRelativeHumidity", "%", list(dimX,dimY, time), NA)
    varMinRH <- ncvar_def( "MinRelativeHumidity", "%", list(dimX,dimY, time), NA)
    varMaxRH <- ncvar_def( "MaxRelativeHumidity", "%", list(dimX,dimY, time), NA)
    varRad <- ncvar_def( "Radiation", "MJ/m2", list(dimX,dimY, time), NA)
    varWindSpeed <- ncvar_def( "WindSpeed", "m/s", list(dimX,dimY, time), NA)
    varWindDirection <- ncvar_def( "WindDirection", "Degrees", list(dimX,dimY, time), NA)
    varPET <- ncvar_def( "PET", "L/m2", list(dimX,dimY, time), NA)
    nc <- nc_create(file, list(varMeanTemp,varMinTemp,varMaxTemp,varPrec,
                               varMeanRH, varMinRH,varMaxRH,
                               varRad, varWindSpeed, varWindDirection, varPET) )
    ncatt_put(nc, 0, "proj4string", as.character(proj4string))
    for(j in 1:length(dates)) {
      putvargriddataday(nc,varMeanTemp, data[[i]]$MeanTemperature,j)
      putvargriddataday(nc,varMinTemp, data[[i]]$MinTemperature,j)
      putvargriddataday( nc, varMaxTemp, data[[i]]$MaxTemperature,j)
      putvargriddataday( nc, varPrec, data[[i]]$Precipitation,j)
      putvargriddataday( nc, varMeanRH, data[[i]]$MeanRelativeHumidity,j)
      putvargriddataday( nc, varMinRH, data[[i]]$MinRelativeHumidity,j)
      putvargriddataday( nc, varMaxRH, data[[i]]$MaxRelativeHumidity,j)
      putvargriddataday( nc, varRad, data[[i]]$Radiation,j)
      putvargriddataday( nc, varWindSpeed, data[[i]]$WindSpeed,j)
      putvargriddataday( nc, varWindDirection, data[[i]]$WindDirection,j)
      putvargriddataday( nc, varPET, data[[i]]$PET,j)
    }
    nc_close(nc)
  }
}
writemeteorologygrid<-function(object, dates = NULL, file, format = "netCDF") {
  if(!inherits(object,"SpatialGridMeteorology")) stop("'object' has to be of class 'SpatialGridMeteorology'.")
  if(!is.null(dates)) dates = object@dates
  if((!inherits(date,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  .writemeteorologygridNetCDF(object@data[[as.character(dates)]], object@grid, proj4string(object), dates, file, format)
}
writemeteorologygridfiles<-function(object, dir=getwd(), format ="netCDF", metadatafile="MG.txt") {
  if(!inherits(object,"SpatialGridMeteorology")) stop("'object' has to be of class 'SpatialGridMeteorology'.")
  ndates = length(object@data)
  dates = names(object@data)

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
    .writemeteorologygridNetCDF(object@data[[i]],object@grid,proj4string(object),dates[i],
                          f,format)
  }

  # Write metadata
  if(dir!="") f = paste(dir,metadatafile, sep="/")
  else f = metadatafile
  write.table(dfout,file= f,sep="\t", quote=FALSE)
  invisible(dfout)
}
