.writemeteorologygrid<-function(data, grid, proj4string, date, file, format = "netCDF") {
  nx = grid@cells.dim[1]
  ny = grid@cells.dim[2]
  #Writes rows by decreasing order
  putvargriddata<-function(nc, var, datavec) {
    for(i in 1:ny) ncvar_put(nc, var, datavec[((i-1)*nx+1):(i*nx)], start=c(1,ny-i+1), count=c(nx,1))
  }
  if(format=="netCDF") {
    dimX <- ncdim_def( "X", "meters", sort(unique(coordinates(grid)[,1])))
    dimY <- ncdim_def( "Y", "meters", sort(unique(coordinates(grid)[,2])))
    varMeanTemp <- ncvar_def( "MeanTemperature", "Celsius", list(dimX,dimY), NA)
    varMinTemp <- ncvar_def( "MinTemperature", "Celsius", list(dimX,dimY), NA)
    varMaxTemp <- ncvar_def( "MaxTemperature", "Celsius", list(dimX,dimY), NA)
    varPrec <- ncvar_def( "Precipitation", "L/m2", list(dimX,dimY), NA)
    varMeanRH <- ncvar_def( "MeanRelativeHumidity", "%", list(dimX,dimY), NA)
    varMinRH <- ncvar_def( "MinRelativeHumidity", "%", list(dimX,dimY), NA)
    varMaxRH <- ncvar_def( "MaxRelativeHumidity", "%", list(dimX,dimY), NA)
    varRad <- ncvar_def( "Radiation", "MJ/m2", list(dimX,dimY), NA)
    varWindSpeed <- ncvar_def( "WindSpeed", "m/s", list(dimX,dimY), NA)
    varWindDirection <- ncvar_def( "WindDirection", "Degrees", list(dimX,dimY), NA)
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
writemeteorologygrid<-function(object, date, file, format = "netCDF") {
  if((!inherits(date,"Date"))&&(!inherits(date,"character"))) stop("'date' must be a 'character' or 'Date'")
  date = as.character(date)
  .writemeteorologygrid(object@data[[date]], object@grid, proj4string(object), date, file, format)
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
    .writemeteorologygrid(object@data[[i]],object@grid,proj4string(object),dates[i],
                          f,format)
  }

  # Write metadata
  if(dir!="") f = paste(dir,metadatafile, sep="/")
  else f = metadatafile
  write.table(dfout,file= f,sep="\t", quote=FALSE)
  invisible(dfout)
}
