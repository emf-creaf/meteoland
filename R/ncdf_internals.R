#Opens/creates a NetCDF file for writing
.openmeteorologygridNetCDF<-function(grid, proj4string, dates, file, add=FALSE, overwrite = FALSE) {
  if(!add) {
    if(file.exists(file) & !overwrite) stop(paste0("File '",file,"' already exist. Use 'overwrite = TRUE' to force overwriting or 'add = TRUE' to add/replace content."))
    cat(paste0("Creating '", file,"'.\n"))
    nx = grid@cells.dim[1]
    ny = grid@cells.dim[2]
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
  } else {
    if(!file.exists(file)) stop(paste0("File '", file, "' does not exist."))
    cat(paste0("Opening '", file,"' to add/replace data.\n"))
    nc <- nc_open(file, write = T)
  }
  return(nc)
}
#writes a grid for a single variable and day
.putvardataday<-function(nc, var, datavec, day, index=NULL) {
  nx = nc$dim$X$len
  ny = nc$dim$Y$len
  if(!is.null(index)) {
    datavecfull = rep(NA, ny*nx)
    datavecfull[index] = datavec
  } else {
    datavecfull = datavec
  }
  for(i in 1:ny) ncvar_put(nc, varid=var, vals=datavecfull[((i-1)*nx+1):(i*nx)], start=c(1,ny-i+1, day), count=c(nx,1,1))
}
#Writes full NetCDF grids
.writemeteorologygridNetCDF<-function(data, grid, proj4string, nc, index=NULL) {
  nx = nc$dim$X$len
  ny = nc$dim$Y$len
  dates = as.Date(names(data))
  dates_file = as.Date(nc$dim$time$vals, origin="1970-01-01")
  if(nx != grid@cells.dim[1]) stop("Number of x-axis values does not match X dimension in nc file")
  if(ny != grid@cells.dim[2]) stop("Number of y-axis values does not match Y dimension in nc file")
  varMeanTemp = nc$var$MeanTemperature
  varMinTemp = nc$var$MinTemperature
  varMaxTemp = nc$var$MaxTemperature
  varPrec = nc$var$Precipitation
  varMeanRH = nc$var$MeanRelativeHumidity
  varMinRH = nc$var$MinRelativeHumidity
  varMaxRH = nc$var$MaxRelativeHumidity
  varRad = nc$var$Radiation
  varWindSpeed = nc$var$WindSpeed
  varWindDirection = nc$var$WindDirection
  varPET = nc$var$PET
  dates_file = as.Date(nc$dim$time$vals, origin="1970-01-01")
  if(sum(dates %in% dates_file)<length(dates)) stop("Time axis of nc file does not include all supplied dates")
  
  for(j in 1:length(dates)) {
    if(as.character(dates[j]) %in% names(data)) {
      day = which(dates_file==dates[j])
      cat(paste0("Writing data for day '", as.character(dates[j]), "' at time position [",day, "].\n"))
      df = data[[as.character(dates[j])]]
      .putvardataday(nc,varMeanTemp, df$MeanTemperature,day, index)
      .putvardataday(nc,varMinTemp, df$MinTemperature,day, index)
      .putvardataday( nc, varMaxTemp, df$MaxTemperature,day, index)
      .putvardataday( nc, varPrec, df$Precipitation,day, index)
      .putvardataday( nc, varMeanRH, df$MeanRelativeHumidity,day, index)
      .putvardataday( nc, varMinRH, df$MinRelativeHumidity,day, index)
      .putvardataday( nc, varMaxRH, df$MaxRelativeHumidity,day, index)
      .putvardataday( nc, varRad, df$Radiation,day, index)
      .putvardataday( nc, varWindSpeed, df$WindSpeed,day, index)
      .putvardataday( nc, varWindDirection, df$WindDirection,day, index)
      .putvardataday( nc, varPET, df$PET,day, index)
    }
  }
}
#Writes pixels in a NetCDF grid
.writemeteorologypixelsNetCDF<-function(data, pixels, proj4string, nc) {
  .writemeteorologygridNetCDF(data, pixels@grid, proj4string, nc, index=pixels@grid.index)
}
#Closes file
.closeNetCDF<-function(file,nc) {
  cat(paste0("Closing '", file,"'.\n"))
  nc_close(nc)
}