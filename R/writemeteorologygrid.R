.openmeteorologygridNetCDF<-function(grid, proj4string, dates, file, add=FALSE) {
  if(!add) {
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
    cat(paste0("Opening '", file,"' to add/replace data.\n"))
    nc <- nc_open(file, write = T)
  }
  return(nc)
}

.writemeteorologygridNetCDF<-function(data, grid, proj4string, dates, nc) {

  nx = nc$dim$X$len
  ny = nc$dim$Y$len
  dates_file = as.Date(nc$dim$time$vals, origin="1970-01-01")
  if(nx != grid@cells.dim[1]) stop("Number of x-axis values does not match nc file")
  if(ny != grid@cells.dim[2]) stop("Number of y-axis values does not match nc file")
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
  
  #Writes rows by decreasing order
  putvargriddataday<-function(nc, var, datavec, day) {
    for(i in 1:ny) ncvar_put(nc, varid=var, vals=datavec[((i-1)*nx+1):(i*nx)], start=c(1,ny-i+1, day), count=c(nx,1,1))
  }
  for(j in 1:length(dates)) {
    if(as.character(dates[j]) %in% names(data)) {
      day = which(dates_file==dates[j])
      cat(paste0("Writing data for day '", as.character(dates[j]), "' at time position [",day, "].\n"))
      df = data[[as.character(dates[j])]]
      putvargriddataday(nc,varMeanTemp, df$MeanTemperature,day)
      putvargriddataday(nc,varMinTemp, df$MinTemperature,day)
      putvargriddataday( nc, varMaxTemp, df$MaxTemperature,day)
      putvargriddataday( nc, varPrec, df$Precipitation,day)
      putvargriddataday( nc, varMeanRH, df$MeanRelativeHumidity,day)
      putvargriddataday( nc, varMinRH, df$MinRelativeHumidity,day)
      putvargriddataday( nc, varMaxRH, df$MaxRelativeHumidity,day)
      putvargriddataday( nc, varRad, df$Radiation,day)
      putvargriddataday( nc, varWindSpeed, df$WindSpeed,day)
      putvargriddataday( nc, varWindDirection, df$WindDirection,day)
      putvargriddataday( nc, varPET, df$PET,day)
    }
  }
}
writemeteorologygrid<-function(object, file, dates = NULL, format = "netCDF", add=FALSE) {
  if(!inherits(object,"SpatialGridMeteorology")) stop("'object' has to be of class 'SpatialGridMeteorology'.")
  if(is.null(dates)) dates = object@dates
  if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  nc = .openmeteorologygridNetCDF(object@grid, proj4string = proj4string(object), dates = dates,
                                  file=file, add= add)
  .writemeteorologygridNetCDF(data = object@data, 
                              grid=object@grid, proj4string = proj4string(object), 
                              dates = dates, nc=nc)
  cat(paste0("Closing '", file,"'.\n"))
  nc_close(nc)
}
# writemeteorologygridfiles<-function(object, dir=getwd(), format ="netCDF", metadatafile="MG.txt") {
#   if(!inherits(object,"SpatialGridMeteorology")) stop("'object' has to be of class 'SpatialGridMeteorology'.")
#   ndates = length(object@data)
#   dates = names(object@data)
# 
#   # Define meta data frame
#   dfout = data.frame(dir = rep(dir, ndates), filename=rep("", ndates))
#   dfout$dir = as.character(dfout$dir)
#   dfout$filename = as.character(dfout$filename)
#   rownames(dfout) = dates
# 
#   # Write one grid per day
#   for(i in 1:ndates) {
#     if(format=="netCDF") dfout$filename[i] = paste(dates[i],".nc", sep="")
#     if(dfout$dir[i]!="") f = paste(dfout$dir[i],dfout$filename[i], sep="/")
#     else f = dfout$filename[i]
#     .writemeteorologygridNetCDF(object@data[[i]],object@grid,proj4string(object),dates[i],
#                           f,format)
#   }
# 
#   # Write metadata
#   if(dir!="") f = paste(dir,metadatafile, sep="/")
#   else f = metadatafile
#   write.table(dfout,file= f,sep="\t", quote=FALSE)
#   invisible(dfout)
# }
