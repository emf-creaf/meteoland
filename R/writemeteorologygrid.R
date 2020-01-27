# Writes grid meteorological data (SpatialGridMeteorology) into a netCDF
writemeteorologygrid<-function(object, file, dates = NULL, format = "netCDF", add=FALSE, overwrite = FALSE, verbose = FALSE) {
  if(!inherits(object,"SpatialGridMeteorology")) stop("'object' has to be of class 'SpatialGridMeteorology'.")
  if(is.null(dates)) dates = object@dates
  if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  nc = .openwritegridNetCDF(object@grid, proj4string = proj4string(object), dates = dates,
                                  file=file, add= add, overwrite = overwrite, verbose = verbose)
  if(length(dates)>0) .writemeteorologygridNetCDF(data = object@data[as.character(dates)], 
                              grid=object@grid, proj4string = proj4string(object), 
                              nc=nc, verbose = verbose)
  .closeNetCDF(file,nc, verbose = verbose)
}
# Writes grid meteorological data (SpatialPixelMeteorology) into a netCDF
writemeteorologypixels<-function(object, file, dates = NULL, format = "netCDF", add=FALSE, overwrite = FALSE, verbose = FALSE) {
  if(!inherits(object,"SpatialPixelsMeteorology")) stop("'object' has to be of class 'SpatialPixelsMeteorology'.")
  if(is.null(dates)) dates = object@dates
  if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  nc = .openwritegridNetCDF(object@grid, proj4string = proj4string(object), dates = dates,
                                  file=file, add= add, overwrite = overwrite, verbose = verbose)
  if(length(dates)>0) .writemeteorologypixelsNetCDF(data = object@data[as.character(dates)], 
                                pixels=object, proj4string = proj4string(object), 
                                nc=nc)
  .closeNetCDF(file,nc, verbose = verbose)
}
# Replaces the content of a grid pixel in an existing netCDF
writemeteorologygridpixel<-function(file, index, data, verbose = FALSE) {
  nc = .openaddNetCDF(file=file, verbose=verbose)
  gt = .readgridtopologyNetCDF(nc)
  gdates = .readdatesNetCDF(nc)
  ny = gt@cells.dim[2]
  nt = length(gdates)
  cv = coordinatevalues(gt)
  cci = coordinates(gt)[index,]
  i = which(cv[[1]]==cci[1])
  j = which(cv[[2]]==cci[2])
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
  if("MeanTemperature" %in% names(data)) .putvardatapixel(nc,ny, nt, varMeanTemp, i, j, data$MeanTemperature)
  if("MinTemperature" %in% names(data)) .putvardatapixel(nc,ny, nt, varMinTemp, i, j, data$MinTemperature)
  if("MaxTemperature" %in% names(data)) .putvardatapixel( nc, ny, nt, varMaxTemp, i, j, data$MaxTemperature)
  if("Precipitation" %in% names(data)) .putvardatapixel( nc, ny, nt, varPrec, i, j, data$Precipitation)
  if("MeanRelativeHumidity" %in% names(data)) .putvardatapixel( nc, ny, nt, varMeanRH, i, j, data$MeanRelativeHumidity)
  if("MinRelativeHumidity" %in% names(data)) .putvardatapixel( nc, ny, nt, varMinRH, i, j, data$MinRelativeHumidity)
  if("MaxRelativeHumidity" %in% names(data)) .putvardatapixel( nc, ny, nt, varMaxRH, i, j, data$MaxRelativeHumidity)
  if("Radiation" %in% names(data)) .putvardatapixel( nc, ny, nt, varRad, i, j, data$Radiation)
  if("WindSpeed" %in% names(data)) .putvardatapixel( nc, ny, nt, varWindSpeed, i, j, data$WindSpeed)
  if("WindDirection" %in% names(data)) .putvardatapixel( nc, ny, nt, varWindDirection, i, j, data$WindDirection)
  if("PET" %in% names(data)) .putvardatapixel( nc, ny, nt, varPET, i, j, data$PET)
  
  .closeNetCDF(file, nc, verbose=verbose)
}