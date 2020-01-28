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
  if("MeanTemperature" %in% names(data)) .putgridvardatapixel(nc,ny, nt, "MeanTemperature", i, j, data$MeanTemperature)
  if("MinTemperature" %in% names(data)) .putgridvardatapixel(nc,ny, nt, "MinTemperature", i, j, data$MinTemperature)
  if("MaxTemperature" %in% names(data)) .putgridvardatapixel( nc, ny, nt, "MaxTemperature", i, j, data$MaxTemperature)
  if("Precipitation" %in% names(data)) .putgridvardatapixel( nc, ny, nt, "Precipitation", i, j, data$Precipitation)
  if("MeanRelativeHumidity" %in% names(data)) .putgridvardatapixel( nc, ny, nt, "MeanRelativeHumidity", i, j, data$MeanRelativeHumidity)
  if("MinRelativeHumidity" %in% names(data)) .putgridvardatapixel( nc, ny, nt, "MinRelativeHumidity", i, j, data$MinRelativeHumidity)
  if("MaxRelativeHumidity" %in% names(data)) .putgridvardatapixel( nc, ny, nt, "MaxRelativeHumidity", i, j, data$MaxRelativeHumidity)
  if("Radiation" %in% names(data)) .putgridvardatapixel( nc, ny, nt, "Radiation", i, j, data$Radiation)
  if("WindSpeed" %in% names(data)) .putgridvardatapixel( nc, ny, nt, "WindSpeed", i, j, data$WindSpeed)
  if("WindDirection" %in% names(data)) .putgridvardatapixel( nc, ny, nt, "WindDirection", i, j, data$WindDirection)
  if("PET" %in% names(data)) .putgridvardatapixel( nc, ny, nt, "PET", i, j, data$PET)
  
  .closeNetCDF(file, nc, verbose=verbose)
}