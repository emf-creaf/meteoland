
writemeteorologygrid<-function(object, file, dates = NULL, format = "netCDF", add=FALSE, overwrite = FALSE) {
  if(!inherits(object,"SpatialGridMeteorology")) stop("'object' has to be of class 'SpatialGridMeteorology'.")
  if(is.null(dates)) dates = object@dates
  if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  nc = .openwriteNetCDF(object@grid, proj4string = proj4string(object), dates = dates,
                                  file=file, add= add, overwrite = overwrite)
  .writemeteorologygridNetCDF(data = object@data[as.character(dates)], 
                              grid=object@grid, proj4string = proj4string(object), 
                              nc=nc)
  .closeNetCDF(file,nc)
}

writemeteorologypixels<-function(object, file, dates = NULL, format = "netCDF", add=FALSE, overwrite = FALSE) {
  if(!inherits(object,"SpatialPixelsMeteorology")) stop("'object' has to be of class 'SpatialPixelsMeteorology'.")
  if(is.null(dates)) dates = object@dates
  if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  nc = .openwriteNetCDF(object@grid, proj4string = proj4string(object), dates = dates,
                                  file=file, add= add, overwrite = overwrite)
  .writemeteorologypixelsNetCDF(data = object@data[as.character(dates)], 
                                pixels=object, proj4string = proj4string(object), 
                                nc=nc)
  .closeNetCDF(file,nc)
}
