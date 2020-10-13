# Writes grid meteorological data (SpatialGridMeteorology) into a netCDF
writemeteorologygrid<-function(object, file, dates = NULL, format = "netCDF", 
                               byPixel = FALSE, chunksizes = NA, 
                               add=FALSE, overwrite = FALSE, verbose = FALSE) {
  if(!inherits(object,"SpatialGridMeteorology")) stop("'object' has to be of class 'SpatialGridMeteorology'.")
  if(is.null(dates)) dates = object@dates
  if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  vars = NULL
  if(length(dates)>0) vars = names(object@data[[1]])
  nc = .openwritegridNetCDF(object@grid, proj4string = proj4string(object), dates = dates, vars = vars,
                            file=file, byPixel = byPixel, chunksizes = chunksizes, 
                            add= add, overwrite = overwrite, verbose = verbose)
  if(length(dates)>0) .writemeteorologygridNetCDF(data = object@data[as.character(dates)], 
                              grid=object@grid, proj4string = proj4string(object), 
                              nc=nc, byPixel = byPixel, verbose = verbose)
  .closeNetCDF(file,nc, verbose = verbose)
}
# Writes grid meteorological data (SpatialPixelMeteorology) into a netCDF
writemeteorologypixels<-function(object, file, dates = NULL, format = "netCDF", 
                                 byPixel = FALSE, chunksizes = NA, 
                                 add=FALSE, overwrite = FALSE, verbose = FALSE) {
  if(!inherits(object,"SpatialPixelsMeteorology")) stop("'object' has to be of class 'SpatialPixelsMeteorology'.")
  if(is.null(dates)) dates = object@dates
  if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  vars = NULL
  if(length(dates)>0) vars = names(object@data[[1]])
  nc = .openwritegridNetCDF(object@grid, proj4string = proj4string(object), dates = dates, vars = vars,
                                  file=file, byPixel = byPixel, chunksizes = chunksizes, add= add, overwrite = overwrite, verbose = verbose)
  if(length(dates)>0) .writemeteorologypixelsNetCDF(data = object@data[as.character(dates)], 
                                pixels=object, proj4string = proj4string(object), 
                                byPixel = byPixel, nc=nc)
  .closeNetCDF(file,nc, verbose = verbose)
}
writeemptymeteorologygrid<-function(file, grid, proj4string, dates, byPixel = FALSE, chunksizes = NA, overwrite = FALSE, verbose = FALSE) {
  if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  nc = .openwritegridNetCDF(grid, proj4string = proj4string, dates = dates, vars = NULL,
                            file=file, byPixel = byPixel, chunksizes = chunksizes, 
                            add= FALSE, overwrite = overwrite, verbose = verbose)
  .closeNetCDF(file,nc, verbose = verbose)
}

# Replaces the content of a grid pixel in an existing netCDF
writemeteorologygridpixel<-function(file, index, data, verbose = FALSE) {
  nc = .openaddNetCDF(file=file, verbose=verbose)
  gt = .readgridtopologyNetCDF(nc)
  gdates = .readdatesNetCDF(nc)
  ny = gt@cells.dim[2]
  cv = coordinatevalues(gt)
  cci = coordinates(gt)[index,]
  i = which(cv[[1]]==cci[1])
  j = which(cv[[2]]==cci[2])
  dates = as.Date(row.names(data))
  t = which(as.character(gdates)==as.character(dates[1]))
  .putgridpixel(nc,ny, i,j,t,data)
  .closeNetCDF(file, nc, verbose=verbose)
}