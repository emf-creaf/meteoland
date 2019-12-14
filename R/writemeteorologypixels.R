
writemeteorologypixels<-function(object, file, dates = NULL, format = "netCDF", add=FALSE, overwrite = FALSE) {
  if(!inherits(object,"SpatialPixelsMeteorology")) stop("'object' has to be of class 'SpatialPixelsMeteorology'.")
  if(is.null(dates)) dates = object@dates
  if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  nc = .openmeteorologygridNetCDF(object@grid, proj4string = proj4string(object), dates = dates,
                                  file=file, add= add, overwrite = overwrite)
  .writemeteorologypixelsNetCDF(data = object@data[as.character(dates)], 
                                pixels=object, proj4string = proj4string(object), 
                                nc=nc)
  .closeNetCDF(file,nc)
}

# writemeteorologypixelsfiles<-function(object, dir=getwd(), format ="netCDF", metadatafile="MG.txt") {
#   if(!inherits(object,"SpatialPixelsMeteorology")) stop("'object' has to be of class 'SpatialPixelsMeteorology'.")
#   ndates = length(object@data)
#   dates = names(object@data)
#   pixels = as(object, "SpatialPixels")
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
#     .writemeteorologypixelsNetCDF(object@data[[i]],pixels,proj4string(object),dates[i],
#                           f,format)
#   }
# 
#   # Write metadata
#   if(dir!="") f = paste(dir,metadatafile, sep="/")
#   else f = metadatafile
#   write.table(dfout,file= f,sep="\t", quote=FALSE)
#   invisible(dfout)
# }
