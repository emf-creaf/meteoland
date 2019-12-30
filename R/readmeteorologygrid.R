readmeteorologygrid<-function(file, dates = NULL, format = "netCDF", varmapping = NULL) {
  nc = .openreadNetCDF(file)
  if(!is.null(dates)) {
    if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
    sgm = .readmeteorologyNetCDF(nc, dates = as.Date(dates), varmapping = varmapping)
  } else {
    sgm = .readmeteorologyNetCDF(nc, varmapping = varmapping)
  }
  .closeNetCDF(file,nc)
  return(sgm)
}
readmeteorologypixels<-function(file, dates = NULL, format = "netCDF", varmapping = NULL) {
  nc = .openreadNetCDF(file)
  if(!is.null(dates)) {
    if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
    spm = .readmeteorologyNetCDF(nc, dates = as.Date(dates), pixels=T, varmapping = varmapping)
  } else {
    spm = .readmeteorologyNetCDF(nc, pixels=T, varmapping = varmapping)
  }
  .closeNetCDF(file,nc)
  return(spm)
}
# readmeteorologygridfiles<-function(files, format="netCDF") {
#   if((!inherits(files,"character"))&&(!inherits(files,"data.frame"))) stop("'files' has to be a character vector or a data frame with columns 'dir' and 'filename'.")
#   if(inherits(files,"data.frame")) {
#     nfiles = nrow(files)
#     filevec = rep("", nfiles)
#     for(i in 1:nfiles) {
#       if(files$dir[i]!="") filevec[i] = paste(files$dir[i], files$filename[i], sep="/")
#       else filevec[i] = files$filename[i]
#     }
#     files = filevec
#   } else {
#     nfiles = length(files)
#   }
#   day1met = .readmeteorologygrid(files[1],format)
#   grid = day1met$sgdf@grid
#   proj4string = day1met$sgdf@proj4string
#   dstringvec = rep("", nfiles)
#   if(nfiles>1) {
#     dfvec = vector("list",nfiles)
#     dfvec[[1]] = day1met$sgdf@data
#     names(dfvec)[1] = day1met$date
#     dstringvec[1] =  day1met$date
#     for(i in 2:nfiles) {
#       dayimet = .readmeteorologygrid(files[i],format)
#       dfvec[[i]] = dayimet$sgdf@data
#       names(dfvec)[i] = dayimet$date
#       dstringvec[i] =  dayimet$date
#     }
#     return(SpatialGridMeteorology(grid, proj4string, dfvec, as.Date(dstringvec)))
#   }
#   return(day1met$sgdf)
# }
# readmeteorologygridcells<-function(files, cellIndices, format="netCDF"){
#   if((!inherits(files,"character"))&&(!inherits(files,"data.frame"))) stop("'files' has to be a character vector or a data frame with columns 'dir' and 'filename'.")
#   if(inherits(files,"data.frame")) {
#     nfiles = nrow(files)
#     filevec = rep("", nfiles)
#     for(i in 1:nfiles) {
#       if(files$dir[i]!="") filevec[i] = paste(files$dir[i], files$filename[i], sep="/")
#       else filevec[i] = files$filename[i]
#     }
#     files = filevec
#   } else {
#     nfiles = length(files)
#   }
#   ncells = length(cellIndices)
# 
#   #List of data frames
#   l = vector("list", ncells)
#   for(i in 1:ncells) l[[i]] = matrix(NA, nrow= nfiles, ncol=11)
# 
#   dates = rep("", nfiles)
#   x = NULL
#   y = NULL
#   crs = CRS(as.character(NA))
#   for(i in 1:nfiles) {
#     ncin <- nc_open(files[i])
#     if(is.null(crs)) {
#       proj4string <- ncatt_get(ncin,0, "proj4string")$value
#       if(proj4string!="NA") crs = CRS(proj4string)
#     }
#     if(is.null(x) || is.null(y)) {
#       dimX <- ncvar_get(ncin, "X")
#       dimY <- ncvar_get(ncin, "Y")
#       nx = length(dimX)
#       ny = length(dimY)
#       cells.dim = c(nx, ny)
#       grid = GridTopology(c(1,1), c(1,1), cells.dim)
#       xy = coordinates(grid)
#       x = xy[cellIndices,1]
#       y = xy[cellIndices,2]
#     }
#     dates[i] <- ncatt_get(ncin,0, "date")$value
#     for(c in 1:ncells) {
#       l[[c]][i,1] = ncvar_get(ncin, "MeanTemperature",start=c(x[c],y[c]), count=c(1,1))
#       l[[c]][i,2] = ncvar_get(ncin, "MinTemperature",start=c(x[c],y[c]), count=c(1,1))
#       l[[c]][i,3] = ncvar_get(ncin, "MaxTemperature",start=c(x[c],y[c]), count=c(1,1))
#       l[[c]][i,4] = ncvar_get(ncin, "Precipitation",start=c(x[c],y[c]), count=c(1,1))
#       l[[c]][i,5] = ncvar_get(ncin, "MeanRelativeHumidity",start=c(x[c],y[c]), count=c(1,1))
#       l[[c]][i,6] = ncvar_get(ncin, "MinRelativeHumidity",start=c(x[c],y[c]), count=c(1,1))
#       l[[c]][i,7] = ncvar_get(ncin, "MaxRelativeHumidity",start=c(x[c],y[c]), count=c(1,1))
#       l[[c]][i,8] = ncvar_get(ncin, "Radiation",start=c(x[c],y[c]), count=c(1,1))
#       l[[c]][i,9] = ncvar_get(ncin, "WindSpeed",start=c(x[c],y[c]), count=c(1,1))
#       l[[c]][i,10] = ncvar_get(ncin, "WindDirection",start=c(x[c],y[c]), count=c(1,1))
#       l[[c]][i,11] = ncvar_get(ncin, "PET",start=c(x[c],y[c]), count=c(1,1))
#     }
#     nc_close(ncin)
#   }
#   for(i in 1:ncells) {
#     l[[i]][is.na(l[[i]])] =NA
#     l[[i]] = data.frame(l[[i]])
#     names(l[[i]])<-c("MeanTemperature","MinTemperature","MaxTemperature",
#                     "Precipitation","MeanRelativeHumidity","MinRelativeHumidity",
#                     "MaxRelativeHumidity","Radiation","WindSpeed","WindDirection",
#                     "PET")
#     rownames(l[[i]])<-dates
#   }
#   points = SpatialPoints(cbind(x,y), crs)
#   return(SpatialPointsMeteorology(points, l, as.Date(dates)))
# }
