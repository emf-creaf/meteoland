# Reads one or many gridded data and merges the result into a SpatialGridMeteorology
readmeteorologygrid<-function(files, format = "netCDF", varmapping = NULL,
                              dates = NULL, bbox = NULL, offset = 0, verbose = FALSE) {
  nfiles = length(files)
  l = vector("list", nfiles)
  for(i in 1:nfiles) {
    nc = .openreadgridNetCDF(files[i], verbose = verbose)
    if(!is.null(dates)) {
      if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
      sgm = .readmeteorologygridNetCDF(nc, dates = as.Date(dates), varmapping = varmapping, bbox = bbox, offset = offset, verbose = verbose)
    } else {
      sgm = .readmeteorologygridNetCDF(nc, varmapping = varmapping, bbox = bbox, offset = offset, verbose = verbose)
    }
    .closeNetCDF(files[i],nc, verbose = verbose)
    l[[i]] = sgm
  }
  if(nfiles==1) return(l[[1]])
  if(verbose) cat("\nMerging gridded data...\n")
  return(mergegrids(l, verbose = verbose))
}
# Reads one or many gridded data, subsets pixels with non missing data, and merges the result into a SpatialPixelsMeteorology
readmeteorologypixels<-function(files, format = "netCDF", varmapping = NULL,
                                dates = NULL, bbox = NULL, offset = 0, verbose = FALSE) {
  nfiles = length(files)
  l = vector("list", nfiles)
  for(i in 1:nfiles) {
    nc = .openreadgridNetCDF(files[i], verbose = verbose)
    if(!is.null(dates)) {
      if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
      spm = .readmeteorologygridNetCDF(nc, dates = as.Date(dates), pixels=T, varmapping = varmapping, bbox = bbox, offset = offset, verbose = verbose)
    } else {
      spm = .readmeteorologygridNetCDF(nc, pixels=T, varmapping = varmapping, bbox = bbox, offset = offset, verbose = verbose)
    }
    .closeNetCDF(files[i],nc, verbose = verbose)
    l[[i]] = spm
  }
  if(nfiles==1) return(l[[1]])
  if(verbose) cat("\nMerging gridded data...\n")
  return(mergegrids(l, verbose = verbose))
}
# Reads a subset of grid cells from one or many files and merges the result as a SpatialPointsMeteorology
readmeteorologygridpoints<-function(files, format = "netCDF", varmapping = NULL,
                                    dates = NULL, bbox = NULL, offset = 0, 
                                    relativehumidity = FALSE, verbose = FALSE) {
  nfiles = length(files)
  l = vector("list", nfiles)
  for(i in 1:nfiles) {
    nc = .openreadgridNetCDF(files[i], verbose = verbose)
    if(!is.null(dates)) {
      if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
      sgm = .readmeteorologygridpointsNetCDF(nc, dates = as.Date(dates), varmapping = varmapping, bbox = bbox, offset = offset, verbose = verbose)
    } else {
      sgm = .readmeteorologygridpointsNetCDF(nc, varmapping = varmapping, bbox = bbox, offset = offset, verbose = verbose)
    }
    .closeNetCDF(files[i],nc, verbose = verbose)
    l[[i]] = sgm
  }
  if(nfiles==1) return(l[[1]])
  if(verbose) cat("\nMerging point data...\n")
  mp = mergepoints(l, verbose = verbose)
  if(relativehumidity) {
    if(verbose) cat("\nCompleting relative humidity...\n")
    for(i in 1:length(mp@data)) {
      df = mp@data[[i]]
      if(!("MeanRelativeHumidity" %in% names(df)) && ("SpecificHumidity" %in% names(df)) && ("MeanTemperature" %in% names(df))) {
        df$MeanRelativeHumidity = humidity_specific2relative(df$MeanTemperature, df$SpecificHumidity)
      }
      if(!("MinRelativeHumidity" %in% names(df)) && ("SpecificHumidity" %in% names(df)) && ("MaxTemperature" %in% names(df))) {
        df$MinRelativeHumidity = humidity_specific2relative(df$MaxTemperature, df$SpecificHumidity)
      }
      if(!("MaxRelativeHumidity" %in% names(df)) && ("SpecificHumidity" %in% names(df)) && ("MinTemperature" %in% names(df))) {
        df$MaxRelativeHumidity = humidity_specific2relative(df$MinTemperature, df$SpecificHumidity)
      }
      mp@data[[i]] = df
    }
  }
  return(mp)
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
