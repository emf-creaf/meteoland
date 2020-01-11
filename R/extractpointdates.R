extractpointdates<-function(points, dates = NULL, verbose=FALSE) {
  if(!inherits(points,"SpatialPointsMeteorology") 
     && !inherits(points,"SpatialPointsDataFrame")
     && !inherits(points,"character")) stop("'points' has to be of class 'SpatialPointsMeteorology', 'SpatialPointsDataFrame' or a character string.")
  if(!is.null(dates)) if(class(dates)!="Date") stop("'dates' has to be of class 'Date'.")
  
  if(inherits(points,"SpatialPointsMeteorology") || inherits(points,"SpatialPointsDataFrame")) {
    if(is.null(dates)) dates = points@dates
    npoints = length(points)
    dfvec = vector("list",npoints)
    if(inherits(points,"SpatialPointsMeteorology")) {
      if(!is.null(names(points@data))) ids = names(points@data)
      else ids = 1:npoints
    } else {
      if(!is.null(rownames(points@data))) ids = rownames(points@data)
      else ids = 1:npoints
    }
    ptsout = as(points,"SpatialPoints")
  } else {
    file = points
    ncin = .openreadNetCDF(file)
    crs = .readCRSNetCDF(ncin)
    cc = .readpointcoordinatesNetCDF(ncin, crs)
    ids = rownames(cc)
    dates_file = .readdatesNetCDF(ncin)
    varmapping = .defaultMapping()
    ptsout = SpatialPoints(cc, crs)
  }
  
  npoints = length(ids)
  ndates = length(dates)
  dateschar =as.character(dates)
  if(verbose) cat(paste("  Extracting ", ndates, " dates from ", npoints," points...\n", sep=""))
  res = vector("list", ndates)
  
  if(verbose)  pb = txtProgressBar(0, npoints, 0, style = 3)
  for(i in 1:npoints) {
    if(verbose) setTxtProgressBar(pb, i)
    if(inherits(points,"SpatialPointsMeteorology")) {
      obs = points@data[[i]]
    } else if(inherits(points,"SpatialPointsDataFrame")) {
      f = paste(points@data$dir[i], points@data$filename[i],sep="/")
      if(!file.exists(f)) stop(paste("Observed file '", f,"' does not exist!", sep=""))
      if("format" %in% names(points@data)) { ##Format specified
        obs = readmeteorologypoint(f, format=points@data$format[i])
      } else {
        obs = readmeteorologypoint(f)
      }
    } else {
      obs = .readmeteorologypointNetCDF(ncin,i, dates_file, varmapping)
    }
    for(d in 1:ndates) {
      if(is.null(res[[d]])) {
        res[[d]] = data.frame(matrix(NA, npoints, ncol(obs)))
        row.names(res[[d]])<-ids
        names(res[[d]])<-names(obs)
      }
      res[[d]][i,]<-obs[dateschar[d],]
    }
  }
  if(verbose) cat("\n")
  if(inherits(points,"character")) .closeNetCDF(file,ncin)
  
  for(d in 1:ndates) {
    res[[d]] = SpatialPointsDataFrame(ptsout,res[[d]])
  }
  if(ndates==1) res = res[[1]]
  return(res)
}

.extractgridpixelsdates<-function(object, pixels = FALSE, dates = NULL, verbose = FALSE) {
  if(!is.null(dates)) 
    if((!inherits(dates,"Date")) && (!inherits(dates,"character"))) 
       stop("'dates' has to be of class 'Date' or 'character'.")
  if(inherits(object,"SpatialGridMeteorology")) {
    if(is.null(dates)) dates = object@dates
    dates<-as.character(dates)
    res = vector("list", length(dates))
    names(res)<-dates
    for(i in 1:length(dates)) {
      res[[i]] = SpatialGridDataFrame(object@grid, object@data[[dates[i]]], object@proj4string)
    }
  }
  else if(inherits(object,"SpatialPixelsMeteorology")) {
    if(is.null(dates)) dates = object@dates
    dates<-as.character(dates)
    res = vector("list", length(dates))
    names(res)<-dates
    for(i in 1:length(dates)) {
      res[[i]] = SpatialPixelsDataFrame(object@coords, data = object@data[[dates[i]]], 
                                        grid = object@grid, proj4string = object@proj4string)
    }
  }
  if(length(res)==1) return(res[[1]])
  return(res)  
}
extractgriddates<-function(grid, dates = NULL, verbose=FALSE) {
  if(!inherits(grid,"SpatialGridMeteorology") 
     && !inherits(grid,"character")) 
    stop("'grid' has to be of class 'SpatialGridMeteorology', or the string of a file name.")
  return(.extractgridpixelsdates(grid, pixels = FALSE, dates = dates, verbose = verbose))
}

extractpixelsdates<-function(pixels, dates = NULL, verbose=FALSE) {
  if(!inherits(pixels,"SpatialPixelsMeteorology") 
     && !inherits(pixels,"character")) 
    stop("'pixels' has to be of class 'SpatialPixelsMeteorology', or the string of a file name.")
  return(.extractgridpixelsdates(pixels, pixels = TRUE, dates = dates, verbose = verbose))
}