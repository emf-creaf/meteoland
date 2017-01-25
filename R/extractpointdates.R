extractpointdates<-function(points, dates = NULL) {
  if(!inherits(points,"SpatialPointsMeteorology") && !inherits(points,"SpatialPointsDataFrame")) stop("'points' has to be of class 'SpatialPointsMeteorology' or 'SpatialPointsDataFrame'.")
  if(class(dates)!="Date") stop("'dates' has to be of class 'Date'.")
  npoints = length(points)
  ndates = length(dates)
  cat(paste("  Extracting ", ndates, " dates from ", npoints," points...\n", sep=""))
  
  dfvec = vector("list",npoints)
  if(inherits(points,"SpatialPointsMeteorology")) {
    if(!is.null(names(points@data))) ids = names(points@data)
    else ids = 1:npoints
  } else {
    if(!is.null(rownames(points@data))) ids = rownames(points@data)
    else ids = 1:npoints
  }
  dateschar =as.character(dates)
  res = vector("list", ndates)
  pb = txtProgressBar(0, npoints, 0, style = 3)
  for(i in 1:npoints) {
    setTxtProgressBar(pb, i)
    if(inherits(points,"SpatialPointsMeteorology")) {
      obs = points@data[[i]]
    } else {
      f = paste(points@data$dir[i], points@data$filename[i],sep="/")
      if(!file.exists(f)) stop(paste("Observed file '", f,"' does not exist!", sep=""))
      obs = readmeteorologypoint(f)
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
  cat("\n")
  for(d in 1:ndates) {
    res[[d]] = SpatialPointsDataFrame(as(points,"SpatialPoints"),res[[d]])
  }
  if(ndates==1) res = res[[1]]
  return(res)
}