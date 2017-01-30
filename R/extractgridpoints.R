extractgridpoints<-function(griddata, points) {
  if(!inherits(points,"SpatialPoints")) stop("'points' has to be of class 'SpatialPoints'.")
  if(!inherits(griddata,"SpatialGridMeteorology") && !inherits(griddata,"data.frame")) stop("'grid' has to be of class 'SpatialGridMeteorology' or 'data.frame'.")
  if(inherits(griddata,"SpatialGridMeteorology")) gdates = griddata@dates
  else gdates = row.names(griddata)
  
  dates = gdates
  ndates = length(dates)
  npoints = length(points)
  
  if(inherits(griddata,"SpatialGridMeteorology")) {
    gt =  getGridTopology(griddata)
    proj4string = griddata@proj4string
    varnames = names(griddata@data[[1]])
  } else {
    f = paste(griddata$dir[1], griddata$filename[1],sep="/")
    if(!file.exists(f)) stop(paste("Observed file '", f,"' does not exist!", sep=""))
    obs = readmeteorologygrid(f)
    gt = getGridTopology(obs)
    proj4string = obs@proj4string
    varnames = names(obs)
  }
  res = vector("list",npoints)
  for(i in 1:npoints) {
    res[[i]] = data.frame(matrix(NA, nrow = ndates, ncol=length(varnames)))
    colnames(res[[i]])= varnames
    rownames(res[[i]])=as.character(dates)
  }
  indices = getGridIndex(spTransform(points, proj4string)@coords, gt, TRUE)
  pb = txtProgressBar(0, ndates, 0, style = 3)
  for(i in 1:ndates) {
    setTxtProgressBar(pb, i)
    if(inherits(griddata,"SpatialGridMeteorology")) {
      obs = griddata@data[[i]]
    } else {
      f = paste(griddata$dir[i], griddata$filename[i],sep="/")
      if(!file.exists(f)) stop(paste("Observed file '", f,"' does not exist!", sep=""))
      obs = readmeteorologygrid(f)
    }
    for(j in 1:npoints) {
      res[[j]][i,] = obs[indices[j],]
    }
  }
  cat("\n")
  return(SpatialPointsMeteorology(points, res, dates))
}