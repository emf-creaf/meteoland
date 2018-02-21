extractgridpoints<-function(griddata, points) {
  if(!inherits(points,"SpatialPoints")) stop("'points' has to be of class 'SpatialPoints'.")
  if(!inherits(griddata,"SpatialGridMeteorology") && !inherits(griddata,"SpatialPixelsMeteorology") && !inherits(griddata,"data.frame")) 
    stop("'grid' has to be of class 'SpatialGridMeteorology', 'SpatialPixelsMeteorology' or 'data.frame'.")
  if(inherits(griddata,"SpatialGridMeteorology") || inherits(griddata,"SpatialPixelsMeteorology")) gdates = griddata@dates
  else gdates = row.names(griddata)
  
  dates = gdates
  ndates = length(dates)
  npoints = length(points)
  
  if(inherits(griddata,"SpatialGridMeteorology") || inherits(griddata,"SpatialPixelsMeteorology")) {
    gt =  getGridTopology(griddata)
    proj4string = griddata@proj4string
    varnames = names(griddata@data[[1]])
  } else {
    f = paste(griddata$dir[1], griddata$filename[1],sep="/")
    if(!file.exists(f)) stop(paste("Observed file '", f,"' does not exist!", sep=""))
    obs = readmeteorologypixels(f)
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
  ind = getGridIndex(spTransform(points, proj4string)@coords, gt, TRUE)
  if(inherits(griddata,"SpatialPixelsMeteorology")) {
    sel = ind %in% griddata@grid.index
    if(sum(sel)==0) stop("All points are outside the grid cells with data.")
    else if(sum(sel)<length(sel)) warning(paste0(length(sel)- sum(sel)," point(s) fall(s) into grid cells that lack meteorology data and will be discarded."))
    indices = rep(NA, sum(sel)) 
    cnt = 1
    for(id in ind[sel]) {
      indices[cnt] = which(griddata@grid.index == id)
      cnt = cnt + 1
    }
  } else {
    indices = ind
  }
    
  pb = txtProgressBar(0, ndates, 0, style = 3)
  for(i in 1:ndates) {
    setTxtProgressBar(pb, i)
    if(inherits(griddata,"SpatialGridMeteorology") || inherits(griddata,"SpatialPixelsMeteorology")) {
      obs = griddata@data[[i]]
    } else {
      f = paste(griddata$dir[i], griddata$filename[i],sep="/")
      if(!file.exists(f)) stop(paste("Observed file '", f,"' does not exist!", sep=""))
      obs = readmeteorologypixels(f)
    }
    for(j in 1:npoints) {
      res[[j]][i,] = obs[indices[j],]
    }
  }
  cat("\n")
  return(SpatialPointsMeteorology(points, res, dates))
}