.extractSGMSPMindexdata<-function(grid, index) {
  gdates = grid@dates
  ndates = length(gdates)
  varnames = names(grid@data[[1]])
  df = data.frame(matrix(NA, nrow = ndates, ncol=length(varnames)))
  colnames(df)= varnames
  rownames(df)=as.character(gdates)
  ng = nrow(grid@data[[1]])
  if(inherits(grid,"SpatialPixelsMeteorology")) {
    index = which(grid@grid.index==index)
    if(length(index)==0) stop("This grid index does not have data.")
  } else {
    if(!(index %in% 1:ng)) stop(paste("Supplied grid index outside ", ng, " grid cells."))
  }
  for(i in 1:ndates) {
    obs = grid@data[[i]]
    df[i,] = obs[index,]
  }
  return(df)
}
extractgridindex<-function(grid, index) {
  if(!inherits(grid,"SpatialGridMeteorology") && !inherits(grid,"SpatialPixelsMeteorology") && !inherits(grid,"character")) 
    stop("'grid' has to be of class 'SpatialGridMeteorology', 'SpatialPixelsMeteorology' or 'character'.")
  if(inherits(grid,"SpatialGridMeteorology") || inherits(grid,"SpatialPixelsMeteorology")) {
    return(.extractSGMSPMindexdata(grid, index))
  } else {
    ncin = .openreadgridNetCDF(grid, verbose=FALSE)
    gt = .readgridtopologyNetCDF(ncin)
    gdates = .readdatesNetCDF(ncin)
    ny = gt@cells.dim[2]
    nt = length(gdates)
    cv = coordinatevalues(gt)
    cci = coordinates(gt)[index,]
    i = which(cv[[1]]==cci[1])
    j = which(cv[[2]]==cci[2])
    df = .readgriddatapixel(ncin, ny, nt, i,j)
    .closeNetCDF(grid, ncin, verbose=FALSE)
    return(df)
  }
}
extractgridpoints<-function(grid, points, verbose = FALSE) {
  if(!inherits(points,"SpatialPoints")) stop("'points' has to be of class 'SpatialPoints'.")
  if(!inherits(grid,"SpatialGridMeteorology") && !inherits(grid,"SpatialPixelsMeteorology") && !inherits(grid,"character")) 
    stop("'grid' has to be of class 'SpatialGridMeteorology', 'SpatialPixelsMeteorology' or 'character'.")
  points = as(points,"SpatialPoints")

  
  if(inherits(grid,"SpatialGridMeteorology") || inherits(grid,"SpatialPixelsMeteorology")) {
    gdates = grid@dates
    gt =  getGridTopology(grid)
    proj4string = grid@proj4string
  } else {
    ncin = .openreadgridNetCDF(grid, verbose=FALSE)
    gdates = .readdatesNetCDF(ncin)
    gt = .readgridtopologyNetCDF(ncin)
    proj4string = .readCRSNetCDF(ncin)
  }
  
  indices = getGridIndex(spTransform(points, proj4string)@coords, gt, all.inside = FALSE)
  if(inherits(grid,"SpatialPixelsMeteorology")) {
    indices[!(indices %in% grid@grid.index)] = NA
  }
  sel = !is.na(indices)
  if(sum(sel)==0) stop("All points are outside the grid cells with data.")
  else if(sum(sel)<length(sel)) if(verbose) cat(paste0(length(sel)- sum(sel)," point(s) fall(s) into grid cells that lack meteorology data and will be discarded.\n"))
  indices = indices[sel]
  points = points[sel]
  npoints = length(points)
  res = vector("list",npoints)
  nt = length(gdates)
  ny = gt@cells.dim[2]

  cv = coordinatevalues(gt)
  ccind = coordinates(gt)[indices,, drop=FALSE]

  if(verbose) pb = txtProgressBar(0, npoints, 0, style = 3)
  for(i in 1:npoints) {
    if(verbose) setTxtProgressBar(pb, i)
    if(inherits(grid,"SpatialGridMeteorology") || inherits(grid,"SpatialPixelsMeteorology")) {
      res[[i]] = .extractSGMSPMindexdata(grid, indices[i])
    } else {
      j = which(cv[[1]]==ccind[i,1])
      k = which(cv[[2]]==ccind[i,2])
      res[[i]] = .readgriddatapixel(ncin, ny, nt, j,k)
    }
  }
  if(!inherits(grid,"SpatialGridMeteorology") && !inherits(grid,"SpatialPixelsMeteorology")) {
    .closeNetCDF(grid, ncin, verbose=FALSE)
  }
  
  return(SpatialPointsMeteorology(points, res, gdates))
}