summarypixels<-function(pixels, var, fun=mean, freq=NULL, dates = NULL, months = NULL, ...) {
  if(!inherits(pixels,"SpatialPixelsMeteorology") && !inherits(pixelsdata,"character")) stop("'pixels' has to be of class 'SpatialPixelsMeteorology' or a 'character'.")
  if(inherits(pixels,"SpatialPixelsMeteorology")) {
    gt = pixels@grid
    nx = grid@cells.dim[1]
    ny = grid@cells.dim[2]
    gdates = grid@dates
    points = as(pixels, "SpatialPoints")
  } else {
    nc = .openreadNetCDF(pixels)
    gt = .readgridtopologyNetCDF(nc)
    gdates = .readdatesNetCDF(nc)
    nx = ncin$dim$X$len
    ny = ncin$dim$Y$len
  }
  
  npoints = nx*ny  
  cat(paste("  Summarizing ", var, " in ", npoints," pixels...\n", sep=""))
  dfvec = vector("list",npoints)
  cnt = 1
  pb = txtProgressBar(0, npoints, 0, style = 3)
  for(i in 1:nx) {
    for(j in 1:ny) {
      setTxtProgressBar(pb, cnt)
      vals = .readvardatapixel(nci, nc$var[[var]], i,j)
      names(vals) = gdates
      dfvec[[cnt]] = .summaryvarpoint(vals, fun = fun, dates = dates, months = months,...)
      cnt = cnt+1
    }
  }  
  noutvars = length(dfvec[[1]])
  dfout = data.frame(matrix(NA,nrow=npoints, ncol=noutvars))
  rownames(dfout) = ids
  outvarnames = names(dfvec[[1]])
  if(!is.null(outvarnames)) names(dfout) = outvarnames
  cat(paste("  Arranging output...\n", sep=""))
  pb = txtProgressBar(0, npoints, 0, style = 3)
  for(i in 1:npoints) {
    setTxtProgressBar(pb, i)
    dfout[i,] = as.numeric(dfvec[[i]])
  }
  return(SpatialPixelsDataFrame(points=points, data=dfout, proj4string=proj4string, grid = gt))
}