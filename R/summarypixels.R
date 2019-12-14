summarypixels<-function(pixels, var, fun=mean, freq=NULL, dates = NULL, months = NULL, ...) {
  if(!inherits(pixels,"SpatialPixelsMeteorology") && !inherits(pixels,"character")) stop("'pixels' has to be of class 'SpatialPixelsMeteorology' or a 'character'.")
  VARS = c("MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
           "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
           "Radiation", "WindSpeed", "WindDirection", "PET")
  var = match.arg(var, VARS)
  
  if(inherits(pixels,"SpatialPixelsMeteorology")) {
    gt = pixels@grid
    nx = grid@cells.dim[1]
    ny = grid@cells.dim[2]
    gdates = grid@dates
    crs = grid@proj4string
    points = as(pixels, "SpatialPoints")
  } else {
    ncin = .openreadNetCDF(pixels)
    gt = .readgridtopologyNetCDF(ncin)
    gdates = .readdatesNetCDF(ncin)
    nx = ncin$dim$X$len
    ny = ncin$dim$Y$len
    crs = .readCRSNetCDF(ncin)
    points = SpatialPoints(coordinates(gt), proj4string = crs)
  }
  
  npoints = nx*ny  
  cat(paste("  Summarizing ", var, " in ", npoints," pixels...\n", sep=""))
  dfvec = vector("list",npoints)
  cnt = 1
  pb = txtProgressBar(0, npoints, 0, style = 3)
  for(j in 1:ny) {
    for(i in 1:nx) {
      setTxtProgressBar(pb, cnt)
      vals = .readvardatapixel(ncin, ncin$var[[var]], i,j)
      names(vals) = as.character(gdates)
      dfvec[[cnt]] = .summaryvarpoint(vals, fun = fun, freq=freq, dates = dates, months = months,...)
      cnt = cnt+1
    }
  }  
  noutvars = length(dfvec[[1]])
  dfout = data.frame(matrix(NA,nrow=npoints, ncol=noutvars))
  outvarnames = names(dfvec[[1]])
  if(!is.null(outvarnames)) names(dfout) = outvarnames
  cat(paste("  Arranging output...\n", sep=""))
  pb = txtProgressBar(0, npoints, 0, style = 3)
  for(i in 1:npoints) {
    setTxtProgressBar(pb, i)
    dfout[i,] = as.numeric(dfvec[[i]])
  }
  return(SpatialPixelsDataFrame(points=points, data=dfout, proj4string=crs, grid = gt))
}