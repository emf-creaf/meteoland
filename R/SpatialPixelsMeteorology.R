SpatialPixelsMeteorology<-function(points, data, dates, tolerance = sqrt(.Machine$double.eps),
                                   proj4string = CRS(as.character(NA)), round = NULL, grid = NULL) {
  spx = SpatialPixels(points, tolerance, proj4string, round, grid) 
  if(!inherits(dates, "Date")) stop("'date' has to be of class 'Date'")
  ndates = length(dates)
  cc = spx@coords
  if(!inherits(data, "list")) stop("'data' has to be a list of data frames")
  if(length(data)!=ndates) stop("Number of data frames must be equal to the number of dates")
  for(i in 1:ndates) {
    if(!inherits(data[[i]], "data.frame")) stop("'data' has to be a list of data frames")
    if(nrow(data[[i]])!=nrow(cc)) stop("Number of rows in all data frames have to be equal to the number of pixels")
  }
  nvar = ncol(data[[1]])
  varnames = names(data[[1]])
  if(ndates>1) {
    for(i in 2:ndates) {
      if(ncol(data[[i]])!=nvar) stop("Number of variables have to be the same for all data frames")
      if(sum(names(data[[i]])==varnames)<nvar) stop("Variables need to be named equally in all data frames")
    }
  }
  spm = new("SpatialPixelsMeteorology",
            coords = cc,
            grid = spx@grid,
            grid.index = spx@grid.index,
            bbox = spx@bbox,
            proj4string = spx@proj4string,
            data = data,
            dates = dates)
  return(spm)
}
