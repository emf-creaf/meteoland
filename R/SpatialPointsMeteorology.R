SpatialPointsMeteorology<-function(points, data, dates) {
  if(!inherits(points, "SpatialPoints")) stop("'points' has to be of class 'SpatialPoints'")
  if(!inherits(dates, "Date")) stop("'date' has to be of class 'Date'")
  if(!inherits(data, "list")) stop("'data' has to be a list of data frames")
  ndata = length(data)
  ndays = length(dates)
  if(ndata!=length(points)) stop("The number of points has to be the same as the length of 'data'")
  for(i in 1:ndata) {
    if(!inherits(data[[i]], "data.frame")) stop("'data' has to be a list of data frames")
  }
  names(data) = rownames(points@coords)
  nvar = ncol(data[[1]])
  varnames = names(data[[1]])
  if(ndata>1) {
    for(i in 2:ndata) {
      if(ncol(data[[i]])!=nvar) stop("Number of variables have to be the same for all data frames")
      if(sum(names(data[[i]])==varnames)<nvar) stop("Variables need to be named equally in all data frames")
      if(sum(rownames(data[[i]])==as.character(dates))<ndays) stop("Data frames must have 'dates' as row names")
    }
  }
  spm = new("SpatialPointsMeteorology",
            coords = points@coords,
            bbox = points@bbox,
            proj4string = points@proj4string,
            data = data,
            dates = dates)
  return(spm)
}
