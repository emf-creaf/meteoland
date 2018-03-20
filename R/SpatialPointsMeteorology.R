SpatialPointsMeteorology<-function(points, data, dates, dataByDate = FALSE) {
  if(!inherits(points, "SpatialPoints")) stop("'points' has to be of class 'SpatialPoints'")
  if(!inherits(dates, "Date")) stop("'dates' has to be of class 'Date'")
  if(!is.list(data)) stop("'data' has to be a list of data frames")
  ndata = length(data)
  for(i in 1:ndata) {
    if(!inherits(data[[i]], "data.frame")) stop("'data' has to be a list of data frames")
  }
  npoints = length(points)
  ndays = length(dates)
  if(!dataByDate) {
    if(ndata!=npoints) stop("The number of points has to be the same as the length of 'data'")
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
  } else {
    if(ndata!=ndays) stop("The number of dates has to be the same as the length of 'data'")
    varnames = c("DOY","MeanTemperature","MinTemperature","MaxTemperature","Precipitation","MeanRelativeHumidity","MinRelativeHumidity","MaxRelativeHumidity","Radiation", "WindSpeed", "WindDirection")
    nvar = length(varnames)
    datavec = vector("list", npoints)
    for(i in 1:npoints) {
      datavec[[i]] = data.frame(matrix(NA, nrow=ndays, ncol=nvar), row.names = as.character(dates))
      names(datavec[[i]]) = varnames
      datavec[[i]]$DOY = as.numeric(format(dates,"%j"))
    }
    names(datavec) = rownames(points@coords)
    for(j in 1:ndays) {
      df = data[[j]]
      dfcn = names(df)
      dfrn = row.names(df)
      for(h in 1:nrow(df)) {
        print(dfrn[h])
        if(dfrn[h] %in% rownames(points@coords)) {
          i = which(rownames(points@coords)==dfrn[h])
          for(var in varnames) if(var %in% dfcn) datavec[[i]][j,var] = df[h,var]
        }
      }
    }
    spm = new("SpatialPointsMeteorology",
              coords = points@coords,
              bbox = points@bbox,
              proj4string = points@proj4string,
              data = datavec,
              dates = dates)
  }
  return(spm)
}
