SpatialGridMeteorology<-function(grid, proj4string=CRS(as.character(NA)), data, dates) {
  if(!inherits(grid, "GridTopology")) stop("'grid' has to be of class 'GridTopology'")
  if(!inherits(dates, "Date")) stop("'date' has to be of class 'Date'")
  ndates = length(dates)
  cc = coordinates(grid)
  if(!inherits(proj4string, "CRS")) stop("'proj4string' has to be of class 'CRS'")
  if(!inherits(data, "list")) stop("'data' has to be a list of data frames")
  if(length(data)!=ndates) stop("Number of data frames must be equal to the number of dates")
  if(ndates>0) {
    for(i in 1:ndates) {
      if(!inherits(data[[i]], "data.frame")) stop("'data' has to be a list of data frames")
      if(nrow(data[[i]])!=nrow(cc)) stop("Number of rows in all data frames have to be equal to the number of grid cells")
    }
    nvar = ncol(data[[1]])
    varnames = names(data[[1]])
    if(ndates>1) {
      for(i in 2:ndates) {
        if(ncol(data[[i]])!=nvar) stop("Number of variables have to be the same for all data frames")
        if(sum(names(data[[i]])==varnames)<nvar) stop("Variables need to be named equally in all data frames")
      }
    }
  }
  sg = SpatialGrid(grid, proj4string)
  spm = new("SpatialGridMeteorology",
            grid = sg@grid,
            bbox = sg@bbox,
            proj4string = proj4string,
            data = data,
            dates = dates)
  return(spm)
}

setMethod("spplot", signature("SpatialGridMeteorology"), definition=
            function(obj, date, variable="MeanTemperature", ...) {
              sgd = SpatialGridDataFrame(grid = obj@grid, data=obj@data[[date]], 
                                         proj4string=obj@proj4string)
              spplot(sgd, variable, ...)
            }
)

print.SpatialGridMeteorology = function(x, ...) {
  cat("Object of class SpatialGridMeteorology\n")
  cat("Dates: ", paste0(length(x@dates)))
  cat(paste0("  (initial: ", x@dates[1], " final: ", x@dates[length(x@dates)],")\n"))
  print(summary(x@grid))
  pst <- paste(strwrap(paste(
    "Coordinate Reference System (CRS) arguments:", 
    proj4string(x))), collapse="\n")
  cat(pst, "\n")
  invisible(x)
}
setMethod("print", "SpatialGridMeteorology", function(x, ...) print.SpatialGridMeteorology(x, ...))
setMethod("show", "SpatialGridMeteorology", function(object) print.SpatialGridMeteorology(object))

subs.SpatialGridMeteorology <- function(x, i, j, ..., drop = FALSE) {
  drop <- FALSE
  #		if (!missing(drop))
  #			stop("don't supply drop: it needs to be FALSE anyway")
  grd = x@grid
  if (missing(i))
    rows = 1:grd@cells.dim[2]
  else {
    if (is(i, "Spatial"))
      stop("area selection only makes sense for objects of class SpatialPixels or SpatialGridDataFrame; for object of class SpatialGrid you can only select x[rows,cols]")
    rows = i
  }
  if (missing(j))
    cols = 1:grd@cells.dim[1]
  else
    cols = j
  idx = 1:prod(grd@cells.dim[1:2])
  m = matrix(idx, grd@cells.dim[2], grd@cells.dim[1], byrow = TRUE)[rows,cols]
  idx = as.vector(t(m)) # t(m)?
  # print(idx)
  if (any(is.na(idx)))
    stop("NAs not permitted in index")
  if (length(idx) == 0) {
    return(x)
  } 
  pts = SpatialPoints(coordinates(x)[idx,,drop=FALSE], CRS(proj4string(x)))
  if (length(idx) == 1) {
    new("SpatialPointsMeteorology",
        coords = pts@coords,
        bbox = pts@bbox,
        proj4string = pts@proj4string,
        dates = x@dates,
        data = x@data[idx])
  } else {
    sg = as(SpatialPixels(pts), "SpatialGrid")
    new("SpatialGridMeteorology",
        grid = sg@grid,
        bbox = sg@bbox,
        proj4string = sg@proj4string,
        dates = x@dates,
        data = x@data[idx])
  }
}
setMethod("[", "SpatialGridMeteorology", subs.SpatialGridMeteorology)


as.SpGrdMet.STFDF = function(from) {

  datavec = from@data
  data = datavec[[1]]
  if(length(datavec)>1) {
    for(i in 2:length(datavec)) {
       data = rbind(data, datavec[[i]]) 
    }
  }
  time = as.POSIXct(as.Date(from@dates))
  sp = as(from, "SpatialGrid")
  spacetime::STFDF(sp, time, data)
}
setAs("SpatialGridMeteorology", "STFDF", as.SpGrdMet.STFDF)
as.SpGrdMet.stars = function(from) {
  stars::st_as_stars(as.SpGrdMet.STFDF(from))
}
setAs("SpatialGridMeteorology", "stars", as.SpGrdMet.stars)

as.SpGrdMet.SpPtsMet = function(from) {
  points = as(from, "SpatialPoints")
  dates = from@dates
  npoints = length(points)
  ndates = length(dates)
  data = vector("list",npoints)
  rownames(points@coords) = 1:npoints
  names(data) = 1:npoints
  varnames = names(from@data[[1]])
  for(i in 1:npoints) {
    df = data.frame(matrix(NA, nrow = ndates, ncol=length(varnames)))
    colnames(df)= varnames
    rownames(df)=as.character(dates)
    for(j in 1:ndates) df[j,] = from@data[[j]][i,]
    data[[i]] = df
  }
  SpatialPointsMeteorology(points, data = data, dates = dates)
}
setAs("SpatialGridMeteorology", "SpatialPointsMeteorology", as.SpGrdMet.SpPtsMet)
