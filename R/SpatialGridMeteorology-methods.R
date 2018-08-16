SpatialGridMeteorology<-function(grid, proj4string=CRS(as.character(NA)), data, dates) {
  if(!inherits(grid, "GridTopology")) stop("'grid' has to be of class 'GridTopology'")
  if(!inherits(dates, "Date")) stop("'date' has to be of class 'Date'")
  ndates = length(dates)
  cc = coordinates(grid)
  if(!inherits(proj4string, "CRS")) stop("'proj4string' has to be of class 'CRS'")
  if(!inherits(data, "list")) stop("'data' has to be a list of data frames")
  if(length(data)!=ndates) stop("Number of data frames must be equal to the number of dates")
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
  cat("SpatialPoints:\n")
  print(coordinates(x))
  pst <- paste(strwrap(paste(
    "Coordinate Reference System (CRS) arguments:", 
    proj4string(x))), collapse="\n")
  cat(pst, "\n")
  invisible(x)
}
setMethod("show", "SpatialGridMeteorology", function(object) print.SpatialGridMeteorology(object))
