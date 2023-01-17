#' Creates a 'SpatialPixelsMeteorology'
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Initializes an object of class \code{SpatialPixelsMeteorology-class}
#' 
#' 
#' @param points An object of class \code{\link{SpatialPoints-class}}.
#' @param data A vector of data frames (one per date).
#' @param dates Object of class \code{"Date"} describing the time period of
#' meteorological estimates.
#' @param tolerance Precision up to which extent points should be exactly on a
#' grid.
#' @param proj4string Object of class \code{\linkS4class{CRS}} in the first
#' form only used when points does not inherit from
#' \code{\linkS4class{Spatial}}.
#' @param round default \code{NULL}, otherwise a value passed to as the digits
#' argument to \code{\link{round}} for setting cell size.
#' @param grid Grid topology using an object of class
#' \code{\linkS4class{GridTopology}}; a value of \code{NULL} implies that this
#' will be derived from the point coordinates.
#' @return An object of class \code{\link{SpatialPixelsMeteorology-class}}
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPixelsMeteorology-class}}
#' @export
SpatialPixelsMeteorology<-function(points, data, dates, tolerance = sqrt(.Machine$double.eps),
                                   proj4string = CRS(as.character(NA)), round = NULL, grid = NULL) {
  
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "SpatialPixelsMeteorology()", with = NULL,
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Meteorology objects are now normal sf points objects"
  )
  
  spx = SpatialPixels(points, tolerance, proj4string, round, grid) 
  if(!inherits(dates, "Date")) stop("'date' has to be of class 'Date'")
  ndates = length(dates)
  cc = spx@coords
  if(!inherits(data, "list")) stop("'data' has to be a list of data frames")
  if(length(data)!=ndates) stop("Number of data frames must be equal to the number of dates")
  if(ndates>0) {
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

#' @export
setMethod("[", "SpatialPixelsMeteorology",
          function(x, i, j, ..., drop = FALSE) {
            if (!missing(j))
              stop("can only select pixels with a single index")
            if (missing(i))
              return(x)
            if (is(i, "Spatial"))
              i = !is.na(over(x, geometry(i)))
            if(length(i)==1) {
              return(new("SpatialPointsMeteorology",
                         coords = x@coords[i,,drop=FALSE],
                         bbox = x@bbox,
                         proj4string = x@proj4string,
                         dates = x@dates,
                         data = x@data[i]))
            }
            if (drop) { # if FALSE: adjust bbox and grid
              res = as(x, "SpatialPoints")[i]
              tolerance = list(...)$tolerance
              if (!is.null(tolerance))
                spdf = SpatialPixels(res, tolerance)
              else
                spdf = SpatialPixels(res)
              new("SpatialPixelsMeteorology",
                  coords = spdf@coords,
                  bbox = spdf@bbox,
                  grid = spdf@grid,
                  grid.index = spdf@grid.index,
                  proj4string = spdf@proj4string,
                  dates = x@dates,
                  data = x@data[i])
            } else # default: don't adjust bbox and grid
              new("SpatialPixelsMeteorology",
                  coords = x@coords[i, , drop = FALSE],
                  bbox = x@bbox,
                  grid = x@grid,
                  grid.index = x@grid.index[i],
                  proj4string = x@proj4string,
                  dates = x@dates,
                  data = x@data[i])
          }
)

#' @export
setMethod("spplot", signature("SpatialPixelsMeteorology"), definition=
            function(obj, date, variable="MeanTemperature", ...) {
              sgd = SpatialPixelsDataFrame(points=obj@coords, grid = obj@grid, data=obj@data[[date]], 
                                           proj4string= obj@proj4string)
              spplot(sgd, variable, ...)
            }
)

print.SpatialPixelsMeteorology = function(x, ...) {
  cat("Object of class SpatialPixelsMeteorology\n")
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

#' @export
setMethod("print", "SpatialPixelsMeteorology", function(x, ..., digits = getOption("digits")) print.SpatialPixelsMeteorology(x, ..., digits))

#' @export
setMethod("show", "SpatialPixelsMeteorology", function(object) print.SpatialPixelsMeteorology(object))


as.SpPxMet.STFDF = function(from) {
  datavec = from@data
  data = datavec[[1]]
  if(length(datavec)>1) {
    for(i in 2:length(datavec)) {
      data = rbind(data, datavec[[i]]) 
    }
  }
  time = as.POSIXct(as.Date(from@dates))
  endTime = as.POSIXct(as.Date(from@dates)+1)
  sp = as(from, "SpatialPixels")
  spacetime::STFDF(sp, time, data, endTime = endTime)
}
setAs("SpatialPixelsMeteorology", "STFDF", as.SpPxMet.STFDF)

as.SpPxMet.SpPtsMet = function(from) {
  points = as(from, "SpatialPoints")
  dates = from@dates
  indices = from@grid.index
  npoints = length(indices)
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
setAs("SpatialPixelsMeteorology", "SpatialPointsMeteorology", as.SpPxMet.SpPtsMet)

as.SpPxMet.stars = function(from) {
  stars::st_as_stars(as.SpPxMet.STFDF(from))
}
setAs("SpatialPixelsMeteorology", "stars", as.SpPxMet.stars)
