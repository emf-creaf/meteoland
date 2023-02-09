#' Creates a 'SpatialPointsMeteorology'
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Initializes an object of class \code{SpatialPointsMeteorology-class}
#' 
#' @details
#' There are two ways of building an object of of class
#' \code{\link{SpatialPointsMeteorology-class}}. The first way
#' (\code{dataByDate = FALSE}) is to supply as value for \code{data} a vector
#' of data frames with one data frame per spatial point, with dates as rows and
#' meteorological variables as columns. In this case all data frames must have
#' the same number of rows (dates) and columns (variables). The second way (if
#' \code{dataByDate = TRUE}) is to supply as value for \code{data} a vector of
#' data frames with one data frame per date, with points as rows and
#' meteorological variables as columns. In this case, the data frames may have
#' different rows and different columns. Only the information corresponding to
#' \code{points} will be taken and some variables may be missing.
#' 
#' @param points An object of class \code{\link{SpatialPoints-class}}. Row
#' names of point coordinates are used to identify points.
#' @param data A list of data frames. If \code{dataByDate = FALSE} the elements
#' of \code{data} are assumed to correspond to points. If \code{dataByDate =
#' TRUE} the elements of \code{data} are assumed to correspond to dates (see
#' 'Details').
#' @param dates Object of class \code{"Date"} describing the time period of
#' meteorological estimates.
#' @param dataByDate A flag to indicate that elements of \code{data} correspond
#' to dates, as opposed to the default (\code{dataByDate = FALSE}) which
#' assumes that elements correspond to points (see 'Details').
#' @return An object of class \code{\link{SpatialPointsMeteorology-class}}
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{SpatialPointsMeteorology-class}}
#' @export
SpatialPointsMeteorology<-function(points, data, dates, dataByDate = FALSE) {
  
  # deprecation notice
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "SpatialPointsMeteorology()", with = NULL,
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Meteorology objects are now normal sf points objects"
  )
  
  if(!inherits(points, "SpatialPoints")) stop("'points' has to be of class 'SpatialPoints'")
  if(!inherits(dates, "Date")) stop("'dates' has to be of class 'Date'")
  if(!is.list(data)) stop("'data' has to be a list of data frames")
  if(is.null(rownames(points@coords))) rownames(points@coords) = 1:nrow(points@coords)
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

#' @export
setMethod("[", signature("SpatialPointsMeteorology"),definition =
            function (x, i, j, ..., drop = TRUE) 
            {
              if (!missing(j)) 
                warning("j index ignored")
              if (is.character(i)) 
                i <- match(i, row.names(x))
              else if (is(i, "Spatial")) 
                i = !is.na(over(x, geometry(i)))
              if (any(is.na(i))) 
                stop("NAs not permitted in row index")
              sp = as(x,"SpatialPoints")[i, , drop=drop]
              SpatialPointsMeteorology(sp, x@data[i], x@dates)
            }
)

head.SpatialPointsMeteorology <- function(x, n=6L, ...) {
  n <- min(n, length(x))
  ix <- sign(n)*seq(abs(n))
  x[ ix , , drop=FALSE]
}

#' @export
setMethod("head", "SpatialPointsMeteorology", function(x, n=6L, ...) head.SpatialPointsMeteorology(x,n,...))

tail.SpatialPointsMeteorology <- function(x, n=6L, ...) {
  n <- min(n, length(x))
  ix <- sign(n)*rev(seq(length(x), by=-1L, len=abs(n)))
  x[ ix , , drop=FALSE]
}

#' @export
setMethod("tail", "SpatialPointsMeteorology", function(x, n=6L, ...) tail.SpatialPointsMeteorology(x,n,...))

print.SpatialPointsMeteorology <- function(x, ..., digits = getOption("digits"))
{
  cat("Object of class SpatialPointsMeteorology\n")
  cat("Dates: ", paste0(length(x@dates)))
  cat(paste0("  (initial: ", x@dates[1], " final: ", x@dates[length(x@dates)],")\n"))
  cat("SpatialPoints:\n")
  print(x@coords)
  pst <- paste(strwrap(paste(
    "Coordinate Reference System (CRS) arguments:", 
    proj4string(x))), collapse="\n")
  cat(pst, "\n")
}

#' @export
setMethod("print", "SpatialPointsMeteorology", function(x, ..., digits = getOption("digits")) print.SpatialPointsMeteorology(x, ..., digits))

#' @export
setMethod("show", "SpatialPointsMeteorology", function(object) print.SpatialPointsMeteorology(object))

as.SpPtsMet.STFDF = function(from) {
  
  datavec = from@data
  data = datavec[[1]]
  if(length(datavec)>1) {
    for(i in 2:length(datavec)) {
      data = rbind(data, datavec[[i]]) 
    }
  }
  time = as.POSIXct(as.Date(from@dates))
  endTime = as.POSIXct(as.Date(from@dates)+1)
  sp = as(from, "SpatialPoints")
  spacetime::STFDF(sp, time, data, endTime = endTime)
}
setAs("SpatialPointsMeteorology", "STFDF", as.SpPtsMet.STFDF)
as.SpPtsMet.stars = function(from) {
  stars::st_as_stars(as.SpPtsMet.STFDF(from))
}
setAs("SpatialPointsMeteorology", "stars", as.SpPtsMet.stars)
