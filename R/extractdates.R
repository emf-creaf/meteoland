#' @describeIn extractdates `r lifecycle::badge("deprecated")`
#' @export
extractvars<-function(object, vars, verbose = FALSE) {
  # deprecation warning
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "extractvars()", with = NULL,
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Extraction of variables or dates can be done as in a normal data.frame as the meteo objects are now sf objects"
  )

  if((!inherits(object,"SpatialPointsMeteorology"))
     && (!inherits(object,"SpatialGridMeteorology"))
     && (!inherits(object,"SpatialPixelsMeteorology"))
     && (!inherits(object,"character")))
    stop("'object' has to be of class 'Spatial*Meteorology' of a file name.")

  for(i in 1:length(vars)) vars[i]<- match.arg(vars[i], c("MeanTemperature", "MinTemperature", "MaxTemperature",
                                                          "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
                                                          "Precipitation", "Radiation", "WindSpeed", "WindDirection", "SpecificHumidity", "PET"))
  if(inherits(object,"SpatialGridMeteorology")) {
    dates = object@dates
    dates<-as.character(dates)
    res = vector("list", length(vars))
    names(res)<-vars
    npts = nrow(coordinates(object))
    for(i in 1:length(vars)) {
      df = data.frame(row.names=1:npts)
      for(j in 1:length(dates)) {
        dfj = object@data[[j]]
        df[[dates[j]]] = dfj[[vars[i]]]
      }
      res[[i]] = SpatialGridDataFrame(object@grid, df, object@proj4string)
    }
  }
  else if(inherits(object,"SpatialPixelsMeteorology")) {
    dates = object@dates
    dates<-as.character(dates)
    res = vector("list", length(vars))
    names(res)<-vars
    npts = nrow(coordinates(object))
    for(i in 1:length(vars)) {
      df = data.frame(row.names=1:npts)
      for(j in 1:length(dates)) {
        dfj = object@data[[j]]
        df[[dates[j]]] = dfj[[vars[i]]]
      }
      res[[i]] = SpatialPixelsDataFrame(object@coords, data = df,
                                        grid = object@grid, proj4string = object@proj4string)
    }
  }
  if(inherits(object,"SpatialPointsMeteorology")) {
    dates = object@dates
    npoints = length(object@data)
    if(!is.null(names(object@data))) ids = names(object@data)
    else ids = 1:npoints
    ptsout = as(object,"SpatialPoints")

    ndates = length(dates)
    dateschar =as.character(dates)
    res = vector("list", length(vars))
    names(res)<-vars

    for(i in 1:length(vars)) {
      df = data.frame(row.names=ids)
      for(j in 1:npoints) {
        dfj = object@data[[j]]
        df[j, dateschar] = dfj[[vars[i]]]
      }
      res[[i]] = SpatialPointsDataFrame(ptsout,df)
    }
  }
  if(length(res)==1) return(res[[1]])
  return(res)
}


#' Extracts meteorological data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Extracts meteorological data from an object.
#'
#' @details
#' Function \code{extractpoints} is deprecated, because its functionality can
#' be achieved using subsetting of spatial classes
#' \code{\link{SpatialGridMeteorology}} and
#' \code{\link{SpatialPixelsMeteorology}}.
#'
#' @aliases extractdates extractvars extractgridindex extractgridpoints
#' @param object An object of class \code{\link{SpatialPointsMeteorology}},
#' \code{\link{SpatialGridMeteorology}} or
#' \code{\link{SpatialPixelsMeteorology}}.
#' @param dates A vector of \code{\link{Date}} with a (subset) of dates to be
#' extracted. If \code{NULL} all dates will be returned.
#' @param vars A character vector with the set of variables to be extracted.
#' @param grid An object of class \code{\link{SpatialGridMeteorology-class}} or
#' \code{\link{SpatialPixelsMeteorology-class}} with the meteorological data
#' for a full grid or a subset of grid cells, respectively. Alternatively, a
#' string specifying a NetCDF to be read from the disk.
#' @param index An integer with a grid index.
#' @param points An object of class \code{\link{SpatialPoints}}.
#' @param verbose Boolean flag to print process information.
#' @return \itemize{ \item{Function \code{extractdates()}, returns a \code{list}
#' with the same length as \code{dates}. Each element of the list is a spatial
#' object (\code{\link{SpatialPointsDataFrame}},
#' \code{\link{SpatialGridDataFrame}} or \code{\link{SpatialPixelsDataFrame}},
#' depending on the input) with the meteorological data for all the spatial
#' elements. If only one date is asked, the function returns directly the
#' spatial object, without embedding it into a list.} \item{Function
#' \code{extractvars()}, returns a \code{list} with the same length as
#' \code{vars}. Each element of the list is a spatial object
#' (\code{\link{SpatialPointsDataFrame}}, \code{\link{SpatialGridDataFrame}} or
#' \code{\link{SpatialPixelsDataFrame}}, depending on the input) with the
#' meteorological data for all the spatial elements. If only one variable is
#' asked, the function returns directly the spatial object, without embedding
#' it into a list.} \item{Function \code{extractgridindex()} returns a data
#' frame.} \item{Function \code{extractgridpoints()} returns an object of class
#' \code{\link{SpatialPointsMeteorology}}.} }
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @export
extractdates<-function(object, dates = NULL, verbose=FALSE) {
  lifecycle::deprecate_warn(
    when = "2.0.0", what = "extractdates()", with = NULL,
    details = "Spatial_*_Meteorology classes are soft deprecated.
    Extraction of variables or dates can be done as in a normal data.frame as the meteo objects are
    now sf objects"
  )

  if((!inherits(object,"SpatialPointsMeteorology"))
     && (!inherits(object,"SpatialGridMeteorology"))
     && (!inherits(object,"SpatialPixelsMeteorology"))
     && (!inherits(object,"character")))
    stop("'object' has to be of class 'Spatial*Meteorology' of a file name.")
  if(!is.null(dates))
    if((!inherits(dates,"Date")) && (!inherits(dates,"character")))
      stop("'dates' has to be of class 'Date' or 'character'.")
  if(inherits(object,"SpatialGridMeteorology")) {
    if(is.null(dates)) dates = object@dates
    dates<-as.character(dates)
    res = vector("list", length(dates))
    names(res)<-dates
    for(i in 1:length(dates)) {
      if(!(dates[i] %in% as.character(object@dates))) stop(" Date '",paste0(dates[i], "' not found in data."))
      res[[i]] = SpatialGridDataFrame(object@grid, object@data[[dates[i]]], object@proj4string)
    }
  }
  else if(inherits(object,"SpatialPixelsMeteorology")) {
    if(is.null(dates)) dates = object@dates
    dates<-as.character(dates)
    res = vector("list", length(dates))
    names(res)<-dates
    for(i in 1:length(dates)) {
      if(!(dates[i] %in% as.character(object@dates))) stop(" Date '",paste0(dates[i], "' not found in data."))
      res[[i]] = SpatialPixelsDataFrame(object@coords, data = object@data[[dates[i]]],
                                        grid = object@grid, proj4string = object@proj4string)
    }
  }
  if(inherits(object,"SpatialPointsMeteorology")) {
    if(is.null(dates)) dates = object@dates
    npoints = length(object)
    dfvec = vector("list",npoints)
    if(!is.null(names(object@data))) ids = names(object@data)
    else ids = 1:npoints
    ptsout = as(object,"SpatialPoints")

    npoints = length(ids)
    ndates = length(dates)
    dateschar =as.character(dates)
    if(verbose) cat(paste("  Extracting ", ndates, " dates from ", npoints," points...\n", sep=""))
    res = vector("list", ndates)

    if(verbose)  pb = txtProgressBar(0, npoints, 0, style = 3)
    for(i in 1:npoints) {
      if(verbose) setTxtProgressBar(pb, i)
      obs = object@data[[i]]
      for(d in 1:ndates) {
        if(is.null(res[[d]])) {
          res[[d]] = data.frame(matrix(NA, npoints, ncol(obs)))
          row.names(res[[d]])<-ids
          names(res[[d]])<-names(obs)
        }
        res[[d]][i,]<-obs[dateschar[d],]
      }
    }
    if(verbose) cat("\n")

    for(d in 1:ndates) {
      res[[d]] = SpatialPointsDataFrame(ptsout,res[[d]])
    }
  }
  if(length(res)==1) return(res[[1]])
  return(res)
}


