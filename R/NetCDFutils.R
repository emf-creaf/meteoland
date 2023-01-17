#' Utility functions for NetCDFs
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Functions to read spatial and temporal coordinates from NetCDFs.
#' 
#' 
#' @aliases readNetCDFdates readNetCDFpoints readNetCDFgridtopology
#' readNetCDFproj4string
#' @param file String of the NetCDF whose spatial/temporal coordinates are
#' desired.
#' @return \itemize{ \item{Function \code{readNetCDFdates} returns a
#' \code{\link{Date}} vector.} \item{Function \code{readNetCDFpoints} returns an
#' object \code{\link{SpatialPoints-class}}.} \item{Function
#' \code{readNetCDFgridtopology} returns an object
#' \code{\link{GridTopology-class}}.} \item{Function \code{readNetCDFproj4string}
#' returns an object \code{\link{CRS-class}}.} }
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{readmeteorologypoints}},
#' \code{\link{readmeteorologygrid}}
#' @export
readNetCDFpoints<-function(file) {

  lifecycle::deprecate_warn(
    when = "2.0.0", what = "readNetCDFpoints()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  ncin = .openreadpointsNetCDF(file)
  crs <- .readCRSNetCDF(ncin)
  cc <- .readpointcoordinatesNetCDF(ncin, crs)
  .closeNetCDF(file,ncin)
  return(SpatialPoints(cc, crs))
}
#' @describeIn readNetCDFpoints `r lifecycle::badge("deprecated")`
#' @export
readNetCDFdates<-function(file) {

  lifecycle::deprecate_warn(
    when = "2.0.0", what = "readNetCDFdates()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  ncin = .openreadpointsNetCDF(file)
  dates <- .readdatesNetCDF(ncin)
  .closeNetCDF(file,ncin)
  return(dates)
}
#' @describeIn readNetCDFpoints `r lifecycle::badge("deprecated")`
#' @export
readNetCDFgridtopology<-function(file) {

  lifecycle::deprecate_warn(
    when = "2.0.0", what = "readNetCDFgridtopology()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  ncin = .openreadpointsNetCDF(file)
  gt <- .readgridtopologyNetCDF(ncin)
  .closeNetCDF(file,ncin)
  return(gt)
}
#' @describeIn readNetCDFpoints `r lifecycle::badge("deprecated")`
#' @export
readNetCDFproj4string<-function(file) {

  lifecycle::deprecate_warn(
    when = "2.0.0", what = "readNetCDFproj4string()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  ncin = .openreadpointsNetCDF(file)
  crs <- .readCRSNetCDF(ncin)
  .closeNetCDF(file,ncin)
  return(crs)
}
