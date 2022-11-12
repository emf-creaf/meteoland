readNetCDFpoints<-function(file) {

  lifecycle::deprecate_warn(
    when = "1.1.0", what = "readNetCDFpoints()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  ncin = .openreadpointsNetCDF(file)
  crs <- .readCRSNetCDF(ncin)
  cc <- .readpointcoordinatesNetCDF(ncin, crs)
  .closeNetCDF(file,ncin)
  return(SpatialPoints(cc, crs))
}
readNetCDFdates<-function(file) {

  lifecycle::deprecate_warn(
    when = "1.1.0", what = "readNetCDFdates()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  ncin = .openreadpointsNetCDF(file)
  dates <- .readdatesNetCDF(ncin)
  .closeNetCDF(file,ncin)
  return(dates)
}
readNetCDFgridtopology<-function(file) {

  lifecycle::deprecate_warn(
    when = "1.1.0", what = "readNetCDFgridtopology()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  ncin = .openreadpointsNetCDF(file)
  gt <- .readgridtopologyNetCDF(ncin)
  .closeNetCDF(file,ncin)
  return(gt)
}
readNetCDFproj4string<-function(file) {

  lifecycle::deprecate_warn(
    when = "1.1.0", what = "readNetCDFproj4string()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  ncin = .openreadpointsNetCDF(file)
  crs <- .readCRSNetCDF(ncin)
  .closeNetCDF(file,ncin)
  return(crs)
}
