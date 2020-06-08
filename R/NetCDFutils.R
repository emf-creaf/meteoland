readNetCDFpoints<-function(file) {
  ncin = .openreadpointsNetCDF(file)
  crs <- .readCRSNetCDF(ncin)
  cc <- .readpointcoordinatesNetCDF(ncin, crs)
  .closeNetCDF(file,ncin)
  return(SpatialPoints(cc, crs))
}
readNetCDFdates<-function(file) {
  ncin = .openreadpointsNetCDF(file)
  dates <- .readdatesNetCDF(ncin)
  .closeNetCDF(file,ncin)
  return(dates)
}
readNetCDFgridtopology<-function(file) {
  ncin = .openreadpointsNetCDF(file)
  gt <- .readgridtopologyNetCDF(ncin)
  .closeNetCDF(file,ncin)
  return(gt)
}
readNetCDFproj4string<-function(file) {
  ncin = .openreadpointsNetCDF(file)
  crs <- .readCRSNetCDF(ncin)
  .closeNetCDF(file,ncin)
  return(crs)
}