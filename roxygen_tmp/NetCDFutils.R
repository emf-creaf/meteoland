#' Utility functions for NetCDFs
#' 
#' Functions to read spatial and temporal coordinates from NetCDFs.
#' 
#' 
#' @aliases readNetCDFdates readNetCDFpoints readNetCDFgridtopology
#' readNetCDFproj4string
#' @param file String of the NetCDF whose spatial/temporal coordinates are
#' desired.
#' @return \itemize{ \itemFunction \code{readNetCDFdates} returns a
#' \code{\link{Date}} vector. \itemFunction \code{readNetCDFpoints} returns an
#' object \code{\link{SpatialPoints-class}}. \itemFunction
#' \code{readNetCDFgridtopology} returns an object
#' \code{\link{GridTopology-class}}. \itemFunction \code{readNetCDFproj4string}
#' returns an object \code{\link{CRS-class}}. }
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{readmeteorologypoints}},
#' \code{\link{readmeteorologygrid}}
