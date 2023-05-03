# Writes grid meteorological data (SpatialGridMeteorology) into a netCDF


#' Writes grid meteorology to the disk
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Functions to write grid meteorological data to the file system.
#'
#' @details
#' Functions \code{writemeteorologygrid} and \code{writemeteorologypixels}
#' writes gridded meteorological data (i.e. class
#' \code{\link{SpatialGridMeteorology-class}} or class
#' \code{\link{SpatialPixelsMeteorology-class}}, respectively) into a netCDF,
#' with the possibility to overwrite existing data. Function
#' \code{writemeteorologygridpixel} is meant to add/replace data in a netCDF
#' corresponding to a specific pixel identified by its grid index. Function
#' \code{writemeteorologygrid} creates an empty netCDF with the specified grid
#' dimensions, coordinate reference system and dates.
#'
#' @aliases writemeteorologygrid writeemptymeteorologygrid
#' writemeteorologygridpixel writemeteorologypixels
#' @param object An object of class \code{\link{SpatialGridMeteorology-class}}
#' or class \code{\link{SpatialPixelsMeteorology-class}} with the
#' meteorological data to be written.
#' @param file A string with the file name to be written.
#' @param dates A \code{\link{Date}} vector object or a character string
#' indicating the dates to be written.
#' @param format Format of meteorological data. The only accepted format is
#' \code{"netCDF"}.
#' @param byPixel Boolean flag to specify whether file will be written/read by
#' pixels. This forces a different (supposedly) more efficient chunking based on
#' time series data instead of daily grids.
#' @param chunksizes Specifies the size of data chunks to be read/written. If
#' set, this must be a vector of three integers corresponding to XYT.
#' @param add Boolean flag to indicate that NetCDF exists and data should be
#' added/replaced.
#' @param grid An object of class \code{GridTopology}
#' @param proj4string Object of class \code{CRS}.
#' @param overwrite Boolean flag to force overwriting an existing NetCDF.
#' @param verbose A logical flag to output process information in the console.
#' @param index Integer indicating the grid index position to be written.
#' @param data A data frame with meteorological data corresponding to a pixel.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{readmeteorologygrid}},
#' \code{\link{SpatialGridMeteorology-class}}
#' @export
writemeteorologygrid<-function(object, file, dates = NULL, format = "netCDF",
                               byPixel = FALSE, chunksizes = NA,
                               add=FALSE, overwrite = FALSE, verbose = FALSE) {
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "writemeteorologygrid()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  # if(!inherits(object,"SpatialGridMeteorology")) stop("'object' has to be of class 'SpatialGridMeteorology'.")
  # if(is.null(dates)) dates = object@dates
  # if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  # vars = NULL
  # if(length(dates)>0) vars = names(object@data[[1]])
  # nc = .openwritegridNetCDF(object@grid, proj4string = proj4string(object), dates = dates, vars = vars,
  #                           file=file, byPixel = byPixel, chunksizes = chunksizes,
  #                           add= add, overwrite = overwrite, verbose = verbose)
  # if(length(dates)>0) .writemeteorologygridNetCDF(data = object@data[as.character(dates)],
  #                             grid=object@grid, proj4string = proj4string(object),
  #                             nc=nc, byPixel = byPixel, verbose = verbose)
  # .closeNetCDF(file,nc, verbose = verbose)
}
# Writes grid meteorological data (SpatialPixelMeteorology) into a netCDF
#' @describeIn writemeteorologygrid `r lifecycle::badge("deprecated")`
#' @export
writemeteorologypixels<-function(object, file, dates = NULL, format = "netCDF",
                                 byPixel = FALSE, chunksizes = NA,
                                 add=FALSE, overwrite = FALSE, verbose = FALSE) {
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "writemeteorologypixels()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  # if(!inherits(object,"SpatialPixelsMeteorology")) stop("'object' has to be of class 'SpatialPixelsMeteorology'.")
  # if(is.null(dates)) dates = object@dates
  # if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  # vars = NULL
  # if(length(dates)>0) vars = names(object@data[[1]])
  # nc = .openwritegridNetCDF(object@grid, proj4string = proj4string(object), dates = dates, vars = vars,
  #                                 file=file, byPixel = byPixel, chunksizes = chunksizes, add= add, overwrite = overwrite, verbose = verbose)
  # if(length(dates)>0) .writemeteorologypixelsNetCDF(data = object@data[as.character(dates)],
  #                               pixels=object, proj4string = proj4string(object),
  #                               byPixel = byPixel, nc=nc)
  # .closeNetCDF(file,nc, verbose = verbose)
}
#' @describeIn writemeteorologygrid `r lifecycle::badge("deprecated")`
#' @export
writeemptymeteorologygrid<-function(file, grid, proj4string, dates, byPixel = FALSE, chunksizes = NA, overwrite = FALSE, verbose = FALSE) {
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "writeemptymeteorologygrid()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  # if((!inherits(dates,"Date"))&&(!inherits(dates,"character"))) stop("'dates' must be a 'character' or 'Date'")
  # nc = .openwritegridNetCDF(grid, proj4string = proj4string, dates = dates, vars = NULL,
  #                           file=file, byPixel = byPixel, chunksizes = chunksizes,
  #                           add= FALSE, overwrite = overwrite, verbose = verbose)
  # .closeNetCDF(file,nc, verbose = verbose)
}

# Replaces the content of a grid pixel in an existing netCDF
#' @describeIn writemeteorologygrid `r lifecycle::badge("deprecated")`
#' @export
writemeteorologygridpixel<-function(file, index, data, verbose = FALSE) {
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "writemeteorologygridpixel()", with = NULL,
    details = "meteoland NetCDF utils are deprecated.
    NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars...)"
  )

  # nc = .openaddNetCDF(file=file, verbose=verbose)
  # gt = .readgridtopologyNetCDF(nc)
  # gdates = .readdatesNetCDF(nc)
  # ny = gt@cells.dim[2]
  # cv = coordinatevalues(gt)
  # cci = coordinates(gt)[index,]
  # i = which(cv[[1]]==cci[1])
  # j = which(cv[[2]]==cci[2])
  # dates = as.Date(row.names(data))
  # t = which(as.character(gdates)==as.character(dates[1]))
  # .putgridpixel(nc,ny, i,j,t,data)
  # .closeNetCDF(file, nc, verbose=verbose)
}
