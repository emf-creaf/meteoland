#' Writes grid meteorology to the disk
#' 
#' Functions to write grid meteorological data to the file system.
#' 
#' Functions \code{writemeteorologygrid} and \code{writemeteorologypixels}
#' writes grided meteorological data (i.e. class
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
#' pixels. This forces a different (suposedly) more efficient chunking based on
#' time series data instead of daily grids.
#' @param chunksizes Specifies the size of data chunks to be read/written. If
#' set, this must be a vector of three integers corresponding to XYT.
#' @param add Boolean flag to indicate that NetCDF exists and data should be
#' added/replaced.
#' @param grid An object of class \code{\link{GridTopology-class}}
#' @param proj4string Object of class \code{\linkS4class{CRS}}.
#' @param overwrite Boolean flag to force overwriting an existing NetCDF.
#' @param verbose A logical flag to output process information in the console.
#' @param index Integer indicating the grid index position to be written.
#' @param data A data frame with meteorological data corresponding to a pixel.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{readmeteorologygrid}},
#' \code{\link{SpatialGridMeteorology-class}}