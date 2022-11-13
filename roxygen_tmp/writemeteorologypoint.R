#' Writes point meteorology to the disk
#' 
#' Functions to write point meteorological data to disk files in different
#' formats.
#' 
#' Function \code{writemeteorologypoint} writes data series of a single
#' location (i.e. a data frame) to ascii or rds files. Function
#' \code{writemeteorologypointfiles} takes an object of
#' \code{\link{SpatialPointsMeteorology-class}} and writes one file for each
#' data point in the same formats as \code{writemeteorologypoint}. In addition,
#' it writes a metadata file (see argument \code{metadataFile}) with point
#' coordinates, station names and file names. Both functions share the same
#' accepted formats, which are \code{"meteoland/txt"}, \code{"meteoland/rds"},
#' \code{"castanea/txt"} and \code{"castanea/rds"}.
#' 
#' Function \code{writemeteorologypoints} takes an object of
#' \code{\link{SpatialPointsMeteorology-class}} and writes all its content in a
#' single file (i.e. a netCDF). The same function can be used to replace data
#' from an existing file or to add new points to the netCDF. This is done by
#' using \code{add=TRUE}, and profits from the fact that some netCDF dimensions
#' (in this case the number of points) can be defined as unlimited. If data is
#' added to an existing netCDF, the coordinate reference system and the dates
#' of \code{object} must be the same as those in the netCDF.
#' 
#' @aliases writemeteorologypoint writemeteorologypoints
#' writemeteorologypointfiles
#' @param data An data frame with meteorological data.
#' @param file A string with the file name to be written.
#' @param format Output format of meteorological data. Current accepted formats
#' are \code{"meteoland/txt"}, \code{"meteoland/rds"}, \code{"castanea/txt"}
#' and \code{"castanea/rds"}.
#' @param object An object of class
#' \code{\link{SpatialPointsMeteorology-class}} with the meteorological data to
#' be written.
#' @param dir Output directory for meteorology data.
#' @param metadataFile The name of the file that will store the meta data
#' describing all written files.
#' @param add Boolean flag to indicate that NetCDF exists and data should be
#' added/replaced (see details).
#' @param overwrite Boolean flag to force overwriting an existing NetCDF.
#' @param verbose A logical flag to output process information in the console.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' 
#' Nicolas Martin, INRA-Avignon
#' @seealso \code{\link{readmeteorologypoint}},
#' \code{\link{SpatialPointsMeteorology-class}}