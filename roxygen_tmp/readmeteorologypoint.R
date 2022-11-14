#' Reads point meteorology from the disk
#' 
#' Functions to read point meteorological data from the disks in different
#' formats.
#' 
#' Function \code{readmeteorologypoint} reads data series of a single location
#' from an ascii or rds file and returns a data frame. Function
#' \code{readmeteorologypointfiles} can be used to read multiple ascii/rds
#' files and build an object of \code{\link{SpatialPointsMeteorology-class}}.
#' This is done by supplying an \code{points} object of class
#' \code{SpatialPointsDataFrame-class} with point meta data. In
#' \code{readmeteorologypointfiles} the value of \code{format} is used as
#' default but can be overloaded if \code{points} includes a column
#' '\code{format}'.
#' 
#' Function \code{readmeteorologypoints} is used to read multiple point data
#' from a netCDF. In this case, a mapping can be supplied to map variable names
#' in the netCDF to variables used in meteoland.
#' 
#' @aliases readmeteorologypoint readmeteorologypointfiles
#' readmeteorologypoints
#' @param file A string of the file to be read.
#' @param points An object of class \code{\link{SpatialPoints-class}} (in this
#' case \code{files} cannot be \code{NULL}) or object of class
#' \code{\link{SpatialPointsDataFrame-class}} with two data columns:
#' '\code{dir}' and '\code{filename}' (and possibly '\code{format}').
#' @param files A vector of strings to be read (when \code{points} is of class
#' \code{\link{SpatialPoints-class}}). Length and order must match
#' \code{points}.
#' @param dates Object of class \code{"Date"} describing a subset of dates to
#' be extracted from meteorological series. If \code{NULL} the whole period
#' read from files is kept.
#' @param format Format of meteorological data. Current accepted formats for
#' \code{readmeteorologypoint} and \code{readmeteorologypointfiles} are
#' \code{"meteoland/txt"}, \code{"meteoland/rds"}, \code{"castanea/txt"} and
#' \code{"castanea/rds"}. The only accepted format for
#' \code{readmeteorologypoints} is \code{"netCDF"}.
#' @param sep The field separator character for ascii text files (see
#' \code{\link{read.table}}).
#' @param stations An integer vector or string vector identifying point indices
#' or station names in the netCDF.
#' @param varmapping Named character vector specifying a mapping of variables
#' in the NetCDF into variables used in meteoland (e.g. \code{c(MinTemperature
#' = "tmn")} specifies a map of variable 'tmn' to MinTemperature).
#' @param verbose A logical flag to output process information in the console.
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' Nicolas Martin, INRA-Avignon
#' @seealso \code{\link{writemeteorologypoint}}, \code{\link{read.table}},
#' \code{\link{SpatialPointsMeteorology-class}}
