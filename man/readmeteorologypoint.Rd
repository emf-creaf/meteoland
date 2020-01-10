\encoding{UTF-8}
\name{readmeteorologypoint}
\alias{readmeteorologypoint}
\alias{readmeteorologypointfiles}
\alias{readmeteorologypoints}
\title{
Reads point meteorology from the disk
}
\description{
Functions to read point meteorological data from the disks in different formats.
}
\usage{
readmeteorologypoint(file, dates = NULL, format = "meteoland/txt", sep="\t")
readmeteorologypointfiles(points, files=NULL, dates = NULL, format="meteoland/txt", 
                          sep="\t")
readmeteorologypoints(files, dates = NULL, stations = NULL, format = "netCDF", 
                      varmapping = NULL, verbose = FALSE)
}
\arguments{
  \item{file}{A string of the file to be read.}
  \item{points}{An object of class \code{\link{SpatialPoints-class}} (in this case \code{files} cannot be \code{NULL}) or object of class \code{\link{SpatialPointsDataFrame-class}} with two data columns: '\code{dir}' and '\code{filename}' (and possibly '\code{format}').}
  \item{files}{A vector of strings to be read (when \code{points} is of class \code{\link{SpatialPoints-class}}). Length and order must match \code{points}.}
  \item{dates}{Object of class \code{"Date"} describing a subset of dates to be extracted from meteorological series. If \code{NULL} the whole period read from files is kept.}
  \item{format}{Format of meteorological data. Current accepted formats for \code{readmeteorologypoint} and \code{readmeteorologypointfiles} are \code{"meteoland/txt"},  \code{"meteoland/rds"}, \code{"castanea/txt"} and \code{"castanea/rds"}. The only accepted format for \code{readmeteorologypoints} is \code{"netCDF"}.}
  \item{sep}{The field separator character for ascii text files (see \code{\link{read.table}}).}
  \item{stations}{An integer vector or string vector identifying point indices or station names in the netCDF.}
  \item{varmapping}{Named character vector specifying a mapping of variables in the NetCDF into variables used in meteoland (e.g. \code{c(MinTemperature = "tmn")} specifies a map of variable 'tmn' to MinTemperature).}
  \item{verbose}{A logical flag to output process information in the console.}
}
\details{
Function \code{readmeteorologypoint} reads data series of a single location from an ascii or rds file and returns a data frame. Function \code{readmeteorologypointfiles} can be used to read multiple ascii/rds files and build an object of \code{\link{SpatialPointsMeteorology-class}}. This is done by supplying an \code{points} object of class \code{SpatialPointsDataFrame-class} with point meta data. In \code{readmeteorologypointfiles} the value of \code{format} is used as default but can be overloaded if \code{points} includes a column '\code{format}'. 

Function \code{readmeteorologypoints} is used to read multiple point data from a netCDF. In this case, a mapping can be supplied to map variable names in the netCDF to variables used in meteoland. 
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC

Nicolas Martin, INRA-Avignon
}

\seealso{
\code{\link{writemeteorologypoint}}, \code{\link{read.table}}, \code{\link{SpatialPointsMeteorology-class}}
}
