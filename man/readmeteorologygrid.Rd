\encoding{UTF-8}
\name{readmeteorologygrid}
\alias{readmeteorologygrid}
\alias{readmeteorologypixels}
\alias{readmeteorologygridpoints}
\title{
Reads gridded meteorology from the disk
}
\description{
Functions to read gridded meteorological data from the disk.
}
\usage{
readmeteorologygrid(files, format = "netCDF", varmapping = NULL,
                    dates = NULL, bbox = NULL, offset = 0, verbose = FALSE)
readmeteorologypixels(files, format = "netCDF", varmapping = NULL,
                      dates = NULL, bbox = NULL, offset = 0, verbose = FALSE)
readmeteorologygridpoints(files, format = "netCDF", varmapping = NULL,
                      dates = NULL, bbox = NULL, offset = 0, 
                      relativehumidity = FALSE, verbose = FALSE)
}
\arguments{
  \item{files}{Character vector with the file names to be read.}
  \item{format}{Format of meteorological data. Currently, the only accepted format is \code{"netCDF"}.}
  \item{varmapping}{Named character vector specifying a mapping of variables in the NetCDF into variables used in meteoland (e.g. \code{c(MinTemperature = "tmn")} specifies a map of variable 'tmn' to MinTemperature).}
  \item{dates}{A character or Date vector to specify subset of dates to be read.}
  \item{bbox}{Boundary box (2 x 2 matrix) specifying the minimum and maximum coordinates of a study area.}
  \item{offset}{A buffer to include NetCDF cells that are at a certain distance around the boundary box.}
  \item{relativehumidity}{A logical flag to indicate estimation of relative humidity from specific humidity if possible.}
  \item{verbose}{A logical flag to output process information in the console.}
}
\details{
  Function \code{readmeteorologygrid} reads one or several files containing the meteorology over a grid for a set of days. Function \code{readmeteorologypixels} reads one or several file containing the meteorology over a grid for a set of days and filters those pixels with missing data. If more than one file is specificated, the functions read all of them and then try to merge the data into a single meteorology object (see function \code{\link{mergegrids}}).
  
  Function \code{readmeteorologygridpoints} is similar to the preceding ones, but is meant to extract specific grid pixels and return them as spatial points. If more than one file is specificated, the function reads all of them and then tries to merge the data into a single meteorology object (see function \code{\link{mergepoints}}).
  
  The functions are primarily meant to read NetCDF written by package meteoland, but also to import data written by other software. In this case, a mapping can be supplied to map variable names in the netCDF to variables used in meteoland. Rotated grids should not be read using functions \code{readmeteorologygrid} or \code{readmeteorologypixels}.
}
\value{
\itemize{
\item{Function \code{readmeteorologygrid} returns an object \code{\link{SpatialGridMeteorology-class}}.}
\item{Function \code{readmeteorologypixels} returns an object \code{\link{SpatialPixelsMeteorology-class}}.} \item{Function \code{readmeteorologygridpoints} returns an object \code{\link{SpatialPointsMeteorology-class}}.}
}
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}

\seealso{
\code{\link{writemeteorologygrid}}, \code{\link{writemeteorologypixels}}, \code{\link{SpatialPointsMeteorology-class}}, \code{\link{SpatialGridMeteorology-class}}, \code{\link{SpatialPixelsMeteorology-class}}, \code{\link{mergegrids}}, \code{\link{mergepoints}}
}
