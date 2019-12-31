\encoding{UTF-8}
\name{readmeteorologygrid}
\alias{readmeteorologygrid}
\alias{readmeteorologypixels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads grid meteorology from the disk
}
\description{
Functions to read grid meteorological data from the disk.
}
\usage{
readmeteorologygrid(file, format = "netCDF", varmapping = NULL,
                    dates = NULL, bbox = NULL, offset = 0)
readmeteorologypixels(file, format = "netCDF", varmapping = NULL,
                      dates = NULL, bbox = NULL, offset = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{A string of the file name to be read.}
  \item{format}{Format of meteorological data. Currently, the only accepted format is \code{"netCDF"}.}
  \item{varmapping}{Named character vector specifying a mapping of variables in the NetCDF into variables used in meteoland (e.g. \code{c(MinTemperature = "tmn")} specifies a map of variable 'tmn' to MinTemperature).}
  \item{dates}{A character or Date vector to specify subset of dates to be read.}
  \item{bbox}{Boundary box (2 x 2 matrix) specifying the minimum and maximum coordinates of a study area.}
  \item{offset}{A buffer to include NetCDF cells that are at a certain distance around the boundary box.}
}
\details{
  Function \code{readmeteorologygrid} reads a file containing the meteorology over a grid for a set of days. Function \code{readmeteorologypixels} reads a file containing the meteorology over a grid for a set of days and filters those pixels with missing data. 
}
\value{
Function \code{readmeteorologygridfiles} returns an object \code{\link{SpatialGridMeteorology-class}}. Function \code{readmeteorologypixels} returns an object \code{\link{SpatialPixelsMeteorology-class}}.
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}

\seealso{
\code{\link{writemeteorologygrid}}, \code{\link{writemeteorologypixels}}, \code{\link{SpatialGridMeteorology-class}}, \code{\link{SpatialPixelsMeteorology-class}}
}
