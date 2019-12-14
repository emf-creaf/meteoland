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
readmeteorologygrid(file, dates = NULL, format = "netCDF")
readmeteorologypixels(file, dates = NULL, format = "netCDF")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{A string of the file name to be read.}
  \item{dates}{A character or Date vector to specify dates to be read.}
  \item{format}{Format of meteorological data. Currently, the only accepted format is \code{"netCDF"}.}
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
