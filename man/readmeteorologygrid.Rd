\encoding{UTF-8}
\name{readmeteorologygrid}
\alias{readmeteorologygrid}
\alias{readmeteorologygridfiles}
\alias{readmeteorologygridcells}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads grid meteorology from the disk
}
\description{
Functions to read grid meteorological data from the disk.
}
\usage{
readmeteorologygrid(file, format = "netCDF")
readmeteorologygridfiles(files, format = "netCDF")
readmeteorologygridcells(files, cellIndices, format = "netCDF")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{A string of the file name to be read.}
  \item{format}{Format of meteorological data. The only  accepted format is \code{"netCDF"}.}
  \item{files}{Either a vector of filename strings or a data frame with two columns: 'dir' and 'filename.}
  \item{cellIndices}{An integer vector with grid cell indices indicating the cells for which meteorological data has to be read.}
}
\details{
  Function \code{readmeteorologygrid} reads a file containing the meteorology over a grid for a single day. Function \code{readmeteorologygridfiles} reads several files, each containing the meteorology over the same grid for a different day. Function \code{readmeteorologygridcell} also reads several grid meteorology files, but it keeps the meteorology of a set of cells only.
}
\value{
Function \code{readmeteorologygrid} returns an object \code{\link{SpatialGridDataFrame-class}} where the data frame has grid cells as rows and meteorological variables as columns. Function \code{readmeteorologygrid} returns an object \code{\link{SpatialGridMeteorology-class}} and \code{readmeteorologygridcells} returns an object \code{\link{SpatialPointsMeteorology-class}}.
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya
}

\seealso{
\code{\link{writemeteorologygrid}}, \code{\link{SpatialGridMeteorology-class}}
}
