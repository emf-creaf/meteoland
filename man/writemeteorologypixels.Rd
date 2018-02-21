\encoding{UTF-8}
\name{writemeteorologypixels}
\alias{writemeteorologypixels}
\alias{writemeteorologypixelsfiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Writes pixel meteorology to the disk
}
\description{
Functions to write meteorological data located on pixels of a grid onto the file system.
}
\usage{
writemeteorologypixels(object, date, file, format = "netCDF")
writemeteorologypixelsfiles(object, dir=getwd(), format = "netCDF",
                          metadatafile = "MG.txt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{SpatialPixelsMeteorology-class}} with the meteorological data to be written.}
  \item{date}{A \code{\link{Date}} object or a character string indicating the date of the meteorological data to be written.}
  \item{file}{A string with the file name to be written.}
  \item{format}{Format of meteorological data. The only  accepted format is \code{"netCDF"}.}
  \item{dir}{Output directory for meteorology data.}
  \item{metadatafile}{The name of the file that will store the meta data describing all written files.}
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya
}

\seealso{
\code{\link{readmeteorologypixels}}, \code{\link{SpatialPixelsMeteorology-class}}
}
