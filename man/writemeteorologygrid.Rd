\encoding{UTF-8}
\name{writemeteorologygrid}
\alias{writemeteorologygrid}
\alias{writemeteorologypixels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Writes grid meteorology to the disk
}
\description{
Functions to write grid meteorological data to the file system.
}
\usage{
writemeteorologygrid(object, file, dates, format = "netCDF", 
                     add = FALSE, overwrite = FALSE)
writemeteorologypixels(object, file, dates, format = "netCDF", 
                       add = FALSE, overwrite = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{SpatialGridMeteorology-class}} or class \code{\link{SpatialPixelsMeteorology-class}} with the meteorological data to be written.}
  \item{file}{A string with the file name to be written.}
  \item{dates}{A \code{\link{Date}} vector object or a character string indicating the dates to be written.}
  \item{format}{Format of meteorological data. The only  accepted format is \code{"netCDF"}.}
  \item{add}{Boolean flag to indicate that NetCDF exists and data should be added/replaced.}
  \item{overwrite}{Boolean flag to force overwriting an existing NetCDF.}
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}

\seealso{
\code{\link{readmeteorologygrid}}, \code{\link{SpatialGridMeteorology-class}}
}
