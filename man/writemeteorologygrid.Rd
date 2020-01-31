\encoding{UTF-8}
\name{writemeteorologygrid}
\alias{writemeteorologygrid}
\alias{writeemptymeteorologygrid}
\alias{writemeteorologygridpixel}
\alias{writemeteorologypixels}
\title{
Writes grid meteorology to the disk
}
\description{
Functions to write grid meteorological data to the file system.
}
\usage{
writemeteorologygrid(object, file, dates = NULL, format = "netCDF", 
                     byPixel = FALSE, chunksizes = NA, add = FALSE, overwrite = FALSE, 
                     verbose = FALSE)
writeemptymeteorologygrid(file, grid, proj4string, dates, 
                     byPixel = FALSE, chunksizes = NA, overwrite = FALSE, 
                     verbose = FALSE)
writemeteorologypixels(object, file, dates = NULL, format = "netCDF", 
                       byPixel = FALSE, chunksizes = NA, add = FALSE, overwrite = FALSE, 
                       verbose = FALSE)
writemeteorologygridpixel(file, index, data, verbose = FALSE)
}
\arguments{
  \item{object}{An object of class \code{\link{SpatialGridMeteorology-class}} or class \code{\link{SpatialPixelsMeteorology-class}} with the meteorological data to be written.}
  \item{file}{A string with the file name to be written.}
  \item{dates}{A \code{\link{Date}} vector object or a character string indicating the dates to be written.}
  \item{format}{Format of meteorological data. The only  accepted format is \code{"netCDF"}.}
  \item{byPixel}{Boolean flag to specify whether file will be written/read by pixels. This forces a different (suposedly) more efficient chunking based on time series data instead of daily grids.}
  \item{chunksizes}{Specifies the size of data chunks to be read/written. If set, this must be a vector of three integers corresponding to XYT.}
  \item{add}{Boolean flag to indicate that NetCDF exists and data should be added/replaced.}
  \item{grid}{An object of class \code{\link{GridTopology-class}}}
  \item{proj4string}{Object of class \code{\linkS4class{CRS}}.}
  \item{overwrite}{Boolean flag to force overwriting an existing NetCDF.}
  \item{verbose}{A logical flag to output process information in the console.}
  \item{index}{Integer indicating the grid index position to be written.}
  \item{data}{A data frame with meteorological data corresponding to a pixel.}
}
\details{
Functions \code{writemeteorologygrid} and \code{writemeteorologypixels} writes grided meteorological data (i.e. class \code{\link{SpatialGridMeteorology-class}} or class \code{\link{SpatialPixelsMeteorology-class}}, respectively) into a netCDF, with the possibility to overwrite existing data. Function \code{writemeteorologygridpixel} is meant to add/replace data in a netCDF corresponding to a specific pixel identified by its grid index. Function \code{writemeteorologygrid} creates an empty netCDF with the specified grid dimensions, coordinate reference system and dates.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}

\seealso{
\code{\link{readmeteorologygrid}}, \code{\link{SpatialGridMeteorology-class}}
}
