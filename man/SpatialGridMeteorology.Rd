\encoding{UTF-8}
\name{SpatialGridMeteorology}
\alias{SpatialGridMeteorology}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a 'SpatialGridMeteorology'
}
\description{
Initializes an object of class \code{SpatialGridMeteorology-class}
}
\usage{
SpatialGridMeteorology(grid, proj4string=CRS(as.character(NA)), data, dates)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{An object of class \code{\link{GridTopology-class}}}
  \item{proj4string}{Object of class \code{"CRS"} with the projection string.}
  \item{data}{A vector of data frames (one per date).}
  \item{dates}{Object of class \code{"Date"} describing the time period of meteorological estimates.}
}
\value{An object of class \code{\link{SpatialGridMeteorology-class}}}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

\seealso{
\code{\link{SpatialGridMeteorology-class}}
}
