\encoding{UTF-8}
\name{SpatialPixelsMeteorology}
\alias{SpatialPixelsMeteorology}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a 'SpatialPixelsMeteorology'
}
\description{
Initializes an object of class \code{SpatialPixelsMeteorology-class}
}
\usage{
SpatialPixelsMeteorology(points, data, dates,
                        tolerance = sqrt(.Machine$double.eps), 
                        proj4string = CRS(as.character(NA)), round = NULL, 
                        grid = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points}{An object of class \code{\link{SpatialPoints-class}}.}
  \item{data}{A vector of data frames (one per date).}
  \item{dates}{Object of class \code{"Date"} describing the time period of meteorological estimates.}
  \item{tolerance}{Precision up to which extent points should be exactly on a grid.}
  \item{proj4string}{Object of class \code{\linkS4class{CRS}} in the first form only used when points does not inherit from \code{\linkS4class{Spatial}}.}
  \item{round}{default \code{NULL}, otherwise a value passed to as the digits argument to \code{\link{round}} for setting cell size.}
  \item{grid}{Grid topology using an object of class \code{\linkS4class{GridTopology}}; a value of \code{NULL} implies that this will be derived from the point coordinates.}
}
\value{An object of class \code{\link{SpatialPixelsMeteorology-class}}}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya
}

\seealso{
\code{\link{SpatialPixelsMeteorology-class}}
}
