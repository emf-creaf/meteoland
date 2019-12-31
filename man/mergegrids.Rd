\encoding{UTF-8}
\name{mergegrids}
\alias{mergegrids}
\title{
Merges gridded data
}
\description{
Merges gridded meteorological data into a single object
}
\usage{
mergegrids(..., verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{Objects to be merged, either of class \code{SpatialGridMeteorology} or \code{SpatialPixelsMeteorology}. All objects of the same class.}
  \item{verbose}{A logical flag to indicate console output.}
}
\details{
  The function pools dates and variables. However, spatial structures (i.e. grid topology, pixel indices, reference system, ...) should be the same for all objects to be merged.
}
\value{
An object \code{\link{SpatialGridMeteorology-class}} or an object \code{\link{SpatialPixelsMeteorology-class}}, depending on the input.
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}

\seealso{
\code{\link{readmeteorologygrid}}, \code{\link{SpatialGridMeteorology-class}}, \code{\link{SpatialPixelsMeteorology-class}}
}
