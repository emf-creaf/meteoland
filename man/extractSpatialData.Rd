\encoding{UTF-8}
\name{extractSpatialData}
\alias{extractSpatialData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracts spatial data from coordinates.
}
\description{
Extracts spatial data contained in a \code{\link{SpatialGridDataFrame}} or \code{\link{SpatialPixelsDataFrame}} corresponding to the coordinates of spatial classes.
}
\usage{
extractSpatialData(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \code{\link{GridTopology}}, \code{\link{SpatialGrid}} and \code{\link{SpatialPoints}}.}
  \item{y}{An object of class \code{\link{SpatialGridDataFrame}} or \code{\link{SpatialPixelsDataFrame}}}
}
\value{
A data frame with the contents of slot 'data' corresponding to the coordinates in \code{x}.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{\code{\link{SpatialGridDataFrame}}}

\examples{
# TO BE DONE
}
