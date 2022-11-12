\encoding{UTF-8}
\name{interpolation.coverage}
\alias{interpolation.coverage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spatial and temporal coverage of interpolation data
}
\description{
Function \code{interpolation.coverage} calculates, for each meteorological variable, the number of stations with data per date or the number of dates with data per station in an object of class \code{\link{MeteorologyInterpolationData-class}}.
}
\usage{
interpolation.coverage(object, type = "spatial", percent = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{MeteorologyInterpolationData-class}}.}
  \item{type}{A string with the coverage summary to be produced (either "spatial" or "temporal").}
  \item{percent}{A boolean flag to indicate that percentages should be returned instead of counts.}
}
\value{
If \code{type = "spatial"} the function returns an object of class \code{SpatialPointsDataFrame} with the number (or percentage) of dates with data per station and meteorological variable. If \code{type = "temporal"} the function returns an object of class \code{data.frame} with the number (or percentage) of stations with data per day and meteorological variable.
}
\references{
De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating daily meteorological data and downscaling climate models over landscapes. Environmental Modelling and Software 108: 186-196.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{\code{\link{MeteorologyInterpolationData}}}

\examples{
data(exampleinterpolationdata)

#Number of days with data per station
head(interpolation.coverage(exampleinterpolationdata))

#Number of stations with data per day
head(interpolation.coverage(exampleinterpolationdata, type = "temporal"))
}
