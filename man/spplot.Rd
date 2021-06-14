\encoding{UTF-8}
\name{spplot}
\alias{spplot,SpatialGridTopography-method}
\alias{spplot,SpatialGridMeteorology-method}
\alias{spplot,SpatialPixelsTopography-method}
\alias{spplot,SpatialPixelsMeteorology-method}

\title{Spatial grid plots}
\description{
Function \code{spplot} for \code{\link{SpatialGridTopography-class}} and \code{\link{SpatialPixelsTopography-class}} objects allows drawing maps of topographic attributes. Function \code{spplot} for \code{\link{SpatialGridMeteorology-class}} and \code{\link{SpatialPixelsMeteorology-class}} objects allows drawing maps of meteorological variables corresponding to specific dates.
}
\usage{
\S4method{spplot}{SpatialGridTopography}(obj, variable="elevation",...)
\S4method{spplot}{SpatialGridMeteorology}(obj, date, variable="MeanTemperature", ...)
\S4method{spplot}{SpatialPixelsTopography}(obj, variable="elevation",...)
\S4method{spplot}{SpatialPixelsMeteorology}(obj, date, variable="MeanTemperature", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{An object of class \code{SpatialGridTopography}.}
  \item{variable}{A string of the variable to be plotted (only \code{type="elevation"}, \code{type="slope"}, \code{type="aspect"} are allowed).}
  \item{...}{Additional parameters to function \code{\link{spplot}}.}
  \item{date}{A string or an integer for the date to be plotted.}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{
\code{\link{meteoplot}}
}
\examples{
data(examplegridtopography)

#Display data
spplot(examplegridtopography, type="elevation", scales=list(draw=TRUE))
spplot(examplegridtopography, type="slope", scales=list(draw=TRUE))
spplot(examplegridtopography, type="aspect", scales=list(draw=TRUE))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
