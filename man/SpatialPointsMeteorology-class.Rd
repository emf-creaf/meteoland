\encoding{UTF-8}
\name{SpatialPointsMeteorology-class}
\Rdversion{1.1}
\docType{class}
\alias{SpatialPointsMeteorology-class}
\alias{[,SpatialPointsMeteorology,ANY,ANY-method}
\alias{[,SpatialPointsMeteorology,ANY,ANY,ANY-method}
\alias{show,SpatialPointsMeteorology-method}

\title{Class \code{"SpatialPointsMeteorology"}}
\description{
An S4 class that represents a set of points with meteorology data series.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpatialPointsMeteorology", ...)}, or by calls to the function \code{\link{SpatialPointsMeteorology}}.
}
\section{Slots}{
  \describe{
    \item{\code{dates}:}{Object of class \code{"Date"} describing the time period for which meteorological estimates are available.}
    \item{\code{data}:}{A vector of \code{"data.frame"} objects, each one corresponding to one spatial point.}
    \item{\code{coords}:}{Object of class \code{"matrix"} with the spatial coordinates. }
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box. }
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string.}
    }
}
\section{Extends}{
Class \code{"\linkS4class{SpatialPoints}"}, directly.
Class \code{"\linkS4class{Spatial}"}, by class "SpatialPoints", distance 2.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "SpatialPointsMeteorology", i = "ANY", j = "ANY", ..., drop = TRUE)}: subsets the spatial points and the corresponding list of meteorological data; only rows (points) can be subsetted.}
    \item{coerce}{\code{signature(x = "SpatialPointsMeteorology", to = "STFDF")}: coerces into a spacetime STFDF object.}
    \item{coerce}{\code{signature(x = "SpatialPointsMeteorology", to = "stars")}: coerces into a stars object.}
	 }
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

\seealso{
\code{\link{SpatialPointsTopography-class}}, \code{\link{SpatialPoints-class}}
}
\examples{
#Structure of the S4 object
showClass("SpatialPointsMeteorology")
}
\keyword{classes}
