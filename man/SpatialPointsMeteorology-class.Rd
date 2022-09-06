\encoding{UTF-8}
\name{SpatialPointsMeteorology-class}
\docType{class}
\alias{SpatialPointsMeteorology-class}
\alias{[,SpatialPointsMeteorology-method}
\alias{[,SpatialPointsMeteorology,ANY,ANY-method}
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
\usage{
  \S4method{[}{SpatialPointsMeteorology,ANY,ANY}(x, i, j, ..., drop = TRUE)
  \S4method{show}{SpatialPointsMeteorology}(object)
}
\arguments{
  \item{x,object}{\code{SpatialPointsMeteorology} object}
  \item{i}{row indices}
  \item{j}{column indices}
  \item{drop}{see \code{\link[base]{Extract}}}
  \item{...}{indices passed through}
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
