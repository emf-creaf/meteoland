\encoding{UTF-8}
\name{SpatialGridMeteorology-class}
\docType{class}
\alias{SpatialGridMeteorology-class}
\alias{[,SpatialGridMeteorology-method}
\alias{[,SpatialGridMeteorology,ANY,ANY-method}
\alias{show,SpatialGridMeteorology-method}

\title{Class \code{"SpatialGridMeteorology"}}
\description{
An S4 class that represents a spatial grid with meteorology daily data.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpatialGridMeteorology", ...)}, or by calls to the function \code{\link{SpatialGridMeteorology}}.
}
\section{Slots}{
  \describe{
    \item{\code{dates}:}{Object of class \code{"Date"} describing the time period for which meteorological estimates are available.}
    \item{\code{data}:}{A vector of \code{"data.frame"} objects, each one containing the grid data for one date.}
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box. }
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string.}
    }
}
\section{Extends}{
Class \code{"\linkS4class{SpatialGrid}"}, directly.
Class \code{"\linkS4class{Spatial}"}, by class "SpatialGrid", distance 2.
}

\usage{
  \S4method{[}{SpatialGridMeteorology,ANY,ANY}(x, i, j, ..., drop = FALSE)
  \S4method{show}{SpatialGridMeteorology}(object)
}
\arguments{
  \item{x,object}{\code{SpatialGridMeteorology} object}
  \item{i}{row indices}
  \item{j}{column indices}
  \item{drop}{see \code{\link[base]{Extract}}}
  \item{...}{indices passed through}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{
\code{\link{SpatialGridTopography}}, \code{\link{SpatialGridDataFrame-class}}
}
\examples{
#Structure of the S4 object
showClass("SpatialGridMeteorology")
}
\keyword{classes}
