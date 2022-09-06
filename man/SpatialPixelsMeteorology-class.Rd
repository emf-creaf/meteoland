\encoding{UTF-8}
\name{SpatialPixelsMeteorology-class}
\docType{class}
\alias{SpatialPixelsMeteorology-class}
\alias{[,SpatialPixelsMeteorology-method}
\alias{[,SpatialPixelsMeteorology,ANY,ANY-method}
\alias{show,SpatialPixelsMeteorology-method}

\title{Class \code{"SpatialPixelsMeteorology"}}
\description{
An S4 class that represents meteorology data that has locations on a regular grid.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpatialPixelsMeteorology", ...)}, or by calls to the function \code{\link{SpatialPixelsMeteorology}}.
}
\section{Slots}{
  \describe{
    \item{\code{dates}:}{Object of class \code{"Date"} describing the time period for which meteorological estimates are available.}
    \item{\code{data}:}{A vector of \code{"data.frame"} objects, each one containing the pixel data for one date.}
    \item{\code{grid}:}{Grid parameters (see \code{\linkS4class{GridTopology}}).}
    \item{\code{grid.index}:}{Index of points in full grid.}
    \item{\code{coords}:}{Object of class \code{"matrix"} with the spatial coordinates. }
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box. }
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string.}
    }
}
\section{Extends}{
Class \code{"\linkS4class{SpatialPixels}"}, directly.
Class \code{"\linkS4class{SpatialPoints}"}, by class "SpatialPixels", distance 2.
Class \code{"\linkS4class{Spatial}"}, by class "SpatialPixels", distance 3.
}


\usage{
  \S4method{[}{SpatialPixelsMeteorology,ANY,ANY}(x, i, j, ..., drop = FALSE)
  \S4method{show}{SpatialPixelsMeteorology}(object)
}
\arguments{
  \item{x,object}{\code{SpatialPixelsMeteorology} object}
  \item{i}{row indices}
  \item{j}{column indices}
  \item{drop}{see \code{\link[base]{Extract}}}
  \item{...}{indices passed through}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{SpatialPixelsTopography}}, \code{\link{SpatialPixelsDataFrame-class}}
}
\examples{
#Structure of the S4 object
showClass("SpatialPixelsMeteorology")
}
\keyword{classes}
