\encoding{UTF-8}
\name{SpatialPixelsTopography-class}
\Rdversion{1.1}
\docType{class}
\alias{SpatialPixelsTopography-class}
\alias{[,SpatialPixelsTopography,ANY,ANY-method}
\alias{[,SpatialPixelsTopography,ANY,ANY,ANY-method}
\alias{show,SpatialPixelsTopography-method}

\title{Class \code{"SpatialPixelsTopography"}}
\description{
An S4 class that represents topography that has locations on a regular grid.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpatialPixelsTopography", ...)}, or by calls to the function \code{\link{SpatialPixelsTopography}}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} containing the elevation (in m), slope (in degrees) and aspect (in degrees from North) of every cell.}
    \item{\code{coords.nrs}:}{Inherited from \code{SpatialPointsDataFrame} but not used.}
    \item{\code{grid}:}{Object of class \code{\link{GridTopology}}.  }
    \item{\code{grid.index}:}{Index of points in full grid.}
    \item{\code{coords}:}{Object of class \code{"matrix"} with the spatial coordinates. }
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box. }
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string.}
    }
}
\section{Extends}{
Class \code{"\linkS4class{SpatialPixelsDataFrame}"}, directly.
Class \code{"\linkS4class{SpatialPixels}"}, by class "SpatialPixelsDataFrame", distance 2.
Class \code{"\linkS4class{SpatialPointsDataFrame}"}, by class "SpatialPixelsDataFrame", distance 2.
Class \code{"\linkS4class{SpatialPoints}"}, by class "SpatialPixelsDataFrame", distance 3.
Class \code{"\linkS4class{Spatial}"}, by class "SpatialPixelsDataFrame", distance 4.
}
\section{Methods}{
  \describe{
     \item{[}{\code{signature(x = "SpatialGridTopography", i = "ANY", j = "ANY", ..., drop = FALSE)}: subsets the pixels and associated topography; only one dimension can be subsetted. If \code{drop = TRUE} the boundary box is recalculated.}
    \item{spplot}{\code{signature(object = "SpatialPixelsTopography")}: allows plotting topography maps.}
	 }
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

\seealso{
\code{\link{SpatialPixelsTopography}}, \code{\link{SpatialPixelsDataFrame-class}}
}
\examples{
#Structure of the S4 object
showClass("SpatialPixelsTopography")
}
\keyword{classes}
