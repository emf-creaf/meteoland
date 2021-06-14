\encoding{UTF-8}
\name{SpatialPointsTopography-class}
\Rdversion{1.1}
\docType{class}
\alias{SpatialPointsTopography-class}
\alias{[,SpatialPointsTopography,ANY,ANY,ANY-method}
\alias{show,SpatialPointsTopography-method}

\title{Class \code{"SpatialPointsTopography"}}
\description{
An S4 class that represents topography over a grid of coordinates.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpatialPointsTopography", ...)}, or by calls to the function \code{\link{SpatialPointsTopography}}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} containing the elevation (in m), slope (in degrees) and aspect (in degrees from North) of every point.}
    \item{\code{coords}:}{Object of class \code{"matrix"} with the spatial coordinates. }
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box. }
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string.}
    }
}
\section{Extends}{
Class \code{"\linkS4class{SpatialPointsDataFrame}"}, directly.
Class \code{"\linkS4class{SpatialPoints}"}, by class "SpatialPointsDataFrame", distance 2.
Class \code{"\linkS4class{Spatial}"}, by class "SpatialPointsDataFrame", distance 3.
}
\section{Methods}{
  \describe{
     \item{[}{\code{signature(x = "SpatialPointsTopography", i = "ANY", j = "ANY", drop = "ANY")}: subsets the spatial points and associated topography; only rows (points) can be subsetted.}
	 }
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

\seealso{
\code{\link{SpatialPointsTopography}}, \code{\link{SpatialPointsDataFrame-class}}
}
\examples{
#Structure of the S4 object
showClass("SpatialPointsTopography")
}
\keyword{classes}
