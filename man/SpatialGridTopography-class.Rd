\encoding{UTF-8}
\name{SpatialGridTopography-class}
\Rdversion{1.1}
\docType{class}
\alias{SpatialGridTopography-class}
\alias{[,SpatialGridTopography,ANY,ANY,ANY-method}
\alias{show,SpatialGridTopography-method}

\title{Class \code{"SpatialGridTopography"}}
\description{
An S4 class that represents topography over a grid of coordinates.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpatialGridTopography", ...)}, or by calls to the function \code{\link{SpatialGridTopography}}.
}
\section{Slots}{
  \describe{
    \item{\code{grid}:}{Object of class \code{\link{GridTopology}}.  }
    \item{\code{data}:}{Object of class \code{"data.frame"} containing the elevation (in m), slope (in degrees) and aspect (in degrees from North) of every cell.}
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box. }
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string.}
    }
}
\section{Extends}{
Class \code{"\linkS4class{SpatialGridDataFrame}"}, directly.
Class \code{"\linkS4class{SpatialGrid}"}, by class "SpatialGridDataFrame", distance 2.
Class \code{"\linkS4class{Spatial}"}, by class "SpatialGridDataFrame", distance 3.
}
\section{Methods}{
  \describe{
     \item{[}{\code{signature(x = "SpatialGridTopography", i = "ANY", j = "ANY", drop = "ANY")}: subsets the grid and associated topography; only rows (x values) and columns (y values) can be subsetted.}
	 }
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{SpatialGridTopography}}, \code{\link{SpatialGridDataFrame-class}}
}
\examples{
#Structure of the S4 object
showClass("SpatialGridTopography")
}
\keyword{classes}
