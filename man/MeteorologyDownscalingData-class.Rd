\encoding{UTF-8}
\name{MeteorologyDownscalingData-class}
\Rdversion{1.1}
\docType{class}
\alias{MeteorologyDownscalingData-class}
\title{Class \code{"MeteorologyDownscalingData"}}
\description{
An S4 class to conduct statistical downscaling of meteorology over a landscape.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MeteorologyDownscalingData", ...)}, or by calls to the function \code{\link{MeteorologyDownscalingData}}.
}
\section{Slots}{
  \describe{
    \item{\code{dates}:}{Object of class \code{"Date"} describing the time period for which meteorological estimates are possible.}
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box that sets meteorological estimation boundaries.}
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string of station spatial coordinates.}
    \item{\code{coords}:}{Object of class \code{"matrix"} containing the coordinates of weather stations (each row is a point).}
    \item{\code{historicdata}:}{Historic (reference) meteorological data used to calibrate correction factors when compared with observations. A vector of data frames (one per point) or a single data frame containing the meta data (columns \code{dir} and \code{filename}) of meteorological files that will be read from the disk. }
    \item{\code{futuredata}:}{Future (predicted) meteorological data to be corrected. A vector of data frames (one per point) or a single data frame containing the meta data (columns \code{dir} and \code{filename}) of meteorological files that will be read from the disk. }
    \item{\code{params}:}{A \code{"list"} containing downscaling parameters. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{MeteorologyProcedureData}"}, directly.
Class \code{"\linkS4class{Spatial}"}, by class "MeteorologyProcedureData", distance 2
}
\section{Methods}{
  \describe{
    \item{subsample}{\code{signature(object = "MeteorologyDownscalingData")}: Generates a \code{\link{MeteorologyDownscalingData}} object for a smaller area and a subset of dates. }
	 }
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{
\code{\link{MeteorologyDownscalingData}}, \code{\link{MeteorologyProcedureData-class}}, \code{\link{exampledownscalingdata}}, \code{\link{subsample}}
}
\examples{
#Structure of the S4 object
showClass("MeteorologyDownscalingData")
}
\keyword{classes}
