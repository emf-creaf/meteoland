\encoding{UTF-8}
\name{MeteorologyUncorrectedData-class}
\Rdversion{1.1}
\docType{class}
\alias{MeteorologyUncorrectedData-class}
\title{Class \code{"MeteorologyUncorrectedData"}}
\description{
An S4 class to conduct statistical correction of meteorology over a landscape.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MeteorologyUncorrectedData", ...)}, or by calls to the function \code{\link{MeteorologyUncorrectedData}}.
}
\section{Slots}{
  \describe{
    \item{\code{dates}:}{Object of class \code{"Date"} describing the time period for which meteorological estimates are possible.}
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box that sets meteorological estimation boundaries.}
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string of station spatial coordinates.}
    \item{\code{coords}:}{Object of class \code{"matrix"} containing the coordinates of weather stations (each row is a point).}
    \item{\code{reference_data}:}{Reference (historic) meteorological data used to calibrate correction factors when compared with observations. A vector of data frames (one per point) or a single data frame containing the meta data (columns \code{dir} and \code{filename}) of meteorological files that will be read from the disk. Alternatively, a NetCDF file name where points should be read. }
    \item{\code{projection_data}:}{Projection meteorological data to be corrected. A vector of data frames (one per point) or a single data frame containing the meta data (columns \code{dir} and \code{filename}) of meteorological files that will be read from the disk. Alternatively, a NetCDF file name where points should be read.}
    \item{\code{params}:}{A \code{"list"} containing correction parameters. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{MeteorologyProcedureData}"}, directly.
Class \code{"\linkS4class{Spatial}"}, by class "MeteorologyProcedureData", distance 2
}
\section{Methods}{
  \describe{
    \item{subsample}{\code{signature(object = "MeteorologyUncorrectedData")}: Generates a \code{\link{MeteorologyUncorrectedData}} object for a smaller area and a subset of dates. }
	 }
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{
\code{\link{MeteorologyUncorrectedData}}, \code{\link{MeteorologyProcedureData-class}}, \code{\link{examplecorrectiondata}}, \code{\link{subsample}}
}
\examples{
#Structure of the S4 object
showClass("MeteorologyUncorrectedData")
}
\keyword{classes}
