\encoding{UTF-8}
\name{MeteorologyProcedureData-class}
\Rdversion{1.1}
\docType{class}
\alias{MeteorologyProcedureData-class}

\title{Class \code{"MeteorologyProcedureData"}}
\description{
A virtual class for estimating meteorology over landscapes
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{dates}:}{Object of class \code{"Date"} describing the time period for which meteorological estimates are possible.}
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box that sets meteorological estimation boundaries.}
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string of accepted coordinates.}
  }
}
\section{Methods}{
  \describe{
    \item{subsample}{\code{signature(object = "MeteorologyProcedureData")}: Generates \code{MeteorologyProcedureData} objects for a smaller area and a subset of dates. }
	 }
}
\section{Extends}{
Class \code{"\linkS4class{Spatial}"}, directly.
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

\seealso{
\code{\link{MeteorologyInterpolationData-class}}, \code{\link{MeteorologyUncorrectedData-class}}
}
\examples{
showClass("MeteorologyProcedureData")
}
\keyword{classes}
