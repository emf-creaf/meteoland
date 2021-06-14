\encoding{UTF-8}
\name{MeteorologyInterpolationData-class}
\Rdversion{1.1}
\docType{class}
\alias{MeteorologyInterpolationData-class}
\title{Class \code{"MeteorologyInterpolationData"}}
\description{
An S4 class to interpolate meteorology over a landscape.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MeteorologyInterpolationData", ...)}, or by calls to the function \code{\link{MeteorologyInterpolationData}}.
}
\section{Slots}{
  \describe{
    \item{\code{dates}:}{Object of class \code{"Date"} describing the time period for which meteorological estimates are possible.}
    \item{\code{bbox}:}{Object of class \code{"matrix"} with the boundary box that sets meteorological estimation boundaries.}
    \item{\code{proj4string}:}{Object of class \code{"CRS"} with the projection string of station spatial coordinates.}
    \item{\code{coords}:}{Object of class \code{"matrix"} containing the coordinates of weather stations (each row is a point).}
    \item{\code{elevation}:}{A numeric vector with elevation values of weather stations (in meters).}
    \item{\code{slope}:}{A numeric vector with slope values of weather stations (in degrees). Needed for cross-validation only.}
    \item{\code{aspect}:}{A numeric vector with aspect values of weather stations (in degrees from North). Needed for cross-validation only.}
    \item{\code{MinTemperature}:}{Object of class \code{"data.frame"} with minimum temperature recordings (in degrees Celsius) for all weather stations (in rows) and all days (in columns). }
    \item{\code{MaxTemperature}:}{Object of class \code{"data.frame"} with maximum temperature recordings (in degrees Celsius) for all weather stations (in rows) and all days (in columns).  }
    \item{\code{SmoothedTemperatureRange}:}{Object of class \code{"matrix"} with temporally smoothed temperature range recordings (in degrees Celsius) for all weather stations (in rows) and all days (in columns).}
    \item{\code{Precipitation}:}{Object of class \code{"matrix"} with precipitation recordings (in mm of water) for all weather stations (in rows) and all days (in columns).}
    \item{\code{SmoothedPrecipitation}:}{Object of class \code{"matrix"} with temporally smoothed precipitation recordings (in mm of water) for all weather stations (in rows) and all days (in columns).}
    \item{\code{RelativeHumidity}:}{Object of class \code{matrix} with relative humidity recordings (in percent) for all weather stations (in rows) and all days (in columns). }
    \item{\code{Radiation}:}{Object of class \code{matrix} with relative radiation recordings (in MJ/m2) for all weather stations (in rows) and all days (in columns). Needed for cross-validation only.}
    \item{\code{WindSpeed}:}{Object of class \code{"matrix"} with wind speed recordings (in m/s) for all weather stations (in rows) and all days (in columns). }
    \item{\code{WindDirection}:}{Object of class \code{"matrix"} with wind direction recordings (in degrees from North) for all weather stations (in rows) and all days (in columns). }
    \item{\code{WindFields}:}{Object of class \code{"list"}. See function \code{\link{readWindNinjaWindFields}}.}
    \item{\code{WFIndex}:}{Object of class \code{"matrix"} with the closest windfield index for each stations in each day.}
    \item{\code{WFFactor}:}{Object of class \code{"matrix"} with the multiplication factor for the wind speed of each stations in each day.}
    \item{\code{params}:}{A \code{"list"} containing interpolation parameters. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{MeteorologyProcedureData}"}, directly.
Class \code{"\linkS4class{Spatial}"}, by class "MeteorologyProcedureData", distance 2
}
\section{Methods}{
  \describe{
    \item{subsample}{\code{signature(object = "MeteorologyInterpolationData")}: Generates a \code{\link{MeteorologyInterpolationData}} object for a smaller area and a subset of dates. }
	 }
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{
\code{\link{MeteorologyInterpolationData}}, \code{\link{MeteorologyProcedureData-class}}, \code{\link{subsample}}
}
\examples{
#Structure of the S4 object
showClass("MeteorologyInterpolationData")
}
\keyword{classes}
