% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_reshape_meteo.R
\name{complete_meteo}
\alias{complete_meteo}
\title{Complete missing meteo variables}
\usage{
complete_meteo(meteo, verbose = getOption("meteoland_verbosity", TRUE))
}
\arguments{
\item{meteo}{meteoland weather data}

\item{verbose}{Logical indicating if the function must show messages and info.
Default value checks \code{"meteoland_verbosity"} option and if not set, defaults
to TRUE. It can be turned off for the function with FALSE, or session wide with
\code{options(meteoland_verbosity = FALSE)}}
}
\value{
the same \code{meteo} data provided with the the variables completed
}
\description{
Calculates missing values of relative humidity, radiation and potential
evapotranspiration from a data frame with daily values of
minimum/maximum/mean temperature and precipitation. The function
takes a meteo object (with meteoland names) and complete any
missing variable if it is possible
}
\details{
#' The function fills values for humidity, radiation and PET only if they are
missing in the input data frame. If a column 'SpecificHumidity' is present
in the input data, relative humidity is calculated from it. Otherwise,
relative humidity is calculated assuming that dew point temperature equals
the minimum temperature. Potential solar radiation is calculated from
latitude, slope and aspect. Incoming solar radiation is then corrected
following Thornton & Running (1999) and potential evapotranspiration
following Penman (1948).
}
\examples{

\donttest{
# example data
data("meteoland_meteo_example")

# remove MinRelativeHumidity
meteoland_meteo_example$MinRelativeHumidity <- NULL
# complete vars
completed_meteo <- complete_meteo(meteoland_meteo_example)
# check MinRelativeHumidity
completed_meteo$MinRelativeHumidity
}

}
\references{
Thornton, P.E., Running, S.W., 1999. An improved algorithm for
estimating incident daily solar radiation from measurements of temperature,
humidity, and precipitation. Agric. For. Meteorol. 93, 211-228.

Penman, H. L. 1948. Natural evaporation from open water, bare soil and
grass. Proceedings of the Royal Society of London. Series A. Mathematical
and Physical Sciences, 193, 120-145.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF

Victor Granda \enc{García}{Garcia}, EMF-CREAF
}
