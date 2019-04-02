\name{meteocomplete}
\alias{meteocomplete}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Complete daily meteorological variables
}
\description{
Fills missing values of relative humidity, radiation and potential evapotranspiration from a data frame with daily values of minimum/maximum/mean temperature and precipitation.
}
\usage{
meteocomplete(x, latitude, elevation, slope, aspect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A data frame with dates as row names and columns named 'MeanTemperature', 'MaxTemperature', 'MinTemperature' and 'Precipitation' 
  }
  \item{latitude}{
    Latitude in degrees North.
  }
  \item{elevation}{
    Elevation in m.a.s.l.
   }
  \item{slope}{
    Slope in degrees.
  }
  \item{aspect}{
    Aspect in degrees from North.
  }
}
\details{
  The function fills values for humidity, radiation and PET only if they are missing in the input data frame. If a column 'SpecificHumidity' is present in the input data, relative humidity is calculated from it. Otherwise, relative humidity is calculated assuming that dew point temperature equals the minimum temperature. Potential solar radiation is calculated from latitude, slope and aspect. Incoming solar radiation is then corrected following Thornton & Running (1999) and potential evapotranspiration following Penman (1948).
}
\value{
  A data frame copied from \code{x} but with filled values for variables:
  \itemize{
  \item{\code{MeanRelativeHumidity}: Mean daily relative humidity (in percent).}
  \item{\code{MinRelativeHumidity}: Minimum daily relative humidity (in percent).}
  \item{\code{MaxRelativeHumidity}: Maximum daily relative humidity (in percent).}
  \item{\code{Radiation}: Incoming solar radiation (MJ/m2).}
  \item{\code{PET}: Potential evapotranspiration (in mm of water).}
  }

}
\references{
Thornton, P.E., Running, S.W., 1999. An improved algorithm for estimating incident daily solar radiation from measurements of temperature, humidity, and precipitation. Agric. For. Meteorol. 93, 211-228.

Penman, H. L. 1948. Natural evaporation from open water, bare soil
and grass. Proceedings of the Royal Society of London. Series A.
Mathematical and Physical Sciences, 193, 120-145.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}

\seealso{
\code{\link{penman}}, \code{\link{radiation_solarRadiation}}
}
