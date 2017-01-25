\encoding{UTF-8}
\name{radiation}
\alias{radiation_dateStringToJulianDays}
\alias{radiation_daylength}
\alias{radiation_daylengthseconds}
\alias{radiation_potentialRadiation}
\alias{radiation_julianDay}
\alias{radiation_outgoingLongwaveRadiation}
\alias{radiation_netRadiation}
\alias{radiation_solarRadiation}
\alias{radiation_solarDeclination}
\alias{radiation_sunRiseSet}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Solar radiation utility functions
}
\description{
Set of functions used in the calculation of incoming solar radiation and net radiation. 
}
\usage{
radiation_dateStringToJulianDays(dateStrings)
radiation_daylength(latrad, slorad, asprad, delta)
radiation_daylengthseconds(latrad, slorad, asprad, delta)
radiation_potentialRadiation(latrad, slorad, asprad, delta)
radiation_julianDay(year, month, day)
radiation_outgoingLongwaveRadiation(latrad, elevation,  slorad, asprad, delta, 
                    vpa, tmin, tmax, R_s)
radiation_netRadiation(latrad, elevation, slorad, asprad,  delta, 
                    vpa, tmin, tmax, R_s, alpha = 0.08)
radiation_solarDeclination(J)
radiation_solarRadiation(latrad, elevation, slorad, asprad, delta,
                         diffTemp, diffTempMonth, vpa, precipitation)
radiation_sunRiseSet(latrad, slorad, asprad, delta)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dateStrings}{A character vector with dates in format "YYYY-MM-DD".}
  \item{latrad}{Latitude (in radians North).}
  \item{slorad}{Slope (in radians).}
  \item{asprad}{Aspect (in radians from North).}
  \item{delta}{Solar declination (in radians).}
  \item{J}{Julian day (integer), number of days since January 1, 4713 BCE at noon UTC.}
  \item{year, month, day}{Year, month and day as integers.}
  \item{R_s}{Incident solar radiation (MJ·m-2)}
  \item{alpha}{Surface albedo (from 0 to 1).}
  \item{vpa}{Average daily vapor pressure deficit (kPa).}
  \item{tmin, tmax}{Minimum and maximum daily temperature (ºC).}
  \item{elevation}{Elevation above sea level (in m).}
  \item{precipitation}{Precipitation (in mm).}
  \item{diffTemp}{Difference between maximum and minimum temperature (ºC).}
  \item{diffTempMonth}{Difference between maximum and minimum temperature, averaged over 30 days  (ºC).}
}

\value{
Values returned for each function are:
\itemize{
\item{\code{radiation_dateStringToJulianDays}: A vector of Julian days (i.e. number of days since January 1, 4713 BCE at noon UTC).}
\item{\code{radiation_daylength}: Day length (in hours).}
\item{\code{radiation_daylengthseconds}: Day length (in seconds).}
\item{\code{radiation_potentialRadiation}: Daily (potential) solar radiation (in MJ·m-2).}
\item{\code{radiation_julianDay}: Number of days since January 1, 4713 BCE at noon UTC.}
\item{\code{radiation_outgoingLongwaveRadiation}: Daily outgoing longwave radiation (MJ·m-2·day-1).} 
\item{\code{radiation_netRadiation}: Daily net solar radiation (MJ·m-2·day-1).} 
\item{\code{radiation_solarDeclination}: Solar declination (in radians).}
\item{\code{radiation_solarRadiation}: Daily incident solar radiation (MJ·m-2·day-1).}
\item{\code{radiation_sunRiseSet}: Sunrise and sunset hours in hour angle (radians).}
}
}
\note{Code for \code{radiation_julianDay()} and \code{radiation_solarDeclination()} was translated to C++ from R code in package 'insol' (by J. G. Corripio).}
\references{
Danby, J. M. Eqn. 6.16.4 in Fundamentals of Celestial Mechanics, 2nd ed. Richmond, VA: Willmann-Bell, p. 207, 1988.

Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800

McMahon, T. A., M. C. Peel, L. Lowe, R. Srikanthan, and T. R. McVicar. 2013. Estimating actual, potential, reference crop and pan evaporation using standard meteorological data: a pragmatic synthesis. Hydrology & Earth System Sciences 17:1331–1363. See also:  http://www.fao.org/docrep/x0490e/x0490e06.htm.

Reda, I. and Andreas, A. 2003. Solar Position Algorithm for Solar Radiation Applications. 55 pp.; NREL Report No. TP-560-34302, Revised January 2008. http://www.nrel.gov/docs/fy08osti/34302.pdf

}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya
}
\seealso{
\code{\link{interpolationpoints}}
}