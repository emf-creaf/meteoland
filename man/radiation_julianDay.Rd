% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{radiation_julianDay}
\alias{radiation_julianDay}
\alias{radiation_dateStringToJulianDays}
\alias{radiation_daylength}
\alias{radiation_daylengthseconds}
\alias{radiation_directDiffuseInstant}
\alias{radiation_directDiffuseDay}
\alias{radiation_potentialRadiation}
\alias{radiation_skyLongwaveRadiation}
\alias{radiation_outgoingLongwaveRadiation}
\alias{radiation_netRadiation}
\alias{radiation_solarRadiation}
\alias{radiation_solarConstant}
\alias{radiation_solarElevation}
\alias{radiation_solarDeclination}
\alias{radiation_sunRiseSet}
\title{Solar radiation utility functions}
\usage{
radiation_julianDay(year, month, day)

radiation_dateStringToJulianDays(dateStrings)

radiation_solarDeclination(J)

radiation_solarConstant(J)

radiation_sunRiseSet(latrad, slorad, asprad, delta)

radiation_solarElevation(latrad, delta, hrad)

radiation_daylength(latrad, slorad, asprad, delta)

radiation_daylengthseconds(latrad, slorad, asprad, delta)

radiation_potentialRadiation(solarConstant, latrad, slorad, asprad, delta)

radiation_solarRadiation(
  solarConstant,
  latrad,
  elevation,
  slorad,
  asprad,
  delta,
  diffTemp,
  diffTempMonth,
  vpa,
  precipitation
)

radiation_directDiffuseInstant(
  solarConstant,
  latrad,
  slorad,
  asprad,
  delta,
  hrad,
  R_s,
  clearday
)

radiation_directDiffuseDay(
  solarConstant,
  latrad,
  slorad,
  asprad,
  delta,
  R_s,
  clearday,
  nsteps = 24L
)

radiation_skyLongwaveRadiation(Tair, vpa, c = 0)

radiation_outgoingLongwaveRadiation(
  solarConstant,
  latrad,
  elevation,
  slorad,
  asprad,
  delta,
  vpa,
  tmin,
  tmax,
  R_s
)

radiation_netRadiation(
  solarConstant,
  latrad,
  elevation,
  slorad,
  asprad,
  delta,
  vpa,
  tmin,
  tmax,
  R_s,
  alpha = 0.08
)
}
\arguments{
\item{year, month, day}{Year, month and day as integers.}

\item{dateStrings}{A character vector with dates in format "YYYY-MM-DD".}

\item{J}{Julian day (integer), number of days since January 1, 4713 BCE at
noon UTC.}

\item{latrad}{Latitude (in radians North).}

\item{slorad}{Slope (in radians).}

\item{asprad}{Aspect (in radians from North).}

\item{delta}{Solar declination (in radians).}

\item{hrad}{Solar hour (in radians).}

\item{solarConstant}{Solar constant (in kW·m-2).}

\item{elevation}{Elevation above sea level (in m).}

\item{diffTemp}{Difference between maximum and minimum temperature (ºC).}

\item{diffTempMonth}{Difference between maximum and minimum temperature,
averaged over 30 days (ºC).}

\item{vpa}{Average daily vapor pressure (kPa).}

\item{precipitation}{Precipitation (in mm).}

\item{R_s}{Daily incident solar radiation (MJ·m-2).}

\item{clearday}{Boolean flag to indicate a clearsky day (vs. overcast).}

\item{nsteps}{Number of daily substeps.}

\item{Tair}{Air temperature (in degrees Celsius).}

\item{c}{Proportion of sky covered by clouds (0-1).}

\item{tmin, tmax}{Minimum and maximum daily temperature (ºC).}

\item{alpha}{Surface albedo (from 0 to 1).}
}
\value{
Values returned for each function are: \itemize{
\item\code{radiation_dateStringToJulianDays}: A vector of Julian days (i.e.
number of days since January 1, 4713 BCE at noon UTC).
\item\code{radiation_daylength}: Day length (in hours).
\item\code{radiation_daylengthseconds}: Day length (in seconds).
\item\code{radiation_directDiffuseInstant}: A vector with instantaneous
direct and diffusive radiation rates (for both SWR and PAR).
\item\code{radiation_directDiffuseDay}: A data frame with instantaneous
direct and diffusive radiation rates (for both SWR and PAR) for each
subdaily time step. \item\code{radiation_potentialRadiation}: Daily
(potential) solar radiation (in MJ·m-2). \item\code{radiation_julianDay}:
Number of days since January 1, 4713 BCE at noon UTC.
\item\code{radiation_skyLongwaveRadiation}: Instantaneous incoming (sky)
longwave radiation (W·m-2).
\item\code{radiation_outgoingLongwaveRadiation}: Daily outgoing longwave
radiation (MJ·m-2·day-1).  \item\code{radiation_netRadiation}: Daily net
solar radiation (MJ·m-2·day-1).  \item\code{radiation_solarConstant}: Solar
constant (in kW·m-2). \item\code{radiation_solarDeclination}: Solar
declination (in radians). \item\code{radiation_solarElevation}: Angle of
elevation of the sun with respect to the horizon (in radians).
\item\code{radiation_solarRadiation}: Daily incident solar radiation
(MJ·m-2·day-1). \item\code{radiation_sunRiseSet}: Sunrise and sunset hours
in hour angle (radians). }
}
\description{
Set of functions used in the calculation of incoming solar radiation and net
radiation.
}
\section{Functions}{
\itemize{
\item \code{radiation_dateStringToJulianDays()}: Date string to julian days

\item \code{radiation_solarDeclination()}: solar declination

\item \code{radiation_solarConstant()}: solar constant

\item \code{radiation_sunRiseSet()}: sun rise and set

\item \code{radiation_solarElevation()}: solar elevation

\item \code{radiation_daylength()}: Day length

\item \code{radiation_daylengthseconds()}: Day length seconds

\item \code{radiation_potentialRadiation()}: Potential radiation

\item \code{radiation_solarRadiation()}: solar Radiation

\item \code{radiation_directDiffuseInstant()}: Direct diffuse instant

\item \code{radiation_directDiffuseDay()}: Direct diffuse day

\item \code{radiation_skyLongwaveRadiation()}: Sky longwave radiation

\item \code{radiation_outgoingLongwaveRadiation()}: Outgoing longwave radiation

\item \code{radiation_netRadiation()}: Net radiation

}}
\note{
Code for \code{radiation_julianDay()},
\code{radiation_solarConstant()} and \code{radiation_solarDeclination()} was
translated to C++ from R code in package 'insol' (by J. G. Corripio).
}
\references{
Danby, J. M. Eqn. 6.16.4 in Fundamentals of Celestial Mechanics,
2nd ed. Richmond, VA: Willmann-Bell, p. 207, 1988.

Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct
shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800

McMahon, T. A., M. C. Peel, L. Lowe, R. Srikanthan, and T. R. McVicar. 2013.
Estimating actual, potential, reference crop and pan evaporation using
standard meteorological data: a pragmatic synthesis. Hydrology & Earth
System Sciences 17:1331–1363. See also:
http://www.fao.org/docrep/x0490e/x0490e06.htm.

Reda, I. and Andreas, A. 2003. Solar Position Algorithm for Solar Radiation
Applications. 55 pp.; NREL Report No. TP-560-34302, Revised January 2008.
http://www.nrel.gov/docs/fy08osti/34302.pdf

Spitters, C.J.T., Toussaint, H.A.J.M. and Goudriaan, J. (1986). Separating
the diffuse and direct components of global radiation and its implications
for modeling canopy photosynthesis. I. Components of incoming radiation.
Agricultural and Forest Meteorology, 38, 231–242.
}
\seealso{
\code{\link{interpolate_data}}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
