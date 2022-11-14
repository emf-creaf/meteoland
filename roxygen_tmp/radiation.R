#' Solar radiation utility functions
#' 
#' Set of functions used in the calculation of incoming solar radiation and net
#' radiation.
#' 
#' 
#' @aliases radiation_dateStringToJulianDays radiation_daylength
#' radiation_daylengthseconds radiation_directDiffuseInstant
#' radiation_directDiffuseDay radiation_potentialRadiation radiation_julianDay
#' radiation_skyLongwaveRadiation radiation_outgoingLongwaveRadiation
#' radiation_netRadiation radiation_solarRadiation radiation_solarConstant
#' radiation_solarElevation radiation_solarDeclination radiation_sunRiseSet
#' @param dateStrings A character vector with dates in format "YYYY-MM-DD".
#' @param latrad Latitude (in radians North).
#' @param slorad Slope (in radians).
#' @param asprad Aspect (in radians from North).
#' @param delta Solar declination (in radians).
#' @param solarConstant Solar constant (in kW·m-2).
#' @param hrad Solar hour (in radians).
#' @param R_s Daily incident solar radiation (MJ·m-2).
#' @param clearday Boolean flag to indicate a clearsky day (vs. overcast).
#' @param nsteps Number of daily substeps.
#' @param J Julian day (integer), number of days since January 1, 4713 BCE at
#' noon UTC.
#' @param year,month,day Year, month and day as integers.
#' @param alpha Surface albedo (from 0 to 1).
#' @param Tair Air temperature (in degrees Celsius).
#' @param vpa Average daily vapor pressure (kPa).
#' @param c Proportion of sky covered by clouds [0-1].
#' @param tmin,tmax Minimum and maximum daily temperature (ºC).
#' @param elevation Elevation above sea level (in m).
#' @param precipitation Precipitation (in mm).
#' @param diffTemp Difference between maximum and minimum temperature (ºC).
#' @param diffTempMonth Difference between maximum and minimum temperature,
#' averaged over 30 days (ºC).
#' @return Values returned for each function are: \itemize{
#' \item\code{radiation_dateStringToJulianDays}: A vector of Julian days (i.e.
#' number of days since January 1, 4713 BCE at noon UTC).
#' \item\code{radiation_daylength}: Day length (in hours).
#' \item\code{radiation_daylengthseconds}: Day length (in seconds).
#' \item\code{radiation_directDiffuseInstant}: A vector with instantaneous
#' direct and diffusive radiation rates (for both SWR and PAR).
#' \item\code{radiation_directDiffuseDay}: A data frame with instantaneous
#' direct and diffusive radiation rates (for both SWR and PAR) for each
#' subdaily time step. \item\code{radiation_potentialRadiation}: Daily
#' (potential) solar radiation (in MJ·m-2). \item\code{radiation_julianDay}:
#' Number of days since January 1, 4713 BCE at noon UTC.
#' \item\code{radiation_skyLongwaveRadiation}: Instantaneous incoming (sky)
#' longwave radiation (W·m-2).
#' \item\code{radiation_outgoingLongwaveRadiation}: Daily outgoing longwave
#' radiation (MJ·m-2·day-1).  \item\code{radiation_netRadiation}: Daily net
#' solar radiation (MJ·m-2·day-1).  \item\code{radiation_solarConstant}: Solar
#' constant (in kW·m-2). \item\code{radiation_solarDeclination}: Solar
#' declination (in radians). \item\code{radiation_solarElevation}: Angle of
#' elevation of the sun with respect to the horizon (in radians).
#' \item\code{radiation_solarRadiation}: Daily incident solar radiation
#' (MJ·m-2·day-1). \item\code{radiation_sunRiseSet}: Sunrise and sunset hours
#' in hour angle (radians). }
#' @note Code for \code{radiation_julianDay()},
#' \code{radiation_solarConstant()} and \code{radiation_solarDeclination()} was
#' translated to C++ from R code in package 'insol' (by J. G. Corripio).
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{interpolationpoints}}
#' @references Danby, J. M. Eqn. 6.16.4 in Fundamentals of Celestial Mechanics,
#' 2nd ed. Richmond, VA: Willmann-Bell, p. 207, 1988.
#' 
#' Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct
#' shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800
#' 
#' McMahon, T. A., M. C. Peel, L. Lowe, R. Srikanthan, and T. R. McVicar. 2013.
#' Estimating actual, potential, reference crop and pan evaporation using
#' standard meteorological data: a pragmatic synthesis. Hydrology & Earth
#' System Sciences 17:1331–1363. See also:
#' http://www.fao.org/docrep/x0490e/x0490e06.htm.
#' 
#' Reda, I. and Andreas, A. 2003. Solar Position Algorithm for Solar Radiation
#' Applications. 55 pp.; NREL Report No. TP-560-34302, Revised January 2008.
#' http://www.nrel.gov/docs/fy08osti/34302.pdf
#' 
#' Spitters, C.J.T., Toussaint, H.A.J.M. and Goudriaan, J. (1986). Separating
#' the diffuse and direct components of global radiation and its implications
#' for modeling canopy photosynthesis. I. Components of incoming radiation.
#' Agricultural and Forest Meteorology, 38, 231–242.
