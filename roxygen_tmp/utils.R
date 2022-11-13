#' Physical utility functions
#' 
#' Set of functions used in the calculation of physical variables.
#' 
#' 
#' @aliases utils_airDensity utils_atmosphericPressure utils_averageDailyVP
#' utils_averageDaylightTemperature utils_latentHeatVaporisation
#' utils_latentHeatVaporisationMol utils_psychrometricConstant
#' utils_saturationVP utils_saturationVaporPressureCurveSlope
#' @param temperature Air temperature (ºC).
#' @param Tmin,Tmax Minimum and maximum daily temperature (ºC).
#' @param RHmin,RHmax Minimum and maximum relative humidity (\%).
#' @param Patm Atmospheric air pressure (in kPa).
#' @param elevation Elevation above sea level (in m).
#' @return Values returned for each function are: \itemize{
#' \item\code{utils_airDensity}: air density (in kg·m-3).
#' \item\code{utils_atmosphericPressure}: Air atmospheric pressure (in kPa).
#' \item\code{utils_averageDailyVP}: average (actual) vapour pressure (in kPa).
#' \item\code{utils_averageDaylightTemperature}: average daylight air
#' temperature (in ºC). \item\code{utils_latentHeatVaporisation}: Latent heat
#' of vaporisation (MJ·kg-1).  \item\code{utils_latentHeatVaporisationMol}:
#' Latent heat of vaporisation (J·mol-1).
#' \item\code{utils_psychrometricConstant}: Psychrometric constant (kPa·ºC-1).
#' \item\code{utils_saturationVP}: saturation vapour pressure (in kPa).
#' \item\code{utils_saturationVaporPressureCurveSlope}: Slope of the saturation
#' vapor pressure curve (kPa·ºC-1).  }
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @references McMurtrie, R. E., D. A. Rook, and F. M. Kelliher. 1990.
#' Modelling the yield of Pinus radiata on a site limited by water and
#' nitrogen. Forest Ecology and Management 30:381–413.
#' 
#' McMahon, T. A., M. C. Peel, L. Lowe, R. Srikanthan, and T. R. McVicar. 2013.
#' Estimating actual, potential, reference crop and pan evaporation using
#' standard meteorological data: a pragmatic synthesis. Hydrology & Earth
#' System Sciences 17:1331–1363. See also:
#' http://www.fao.org/docrep/x0490e/x0490e06.htm