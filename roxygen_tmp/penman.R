#' Potential evapotranspiration
#' 
#' Functions to calculate potential evapotranspiration using Penman or
#' Penman-Monteith.
#' 
#' The code was adapted from package `Evapotranspiration', which follows
#' McMahon et al. (2013). If wind speed is not available, an alternative
#' formulation for potential evapotranspiration is used as an approximation
#' (Valiantzas 2006)
#' 
#' @aliases penman penmanmonteith
#' @param latrad Latitude in radians.
#' @param elevation Elevation (in m).
#' @param slorad Slope (in radians).
#' @param asprad Aspect (in radians from North).
#' @param J Julian day, number of days since January 1, 4713 BCE at noon UTC.
#' @param Tmax Maximum temperature (degrees Celsius).
#' @param Tmin Minimum temperature (degrees Celsius).
#' @param RHmin Minimum relative humidity (percent).
#' @param RHmax Maximum relative humidity (percent).
#' @param R_s Solar radiation (MJ/m2).
#' @param u With wind speed (m/s).
#' @param z Wind measuring height (m).
#' @param z0 Roughness height (m).
#' @param alpha Albedo.
#' @param windfun Wind speed function version, either "1948" or "1956".
#' @param rc Canopy vapour flux (stomatal) resistance (s·m-1).
#' @param Rn Daily net radiation (MJ·m-2·day-1).
#' @return Potential evapotranspiration (in mm of water).
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' @seealso \code{\link{interpolationpoints}}
#' @references Penman, H. L. 1948. Natural evaporation from open water, bare
#' soil and grass. Proceedings of the Royal Society of London. Series A.
#' Mathematical and Physical Sciences, 193, 120-145.
#' 
#' Penman, H. L. 1956. Evaporation: An introductory survey. Netherlands Journal
#' of Agricultural Science, 4, 9-29.
#' 
#' McMahon, T.A., Peel, M.C., Lowe, L., Srikanthan, R., McVicar, T.R. 2013.
#' Estimating actual, potential, reference crop and pan evaporation using
#' standard meteorological data: a pragmatic synthesis. Hydrology \& Earth
#' System Sciences 17, 1331–1363. doi:10.5194/hess-17-1331-2013.
