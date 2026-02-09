// [[Rcpp::interfaces(r,cpp)]]
#include <Rcpp.h>
#include <meteoland/utils_c.hpp>
using namespace Rcpp;


//' Physical utility functions
//'
//' Set of functions used in the calculation of physical variables.
//'
//'
//' @aliases utils_airDensity utils_atmosphericPressure utils_averageDailyVP
//' utils_averageDaylightTemperature utils_latentHeatVaporisation
//' utils_latentHeatVaporisationMol utils_psychrometricConstant
//' utils_saturationVP utils_saturationVaporPressureCurveSlope
//' @param temperature Air temperature (ºC).
//' @param Tmin,Tmax Minimum and maximum daily temperature (ºC).
//' @param RHmin,RHmax Minimum and maximum relative humidity (%).
//' @param Patm Atmospheric air pressure (in kPa).
//' @param elevation Elevation above sea level (in m).
//' @return Values returned for each function are: \itemize{
//' \item{\code{utils_airDensity}: air density (in kg·m-3).}
//' \item{\code{utils_atmosphericPressure}: Air atmospheric pressure (in kPa).}
//' \item{\code{utils_averageDailyVP}: average (actual) vapour pressure (in kPa).}
//' \item{\code{utils_averageDaylightTemperature}: average daylight air
//' temperature (in ºC). \item\code{utils_latentHeatVaporisation}: Latent heat
//' of vaporisation (MJ·kg-1).  \item\code{utils_latentHeatVaporisationMol}:
//' Latent heat of vaporisation (J·mol-1).}
//' \item{\code{utils_psychrometricConstant}: Psychrometric constant (kPa·ºC-1).}
//' \item{\code{utils_saturationVP}: saturation vapour pressure (in kPa).}
//' \item{\code{utils_saturationVaporPressureCurveSlope}: Slope of the saturation
//' vapor pressure curve (kPa·ºC-1).}  }
//' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
//' @references McMurtrie, R. E., D. A. Rook, and F. M. Kelliher. 1990.
//' Modelling the yield of Pinus radiata on a site limited by water and
//' nitrogen. Forest Ecology and Management 30:381–413.
//'
//' McMahon, T. A., M. C. Peel, L. Lowe, R. Srikanthan, and T. R. McVicar. 2013.
//' Estimating actual, potential, reference crop and pan evaporation using
//' standard meteorological data: a pragmatic synthesis. Hydrology & Earth
//' System Sciences 17:1331–1363. See also:
//' http://www.fao.org/docrep/x0490e/x0490e06.htm
//' @export
// [[Rcpp::export("utils_saturationVP")]]
double saturationVapourPressure(double temperature) {
  return(saturationVapourPressure_c(temperature));
}



//' @describeIn utils_saturationVP Average daily VP
//' @export
// [[Rcpp::export("utils_averageDailyVP")]]
double averageDailyVapourPressure(double Tmin, double Tmax, double RHmin, double RHmax) {
  return(averageDailyVapourPressure_c(Tmin, Tmax, RHmin, RHmax));
}

//' @describeIn utils_saturationVP Atmospheric pressure
//' @export
// [[Rcpp::export("utils_atmosphericPressure")]]
double atmosphericPressure(double elevation) {
  return(atmosphericPressure_c(elevation));
}

//' @describeIn utils_saturationVP Air density
//' @export
// [[Rcpp::export("utils_airDensity")]]
double airDensity(double temperature, double Patm) {
  return(airDensity_c(temperature, Patm));
}

//' @describeIn utils_saturationVP Daylight temperature
//' @export
// [[Rcpp::export("utils_averageDaylightTemperature")]]
double averageDaylightTemperature(double Tmin, double Tmax) {
  return(averageDaylightTemperature_c(Tmin, Tmax));
}


//' @describeIn utils_saturationVP latent heat vaporisation
//' @export
// [[Rcpp::export("utils_latentHeatVaporisation")]]
double latentHeatVaporisation(double temperature) {
  return(latentHeatVaporisation_c(temperature));
}

//' @describeIn utils_saturationVP Heat vaporisation mol
//' @export
// [[Rcpp::export("utils_latentHeatVaporisationMol")]]
double latentHeatVaporisationMol(double temperature) {
  return(latentHeatVaporisationMol_c(temperature));
}



//' @describeIn utils_saturationVP psychrometric constant
//' @export
// [[Rcpp::export("utils_psychrometricConstant")]]
double psychrometricConstant(double temperature, double Patm) {
  return(psychrometricConstant_c(temperature, Patm));
}

//' @describeIn utils_saturationVP Saturation VP curve slope
//' @export
// [[Rcpp::export("utils_saturationVaporPressureCurveSlope")]]
double saturationVaporPressureCurveSlope(double temperature) {
  return(saturationVaporPressureCurveSlope_c(temperature));
}


double dewpointTemperatureFromRH(double T, double RH) {
  return(dewpointTemperatureFromRH_c(T,RH));
}
