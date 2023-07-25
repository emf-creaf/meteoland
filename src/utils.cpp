// [[Rcpp::interfaces(r,cpp)]]

#include <Rcpp.h>
#include "radiation.h"
using namespace Rcpp;

const double Cp_MJKG = 0.00101386;// MJ * kg^-1 * ºC^-1

/**
 *  Calculates saturation vapour pressure (in kPa)
 *
 *  temperature - temperature (in degrees Celsius)
 */
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
  return(0.61078 * exp(17.269 * temperature/(temperature + 237.3)));
}

/**
 * Temperature (ºC) to vapor pressure (in kPa)
 * Murray (1967) formulation
 */
double temp2SVP(double TD) {
  if(!NumericVector::is_na(TD)) return(0.61078*exp((17.269*TD)/(237.3+TD)));
  return(NA_REAL);
}

/**
 * Calculates relative humidity from temperature
 * T - Average temperature (in Celsius)
 * TD - Dewpoint temperature (in Celsius)
 */
double relativeHumidity(double T,double TD) {
  if((!NumericVector::is_na(T)) && (!NumericVector::is_na(TD))) return(std::min(100.0*(temp2SVP(TD)/temp2SVP(T)),100.0));
  return(NA_REAL);
}

/**
 * Calculates vapor pressure (kPa) from T and relative humidity
 */
double vapourPressureFromRH(double T, double RH) {
  return(temp2SVP(T)*(RH/100.0));
}

double dewpointTemperatureFromRH(double T, double RH) {
  double vp = vapourPressureFromRH(T,RH);
  return(std::min((237.3*log(vp/0.61078))/(17.269-log(vp/0.61078)),40.0));
}
/**
*  Calculates average vapour pressure (in kPa)
*
*  Tmin, Tmax - Minimum and maximum temperature (in degrees Celsius)
*  RHmin, RHmax - Minimum and maximum relative humidity (in percent)
*/
//' @describeIn utils_saturationVP Average daily VP
//' @export
// [[Rcpp::export("utils_averageDailyVP")]]
double averageDailyVapourPressure(double Tmin, double Tmax, double RHmin, double RHmax) {
  double vs_Tmax = saturationVapourPressure(Tmax);
  double vs_Tmin = saturationVapourPressure(Tmin);
  return((vs_Tmin * (RHmax/100.0) + vs_Tmax * (RHmin/100.0))/2.0);
}

/**
 *  Calculates air atmospheric pressure (in kPa)
 *
 *  z - Elevation (in m)
 */
//' @describeIn utils_saturationVP Atmospheric pressure
//' @export
// [[Rcpp::export("utils_atmosphericPressure")]]
double atmosphericPressure(double elevation) {
  return(101.32500*pow(1.0-2.2569*pow(10.0,-5)*elevation,5.2353));
}
/**
*  Calculates air density (in kg/m3)
*
*  temperature - Air temperature (in degrees Celsius)
*  Patm - air atmospheric pressure (in kPa)
*/
//' @describeIn utils_saturationVP Air density
//' @export
// [[Rcpp::export("utils_airDensity")]]
double airDensity(double temperature, double Patm) {
  return((Patm/(1.01*(temperature+273.16)*0.287)));
}
/**
*  Calculates average daylight air temperature (in celsius)
*
*  Tmin, Tmax - Minimum and maximum temperature (in degrees Celsius)
*/
//' @describeIn utils_saturationVP Daylight temperature
//' @export
// [[Rcpp::export("utils_averageDaylightTemperature")]]
double averageDaylightTemperature(double Tmin, double Tmax) {
  return(0.606*Tmax + 0.394*Tmin);
}


/**
*  Calculates latent heat of vaporisation (lambda)
*
*  temperature - Temperature (in ºC)
*
*  Units: MJ.kg^-1
*/
//' @describeIn utils_saturationVP latent heat vaporisation
//' @export
// [[Rcpp::export("utils_latentHeatVaporisation")]]
double latentHeatVaporisation(double temperature) {
  return(2.5023 - 0.00243054*temperature);
}
/**
*  Calculates latent heat of vaporisation (lambda)
*
*  temperature - Temperature (in ºC)
*
*  Units: J.mol^-1
*/
//' @describeIn utils_saturationVP Heat vaporisation mol
//' @export
// [[Rcpp::export("utils_latentHeatVaporisationMol")]]
double latentHeatVaporisationMol(double temperature) {
  return(latentHeatVaporisation(temperature)*pow(10.0,6.0)*0.018);
}



/**
*  Calculates psychrometric constant (gamma)
*
*  temperature - Temperature (in ºC)
*  Patm - air pressure (in kPa)
*
*  Units: kPa*ºC-1
*/
//' @describeIn utils_saturationVP psychrometric constant
//' @export
// [[Rcpp::export("utils_psychrometricConstant")]]
double psychrometricConstant(double temperature, double Patm) {
  return((0.00163*Patm)/latentHeatVaporisation(temperature));
}

/**
*  Slope of the saturation vapor pressure curve (Delta)
*
*  temperature - Temperature (in ºC)
*
*  Units: kPa*ºC-1
*/
//' @describeIn utils_saturationVP Saturation VP curve slope
//' @export
// [[Rcpp::export("utils_saturationVaporPressureCurveSlope")]]
double saturationVaporPressureCurveSlope(double temperature) {
  return(4098.0 * (0.6108 * exp((17.27 * temperature)/(temperature + 237.3)))/pow(temperature + 237.3,2.0));
}


/**
 * Daily PET
 * Recoded from ET.Penman function in package 'Evapotranspiration'
 *
 * latrad - Latitude (radians)
 * elevation - Elevation (m)
 * slorad - Slope in radians
 * asprad - Aspect in radians
 * J - Julian day
 * Tmax - Maximum temperature (Celsius)
 * Tmin - Minimum temperature (Celsius)
 * RHmax - Maximum relative humidity (%)
 * RHmin - Minimum relative humidity (%)
 * R_s - Incident solar radiation (MJ/m2)
 * u - wind speed (m/s)
 * z - wind measuring height (m)
 * z0 - Roughness height (m)
 * alpha - Albedo (from 0 to 1)
 * windfun - Wind function version of Penman "1956" or "1948"
 */
//' Potential evapotranspiration
//'
//' Functions to calculate potential evapotranspiration using Penman or
//' Penman-Monteith.
//'
//' The code was adapted from package `Evapotranspiration', which follows
//' McMahon et al. (2013). If wind speed is not available, an alternative
//' formulation for potential evapotranspiration is used as an approximation
//' (Valiantzas 2006)
//'
//' @aliases penman penmanmonteith
//' @param latrad Latitude in radians.
//' @param elevation Elevation (in m).
//' @param slorad Slope (in radians).
//' @param asprad Aspect (in radians from North).
//' @param J Julian day, number of days since January 1, 4713 BCE at noon UTC.
//' @param Tmax Maximum temperature (degrees Celsius).
//' @param Tmin Minimum temperature (degrees Celsius).
//' @param RHmin Minimum relative humidity (percent).
//' @param RHmax Maximum relative humidity (percent).
//' @param R_s Solar radiation (MJ/m2).
//' @param u With wind speed (m/s).
//' @param z Wind measuring height (m).
//' @param z0 Roughness height (m).
//' @param alpha Albedo.
//' @param windfun Wind speed function version, either "1948" or "1956".
//' @param rc Canopy vapour flux (stomatal) resistance (s·m-1).
//' @param Rn Daily net radiation (MJ·m-2·day-1).
//' @return Potential evapotranspiration (in mm of water).
//' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
//' @seealso \code{\link{interpolate_data}}
//' @references Penman, H. L. 1948. Natural evaporation from open water, bare
//' soil and grass. Proceedings of the Royal Society of London. Series A.
//' Mathematical and Physical Sciences, 193, 120-145.
//'
//' Penman, H. L. 1956. Evaporation: An introductory survey. Netherlands Journal
//' of Agricultural Science, 4, 9-29.
//'
//' McMahon, T.A., Peel, M.C., Lowe, L., Srikanthan, R., McVicar, T.R. 2013.
//' Estimating actual, potential, reference crop and pan evaporation using
//' standard meteorological data: a pragmatic synthesis. Hydrology & Earth
//' System Sciences 17, 1331–1363. doi:10.5194/hess-17-1331-2013.
//' @export
// [[Rcpp::export("penman")]]
double PenmanPET(double latrad, double elevation, double slorad, double asprad, int J,
                 double Tmin, double Tmax, double RHmin, double RHmax, double R_s,
                 double u, double z = 2.0, double z0 = 0.001,
                 double alpha = 0.08, String windfun = "1956") {
  double Tday = (Tmax + Tmin)/2.0;
  double RHmean = (RHmax + RHmin)/2.0;
  //Atmospheric pressure kPa
  double Patm = atmosphericPressure(elevation);
  //Slope of the saturation vapor pressure curve kPa.Celsius^-1
  double delta = saturationVaporPressureCurveSlope(Tday);
  //Latent heat of vaporisation MJ.kg^-1
  double lambda = latentHeatVaporisation(Tday);
  //Psychrometric constant kPa.Celsius^-1
  double gamma = psychrometricConstant(Tday, Patm);
  //Solar declination
  double delta2 =  solarDeclination(J);
  //Solar constant
  double Gsc = solarConstant(J);
  // double d_r2 = 1.0 + 0.033 * cos(2.0 * PI/365.0 * ((double)J));
  // double w_s = acos(-tan(latitude) * tan(delta2));
  // double N = 24.0/PI*w_s; (N not used)

  double PET = 0.0;
  if(!NumericVector::is_na(u)) {
    double u2 = u * log(2.0/z0)/log(z/z0);
    double vs_Tmax = saturationVapourPressure(Tmax);
    double vs_Tmin = saturationVapourPressure(Tmin);
    double vas = (vs_Tmax + vs_Tmin)/2.0;
    double vpa = (vs_Tmin * (RHmax/100.0) + vs_Tmax * (RHmin/100.0))/2.0;
    double R_n = netRadiation(Gsc, latrad, elevation, slorad, asprad, delta2,
                              vpa, Tmin, Tmax, R_s,
                              alpha);//Net radiation
    double Ea = (vas - vpa); //Saturation vapor deficit
    if(windfun == "1956") {
      Ea = Ea*(1.313 + 1.381 * u2);
    } else if(windfun == "1948") {
      Ea = Ea*(2.626 + 1.381*u2);
    }
    PET = delta/(delta + gamma) * (R_n/lambda) + gamma/(delta + gamma) * Ea;
  } else {
    //Equation by Valiantzas (2006, eq 33) for situations where wind is not available
    //Valiantzas JD (2006) Simplified versions for the Penman evaporation equation using routine weather data. Journal of Hydrology 331, 690–702. doi:10.1016/j.jhydrol.2006.06.012.
    double R_a = RpotDay(Gsc, latrad,  slorad, asprad, delta2); //Extraterrestrial (potential) radiation
    double R_ratio = R_s/R_a;
    if(R_a==0.0) R_ratio = 0.0;
    R_ratio = std::min(std::max(R_ratio, 0.0), 1.0);
    double wf = 0.09;
    if(windfun == "1956") {
      wf = 0.06;
    } else if(windfun == "1948") {
      wf = 0.09;
    }
    PET = 0.047 * R_s * sqrt(Tday + 9.5) - 2.4 * pow(R_ratio,2.0) + wf * (Tday + 20.0) * (1.0 - RHmean/100.0);
  }
  if(PET<0.0) PET = 0.0;
  return(PET);
}

/**
*   Calculates daily potential evapotranspiration (mm.m^-2.day^-1) for a surface given microenvironmental conditions
*   (absorbed radiation, wind, temperature, relative humidity) and given a canopy stomatal resistance.
*   Basically is the Penman-Monteith equation for daily steps, where aerodynamic resistance to heat flux
*   is calculated internally (from wind and temperature) and the canopy (stomatal) resistance is an input.
*
*
*   REF:  McMahon et al. (2013)
*   Estimating actual, potential, reference crop and pan evaporation using standard meteorological data: a pragmatic synthesis
*   Hydrology & Earth System Sciences
*   See also:  http://www.fao.org/docrep/x0490e/x0490e06.htm
*
*   rc[s/m] - Canopy vapour flux (stomatal) resistance
*   elevation [m] - Elevation over sea level
*   Tmin, Tmax [Celsius] - Minimum and maximum temperature
*   RHmin, RHmax [%] - Minimum and maximum relative humidity
*   Rn [MJ.m^-2.day^-1] - daily net radiation
*   u [m.s^-1] - wind speed
*/
//' @describeIn penman Penman Monteith method
//' @export
// [[Rcpp::export("penmanmonteith")]]
double PenmanMonteithPET(double rc, double elevation,
                         double Tmin, double Tmax,
                         double RHmin, double RHmax,
                         double Rn, double u = NA_REAL) {
  double Tday = (Tmax + Tmin)/2.0;
  //Saturation vapour pressure at temperature kPa
  double vs_Tmax = saturationVapourPressure(Tmax);
  double vs_Tmin = saturationVapourPressure(Tmin);
  //Daily saturation vapour pressure kPa
  double vas = (vs_Tmax + vs_Tmin)/2.0;
  //Mean daily actual vapour pressure, kPa
  double vabar = (vs_Tmin * RHmax/100.0 + vs_Tmax * RHmin/100.0)/2.0;
  //Atmospheric pressure kPa
  double Patm = atmosphericPressure(elevation);
  //Slope of the saturation vapor pressure curve kPa.Celsius^-1
  double delta = saturationVaporPressureCurveSlope(Tday);
  //Vapour pressure deficit, kPa
  double vpd = (vas - vabar);
  //Latent heat of vaporisation MJ.kg^-1
  double lambda = latentHeatVaporisation(Tday);
  // Rcout<<"Lambda: "<< lambda<<"\n";
  //Psychrometric constant kPa.Celsius^-1
  double gamma = psychrometricConstant(Tday, Patm);
  // Rcout<<"Gamma: "<< gamma<<"\n";
  //Density of air kg.m^-3
  double rho = airDensity(Tday, Patm);
  //Unit conversion from s^-1 to day^-1
  double Kt = 86400.0;

  //Radiative resistance to heat flux s.m^-1
  // double rr = Kt*(rho*Cp)/(4.0*SIGMA*pow(Tday+273.0,3.0));
  //Convective resistance to heat flux s.m^-1
  if(NumericVector::is_na(u)) u = 2.0;
  u = std::max(u, 0.000001);
  double rh = 208.0/u; //Aerodynamic resistance to convective heat transfer
  //Resistance to sensible heat flux (from Biome BGC) s.m^-1
  // double ra = (rh*rr)/(rh+rr);
  rc = std::max(rc, 70.0);
  double ra = rh;
  //   double N1fao = (0.408*delta*(Rn));
  //   double N2fao = gamma*(900.0/(Tday+273.0))*u*vpd;
  //   double Dfao = delta + gamma*(1.0+0.34*u);
  //   double Efao = (N1fao+N2fao)/(Dfao);
  //   Rcout<<"N1(FAO) : "<<N1fao<< " N2(FAO) : "<<N2fao<<" D(FAO) "<<Dfao<<" E(FAO) "<< Efao <<"\n";
  double N1 = ((delta/lambda)*Rn);
  double N2 = Kt*(vpd*(rho*Cp_MJKG)/(lambda*ra));
  double D = delta + gamma*(1.0+(rc/ra));
  double E = (N1+ N2)/(D);
  // Rcout<<"N1 : "<<N1<< " N2 : "<<N2<<" D "<<D<<" E "<< E <<"\n";
  return(E);
}

