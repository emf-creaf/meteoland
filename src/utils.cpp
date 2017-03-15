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
// [[Rcpp::export("utils_saturationVP")]]
double saturationVapourPressure(double temperature) {
  return(0.61078 * exp(17.269 * temperature/(temperature + 237.3)));
}

/**
 * Temperature (ºC) to vapor pressure (in kPa)
 * Murray (1967) formulation
 */
double temp2SVP(double TD) {
  return(0.61078*exp((17.269*TD)/(237.3+TD)));
}

/**
 * Calculates relative humidity from temperature
 * T - Average temperature (in Celsius)
 * TD - Dewpoint temperature (in Celsius)
 */
double relativeHumidity(double T,double TD) {
  return(std::min(100.0*(temp2SVP(TD)/temp2SVP(T)),100.0));
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
// [[Rcpp::export("utils_airDensity")]]
double airDensity(double temperature, double Patm) {
  return((Patm/(1.01*(temperature+273.16)*0.287)));
}
/**
*  Calculates average daylight air temperature (in celsius)
*  
*  Tmin, Tmax - Minimum and maximum temperature (in degrees Celsius) 
*/
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
    PET = 0.047 * R_s * sqrt(Tday + 9.5) - 2.4 * pow(R_s/R_a,2.0) + 0.09 * (Tday + 20.0) * (1.0 - RHmean/100.0);
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

