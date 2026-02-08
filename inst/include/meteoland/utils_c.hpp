#ifndef UTILS_C_H
#define UTILS_C_H

inline const double Cp_MJKG = 0.00101386;// MJ * kg^-1 * ºC^-1

/**
 *  Calculates saturation vapour pressure (in kPa)
 *
 *  temperature - temperature (in degrees Celsius)
 */
inline double saturationVapourPressure_c(double temperature) {
  return(0.61078 * exp(17.269 * temperature/(temperature + 237.3)));
}

inline double temp2SVP_c(double TD) {
  if(!std::isnan(TD)) return(0.61078*exp((17.269*TD)/(237.3+TD)));
  return(TD);
}
inline double relativeHumidity_c(double T,double TD) {
  if((!std::isnan(T)) && (!std::isnan(TD))) return(std::min(100.0*(temp2SVP_c(TD)/temp2SVP_c(T)),100.0));
  return(T);
}

/**
 * Calculates vapor pressure (kPa) from T and relative humidity
 */
inline double vapourPressureFromRH_c(double T, double RH) {
  return(temp2SVP_c(T)*(RH/100.0));
}

double dewpointTemperatureFromRH_c(double T, double RH) {
  double vp = vapourPressureFromRH_c(T,RH);
  return(std::min((237.3*log(vp/0.61078))/(17.269-log(vp/0.61078)),40.0));
}
/**
*  Calculates average vapour pressure (in kPa)
*
*  Tmin, Tmax - Minimum and maximum temperature (in degrees Celsius)
*  RHmin, RHmax - Minimum and maximum relative humidity (in percent)
*/
inline double averageDailyVapourPressure_c(double Tmin, double Tmax, double RHmin, double RHmax) {
  double vs_Tmax = saturationVapourPressure_c(Tmax);
  double vs_Tmin = saturationVapourPressure_c(Tmin);
  return((vs_Tmin * (RHmax/100.0) + vs_Tmax * (RHmin/100.0))/2.0);
}

/**
 *  Calculates air atmospheric pressure (in kPa)
 *
 *  z - Elevation (in m)
 */
inline double atmosphericPressure_c(double elevation) {
  return(101.32500*pow(1.0-2.2569*pow(10.0,-5)*elevation,5.2353));
}
/**
*  Calculates air density (in kg/m3)
*
*  temperature - Air temperature (in degrees Celsius)
*  Patm - air atmospheric pressure (in kPa)
*/
inline double airDensity_c(double temperature, double Patm) {
  return((Patm/(1.01*(temperature+273.16)*0.287)));
}
/**
*  Calculates average daylight air temperature (in celsius)
*
*  Tmin, Tmax - Minimum and maximum temperature (in degrees Celsius)
*/
inline double averageDaylightTemperature_c(double Tmin, double Tmax) {
  return(0.606*Tmax + 0.394*Tmin);
}


/**
*  Calculates latent heat of vaporisation (lambda)
*
*  temperature - Temperature (in ºC)
*
*  Units: MJ.kg^-1
*/
double latentHeatVaporisation_c(double temperature) {
  return(2.5023 - 0.00243054*temperature);
}
/**
*  Calculates latent heat of vaporisation (lambda)
*
*  temperature - Temperature (in ºC)
*
*  Units: J.mol^-1
*/
double latentHeatVaporisationMol_c(double temperature) {
  return(latentHeatVaporisation_c(temperature)*pow(10.0,6.0)*0.018);
}



/**
*  Calculates psychrometric constant (gamma)
*
*  temperature - Temperature (in ºC)
*  Patm - air pressure (in kPa)
*
*  Units: kPa*ºC-1
*/
double psychrometricConstant_c(double temperature, double Patm) {
  return((0.00163*Patm)/latentHeatVaporisation_c(temperature));
}

/**
*  Slope of the saturation vapor pressure curve (Delta)
*
*  temperature - Temperature (in ºC)
*
*  Units: kPa*ºC-1
*/
double saturationVaporPressureCurveSlope_c(double temperature) {
  return(4098.0 * (0.6108 * exp((17.27 * temperature)/(temperature + 237.3)))/pow(temperature + 237.3,2.0));
}


#endif
