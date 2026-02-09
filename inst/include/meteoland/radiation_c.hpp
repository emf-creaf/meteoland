#include "meteoland/utils_c.hpp"

#ifndef RADIATION_C_H
#define RADIATION_C_H

struct SunriseSet {
  double sunrise;
  double set;
};


/**
 * Computes Julian Day from a given date.
 * Julian Day, or number of days since January 1, 4713 BCE at noon UTC.
 *
 * Code from package 'insol' (J. G. Corripio)
 * Danby, J. M. Eqn. 6.16.4 in Fundamentals of Celestial Mechanics, 2nd ed. Richmond, VA: Willmann-Bell, p. 207, 1988.
 * http://scienceworld.wolfram.com/astronomy/JulianDate.html
 */
inline int julianDay_c(int year, int month, int day) {
  int jd = 367 * year - (int)((7 * (year + (month + 9)/12))/4) + (int)((275 * month)/9) + day + 1721013.5 + 0.5;
  return(jd);
}

/**
 * Returns the solar declination (in radians)
 * Code from package 'insol' (J. G. Corripio)
 * http://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html
 * Meeus, J. 1999. Astronomical Algorithms. Willmann-Bell, Richmond, Virginia, USA.
 *
 * J - Julian days (integer), number of days since January 1, 4713 BCE at noon UTC.
 */
inline double solarDeclination_c(int J) {
  // Original calculation
  //double delta = 0.4093 * sin(2.0 * M_PI/365.0 * ((double)J) - 1.405); // solar declination in radians
  // New calculation (Bourges 1985)
  // double D = (2.0*M_PI/365.25)*(((double) J)-79.346);
  // double delta = 0.3723+23.2567*sin(D)-0.758*cos(D)+0.1149*sin(2.0*D)+0.3656*cos(2.0*D)-0.1712*sin(3.0*D)+0.00201*cos(3.0*D);
  // return(delta*(M_PI/180.0));
  //
  double jdc = (((double)J) - 2451545.0)/36525.0;
  double sec = 21.448 - jdc * (46.815 + jdc * (0.00059 - jdc * (0.001813)));
  double e0 = 23.0 + (26.0 + (sec/60.0))/60.0;
  double oblcorr = e0 + 0.00256 * cos((M_PI/180.0)*(125.04 - 1934.136 * jdc));
  double l0 = 280.46646 + jdc * (36000.76983 + jdc * (0.0003032));
  l0 = (double)((((int)l0) - 360*(((int)l0)/360))%360);
  double gmas = (M_PI/180.0)*(357.52911 + jdc * (35999.05029 - 0.0001537 * jdc));
  double seqcent = sin(gmas) * (1.914602 - jdc * (0.004817 + 1.4e-05 * jdc)) + sin(2 * gmas) * (0.019993 - 0.000101 * jdc) + sin(3 * gmas) * 0.000289;
  double suntl = l0 + seqcent;
  double sal = suntl - 0.00569 - 0.00478 * sin((M_PI/180.0)*(125.04 - 1934.136 * jdc));
  double delta = asin(sin((M_PI/180.0)*(oblcorr)) * sin((M_PI/180.0)*(sal)));
  return(delta);
}

/**
 * Calculates the earth radius vector (from package 'insol') to correct solar constant
 * Code from package 'insol' (J. G. Corripio)
 */
inline double solarConstant_c(int J) {
  double jdc = (((double)J) - 2451545.0)/36525.0; //julian century
  double ecc = 0.016708634 - jdc * (4.2037e-05 + 1.267e-07 * jdc);
  double gmasr = (M_PI/180.0)*(357.52911 + jdc * (35999.05029 - 0.0001537 * jdc));
  double seqc = sin(gmasr) * (1.914602 - jdc * (0.004817 + 1.4e-05 *
                    jdc)) + sin(2.0 * gmasr) * (0.019993 - 0.000101 * jdc) +
                    sin(3.0 * gmasr) * 0.000289;
  double star = gmasr + (M_PI/180.0)*seqc;
  double sunrv = (1.000001018 * (1.0 - pow(ecc,2.0)))/(1.0 + ecc * cos(star));
  return(GSC_kWm2*(1.0/sunrv));
}

/**
 * Calculates solar elevation angle (in radians)
 *
 * latrad - Latitude (in radians)
 * delta - Solar declination (in radians)
 * hrad - Solar hour angle (in radians)
 */
inline double solarElevation_c(double latrad, double delta, double hrad) {
  double sinb = sin(latrad)*sin(delta)+cos(latrad)*cos(delta)*cos(hrad);
  return(asin(sinb));
}


inline void sunRiseSet_c(SunriseSet& ss, double latrad, double slorad, double asprad, double delta){
  double L1 = asin(cos(slorad)*sin(latrad)+sin(slorad)*cos(latrad)*cos(asprad)); //latitude on equivalent slope
  double den = cos(slorad)*cos(latrad)-sin(slorad)*sin(latrad)*cos(asprad);
  double L2;
  if(den<0) {
    L2 = atan((sin(slorad)*sin(asprad))/den)+M_PI;
  } else {
    L2 = atan((sin(slorad)*sin(asprad))/den);
  }
  double T = acos(std::max(std::min(-tan(L1)*tan(delta),1.0),-1.0));
  double T7 = T-L2; //hour angle of sunset on equivalent slope
  double T6 = -T-L2; //hour angle of sunrise on equivalent slope
  double T1 = acos(std::max(std::min(-tan(latrad)*tan(delta),1.0),-1.0));  //hour angle of sunset on horizontal surface
  double T0 = - T1; //hour angle of sunrise on horizontal surface
  double T3 = std::min(T1,T7); //hour angle of sunset on slope
  double T2 = std::max(T0,T6); //hour angle of sunrise on slope
  
  ss.sunrise = T2;
  ss.set = T3;
}

/**
 *  Calculates day length (in hours)
 *
 *  latrad - Latitude (in radians)
 *  slorad - Inclination of slope, in radians above horizontal
 *  asprad - Azimuth of slope (aspect), in radians from north
 *  delta - Solar declination, in radians
 */
inline double daylength_c(double latrad, double slorad, double asprad, double delta) {
  SunriseSet v;
  sunRiseSet_c(v, latrad, slorad, asprad, delta); //solar hour at sunrise and sunset, in radians
  double N = 12.0/M_PI*(v.set-v.sunrise);
  return(std::max(N,0.0));
}

/**
 *  Calculates day length (in seconds)
 *
 *  latrad - Latitude (in radians)
 *  slorad - Inclination of slope, in radians above horizontal
 *  asprad - Azimuth of slope (aspect), in radians from north
 *  delta - Solar declination, in radians
 */
inline double daylengthseconds_c(double latrad, double slorad, double asprad, double delta) {
  return(daylength_c(latrad,slorad, asprad, delta)*3600.0);
}

/**
 * Returns instant radiation (in kW/m2). From Granier & Ohmura (1968)
 * Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800
 *
 *  solarConstant - Solar constant (in kW/m2)
 *  latrad - latitude of the slope, in radians
 *  A - Azimuth of slope, in radians from north
 *  Zx - Zenith angle of the vector normal to the slope (= slope?), in radians
 *  delta - Solar declination, in radians
 *  H - hour angle measured from solar noon, in radians
 *
 */
inline double RpotInstant_c(double solarConstant, double latrad, double slorad, double asprad, double delta, double H) {
  double t1 = (sin(latrad)*cos(H))*(-1.0*cos(asprad)*sin(slorad)) -1.0*sin(H)*(sin(asprad)*sin(slorad)) + (cos(latrad)*cos(H))*cos(slorad);
  double t2 = cos(latrad)*(cos(asprad)*sin(slorad)) + sin(latrad)*cos(slorad);
  return(solarConstant*(t1*cos(delta)+ t2*sin(delta)));
}


/**
 * One-day potential solar radiation (in MJ/m2). From Granier & Ohmura (1968)
 * Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800
 *
 *  solarConstant - Solar constant (in kW/m2)
 *  latrad - latitude of the slope, in radians
 *  asprad - Azimuth of slope, in radians from north
 *  slorad - Zenith angle of the vector normal to the slope (= slope) in radians
 *  J - Julian day (1 to 365)
 *  step - Number of seconds step (for integration of instant radiation)
 */
inline double RpotDay_c(double solarConstant, double latrad,  double slorad, double asprad, double delta) {
  double step = 600.0; //10 min = 600 s step
  double hradstep = step*(5.0/1200.0)*(M_PI/180.0);
  double Rpot = 0.0;
  SunriseSet srs;
  sunRiseSet_c(srs, latrad, slorad, asprad, delta);
  for(double hrad=srs.sunrise;hrad<srs.set;hrad+=hradstep) {
    Rpot += step*std::max(0.0, RpotInstant_c(solarConstant, latrad, slorad, asprad, delta, hrad));
  }
  return(Rpot/1000.0);
}

/**
 *   Estimates daily net outgoing longwave radiation from incoming solar radiation data
 *
 *   REF:  McMahon et al. (2013)
 *   Estimating actual, potential, reference crop and pan evaporation using standard meteorological data: a pragmatic synthesis
 *   Hydrology & Earth System Sciences
 *   See also:  http://www.fao.org/docrep/x0490e/x0490e06.htm
 *
 *  solarConstant - Solar constant (in kW/m2)
 *  latrad - Latitude (radians)
 *  elevation - Elevation (m)
 *  slorad - Slope in radians
 *  asprad - Aspect in radians
 *  delta - Solar declination in radians
 *  vpa - mean actual vapor pressure (kPa)
 *  tmax - Maximum temperature (Celsius)
 *  tmin - Minimum temperature (Celsius)
 *  R_s - Incident solar radiation (MJ/m2)
 */
inline double outgoingLongwaveRadiation_c(double solarConstant, double latrad, double elevation,  double slorad,  double asprad, double delta,
                                 double vpa, double tmin, double tmax, double R_s){
  double R_a = RpotDay_c(solarConstant, latrad,  slorad, asprad, delta); //Extraterrestrial (potential) radiation
  double R_so = (0.75 + (2.0 * 0.00001) * elevation) * R_a; //Clear sky radiation
  // Rcout<<R_s<<" "<<R_so<<" "<<R_s/R_so<<"\n";
  //Net outgoing longwave radiation (MJ.m^-2.day^-1)
  double R_nl = SIGMA_MJ_day * (0.34 - 0.14 * sqrt(vpa)) * (pow(tmax + 273.2,4.0) + pow(tmin + 273.2,4.0))/2.0 * (1.35 * std::min(R_s/R_so,1.0) - 0.35);
  return(R_nl);
}


/**
 * One-day solar radiation (in MJ/m2). From Thornton & Running (1999)
 *
 *  solarConstant - Solar constant (in kW/m2)
 *  latrad - latitude of the slope, in radians
 *  elevation - Elevation from sea level (in meters)
 *  slorad - Zenith angle of the vector normal to the slope (= slope?) in radians
 *  asprad - Azimuth of slope, in radians from north
 *  delta - Solar declination (in radians)
 *  diffTemp - Difference between maximum and minimum temperature (in degrees Celsius)
 *  vpa - Vapor pressure (in kPa)
 *  precipitation - Precipitation (in mm of water)
 *
 *  Bibliography:
 *  Thornton, P.E., Running, S.W., 1999. An improved algorithm for estimating incident daily solar radiation from measurements of temperature, humidity, and precipitation. Agric. For. Meteorol. 93, 211–228. doi:10.1016/S0168-1923(98)00126-9
 *  Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800
 *  Pearcy, R.W., Ehleringer, J.R., Mooney, H.A., Rundel, P.W., 1991. Plant Physiological Ecology: Field Methods and Instrumentation. Chapman & Hall, New York 455 pp.
 */
inline double RDay_c(double solarConstant, double latrad, double elevation, double slorad, double asprad, double delta,
                     double diffTemp, double diffTempMonth, double vpa, double precipitation) {
  //Number of seconds step (for integration of instant potential radiation and maximum transmittance)
  double step = 600.0; //10 min = 600 s step
  double Rpot = 0.0;
  double RpotFlat = 0.0; //Rpot for a flat surface (needed for diffuse radiation)
  double B = 0.031+0.201*exp(-0.185*diffTempMonth);
  double Tfmax = 1.0-0.9*exp(-B*pow(diffTemp,1.5));
  if(std::isnan(precipitation)) precipitation = 0.0;
  if(precipitation>0.0) Tfmax *=0.75; //Correction for wet days
  double Ttmax = 0.0, RpotInst = 0.0, RpotInstFlat = 0.0, costheta=0.0;
  double pratio = pow(1.0 -2.2569e-5*elevation,5.2553); // from Pearcy et al. 1991
  double hradstep = step*(5.0/1200.0)*(M_PI/180.0);
  SunriseSet srsFlat;
  sunRiseSet_c(srsFlat, latrad, 0.0, 0.0, delta);
  for(double hrad=srsFlat.sunrise;hrad<srsFlat.set;hrad+=hradstep) {
    RpotInstFlat = std::max(0.0,RpotInstant_c(solarConstant, latrad, 0.0, 0.0, delta, hrad));
    RpotFlat += step*RpotInstFlat;
    //Solar zenith angle = 90º - solar elevation angle
    //cos(solar zenith angle) = sin(solar elevation angle)
    costheta = sin(solarElevation_c(latrad,delta, hrad));
    if(costheta>0.0) {
      Ttmax +=step*RpotInstFlat*pow(0.87,pratio*(1.0/costheta));
    }
  }
  SunriseSet srsSlope;
  sunRiseSet_c(srsSlope, latrad, slorad, asprad, delta);
  for(double hrad=srsSlope.sunrise;hrad<srsSlope.set;hrad+=hradstep) {
    RpotInst = std::max(0.0,RpotInstant_c(solarConstant, latrad, slorad, asprad, delta, hrad));
    Rpot += step*RpotInst;
  }
  Ttmax = (Ttmax/RpotFlat) -6.1e-2*vpa; //vpa in kPa
  // Rcout<<Rpot<<" "<<RpotFlat<<" "<<Ttmax<<" "<<Tfmax<<"\n";
  if(RpotFlat==0.0) return(0.0);
  double dirdifCorrected = (Rpot/1000.0)*Ttmax*Tfmax;
  double difCorrected = 0.3*(RpotFlat/1000.0)*(1.0-Ttmax*Tfmax); //Campbell, G.S., & Norman, J.M. 1998. AN INTRODUCTION TO ENVIRONMENTAL BIOPHYSICS.: 2nd edition.
  return(std::max(dirdifCorrected, difCorrected)); //Radiation in MJ/m2
}

/**
 *  Estimates instantaneous thermal (long-wave) radiation from the sky (i.e. counterradiation)
 *  Units: W/m2
 *
 *  Campbell, G.S., & Norman, J.M. 1998. AN INTRODUCTION TO ENVIRONMENTAL BIOPHYSICS.: 2nd edition.
 *
 *  Tair - Air temperature (ºC)
 *  vpa - water vapour pressure (kPa)
 *  c - fraction of sky covered by clouds
 */

inline double skyLongwaveRadiation_c(double Tair, double vpa, double c = 0) {
  double Tkelv = Tair+273.0;
  double bb = SIGMA_Wm2*pow(Tkelv,4.0); //eq. 10.7 (Stefan-Boltzmann law for blackbodies)
  double eac = 1.72 * pow(vpa/Tkelv,1.0/7.0); // eq. 10.10
  double ea = (1.0 - 0.84*c)*eac + 0.84*c; //eq.10.12
  return(ea*bb); // eq. 10.9 Energy emitted by gray bodies (no dependence of emissivity on wavelength)
}
/**
 *   Estimates daily net solar radiation from incoming solar radiation data
 *
 *   REF:  McMahon et al. (2013)
 *   Estimating actual, potential, reference crop and pan evaporation using standard meteorological data: a pragmatic synthesis
 *   Hydrology & Earth System Sciences
 *   See also:  http://www.fao.org/docrep/x0490e/x0490e06.htm
 *
 *  solarConstant - Solar constant (in kW/m2)
 *  latrad - Latitude (radians)
 *  elevation - Elevation (m)
 *  slorad - Slope in radians
 *  asprad - Aspect in radians
 *  delta - Solar declination in radians
 *  vpa - mean actual vapor pressure (kPa)
 *  tmax - Maximum temperature (Celsius)
 *  tmin - Minimum temperature (Celsius)
 *  R_s - Incident solar radiation (MJ/m2)
 *  alpha - Albedo (from 0 to 1)
 */
inline double netRadiation_c(double solarConstant, double latrad,  double elevation, double slorad, double asprad, double delta,
                    double vpa, double tmin, double tmax, double R_s,
                    double alpha = 0.08) {
  
  //Net outgoing longwave radiation (MJ.m^-2.day^-1)
  double R_nl = outgoingLongwaveRadiation_c(solarConstant, latrad, elevation, slorad, asprad, delta,
                                          vpa, tmin, tmax, R_s);
  //Net incoming shortwave radiation (MJ.m^-2.day^-1, after acounting for surface albedo)
  double R_ns = (1.0 - alpha) * R_s;
  //Net radiation
  return(std::max(0.0,R_ns - R_nl));
}
#endif