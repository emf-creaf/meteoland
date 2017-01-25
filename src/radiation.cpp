// [[Rcpp::interfaces(r,cpp)]]
#include <Rcpp.h>
using namespace Rcpp;


const double GSC_MJminm2 = 0.0820; //solar constant in MJ/m2/min
const double GSC_kWm2 = 1.361; //in kW/m2
const double SIGMA_MJ_day = 4.903*pow(10,-9.0); //Stefan-Boltzmann constant MJ/K^4/m2/day

/**
 * Computes Julian Day from a given date.
 * Julian Day, or number of days since January 1, 4713 BCE at noon UTC.
 * 
 * Code from package 'insol' (J. G. Corripio)
 * Danby, J. M. Eqn. 6.16.4 in Fundamentals of Celestial Mechanics, 2nd ed. Richmond, VA: Willmann-Bell, p. 207, 1988.
 * http://scienceworld.wolfram.com/astronomy/JulianDate.html
 */
// [[Rcpp::export("radiation_julianDay")]]
int julianDay(int year, int month, int day) {
  int jd = 367 * year - (int)((7 * (year + (month + 9)/12))/4) + (int)((275 * month)/9) + day + 1721013.5 + 0.5;
  return(jd);
}

// [[Rcpp::export("radiation_dateStringToJulianDays")]]
IntegerVector dateStringToJulianDays(CharacterVector dateStrings) {
  int numDays = dateStrings.size();
  IntegerVector jd(numDays);
  for(int i=0;i<numDays;i++) {
    std::string c = as<std::string>(dateStrings[i]);
    jd[i] = julianDay(std::atoi(c.substr(0, 4).c_str()),std::atoi(c.substr(5,2).c_str()),std::atoi(c.substr(8,2).c_str()));
  }
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
// [[Rcpp::export("radiation_solarDeclination")]]
double solarDeclination(int J) {
  // Original calculation
  //double delta = 0.4093 * sin(2.0 * PI/365.0 * ((double)J) - 1.405); // solar declination in radians
  // New calculation (Bourges 1985)
  // double D = (2.0*PI/365.25)*(((double) J)-79.346);
  // double delta = 0.3723+23.2567*sin(D)-0.758*cos(D)+0.1149*sin(2.0*D)+0.3656*cos(2.0*D)-0.1712*sin(3.0*D)+0.00201*cos(3.0*D);
  // return(delta*(PI/180.0));
  //
  double jdc = (((double)J) - 2451545.0)/36525.0;
  double sec = 21.448 - jdc * (46.815 + jdc * (0.00059 - jdc * (0.001813)));
  double e0 = 23.0 + (26.0 + (sec/60.0))/60.0;
  double oblcorr = e0 + 0.00256 * cos((PI/180.0)*(125.04 - 1934.136 * jdc));
  double l0 = 280.46646 + jdc * (36000.76983 + jdc * (0.0003032));
  l0 = (double)((((int)l0) - 360*(((int)l0)/360))%360);
  double gmas = (PI/180.0)*(357.52911 + jdc * (35999.05029 - 0.0001537 * jdc));
  double seqcent = sin(gmas) * (1.914602 - jdc * (0.004817 + 1.4e-05 * jdc)) + sin(2 * gmas) * (0.019993 - 0.000101 * jdc) + sin(3 * gmas) * 0.000289;
  double suntl = l0 + seqcent;
  double sal = suntl - 0.00569 - 0.00478 * sin((PI/180.0)*(125.04 - 1934.136 * jdc));
  double delta = asin(sin((PI/180.0)*(oblcorr)) * sin((PI/180.0)*(sal)));
  return(delta);
}

/**
 * Returns the sunrise and sunset hours in hour angle (radians)
 * 
 * latrad - Latitude of actual slope, in radians
 * slorad - Inclination of slope, in radians above horizontal
 * asprad - Azimuth of slope (aspect), in radians from north
 * delta - Solar declination, in radians
 */
// [[Rcpp::export("radiation_sunRiseSet")]]
NumericVector sunRiseSet(double latrad, double slorad, double asprad, double delta){
  double L1 = asin(cos(slorad)*sin(latrad)+sin(slorad)*cos(latrad)*cos(asprad)); //latitude on equivalent slope
  double den = cos(slorad)*cos(latrad)-sin(slorad)*sin(latrad)*cos(asprad);
  double L2;
  if(den<0) {
    L2 = atan((sin(slorad)*sin(asprad))/den)+PI;
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
  return(NumericVector::create(T2,T3));
}


/**
 *  Calculates day length (in hours)
 *  
 *  latrad - Latitude (in radians) 
 *  slorad - Inclination of slope, in radians above horizontal
 *  asprad - Azimuth of slope (aspect), in radians from north
 *  delta - Solar declination, in radians
 */
// [[Rcpp::export("radiation_daylength")]]
double daylength(double latrad, double slorad, double asprad, double delta) {
  NumericVector v = sunRiseSet(latrad, slorad, asprad, delta); //solar hour at sunrise and sunset, in radians
  double N = 12.0/PI*(v[1]-v[0]); 
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
// [[Rcpp::export("radiation_daylengthseconds")]]
double daylengthseconds(double latrad, double slorad, double asprad, double delta) {
  return(daylength(latrad,slorad, asprad, delta)*3600.0);
}


/**
 * Returns instant radiation (in kW/m2). From Granier & Ohmura (1968)
 * Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800
 *
 *  latrad - latitude of the slope, in radians
 *  A - Azimuth of slope, in radians from north
 *  Zx - Zenith angle of the vector normal to the slope (= slope?), in radians
 *  delta - Solar declination, in radians
 *  H - hour angle measured from solar noon, in radians
 */
double RpotInstant(double latrad, double slorad, double asprad, double delta, double H) {
  double t1 = (sin(latrad)*cos(H))*(-1.0*cos(asprad)*sin(slorad)) -1.0*sin(H)*(sin(asprad)*sin(slorad)) + (cos(latrad)*cos(H))*cos(slorad);
  double t2 = cos(latrad)*(cos(asprad)*sin(slorad)) + sin(latrad)*cos(slorad);
  return(GSC_kWm2*(t1*cos(delta)+ t2*sin(delta)));
}


/**
* One-day potential solar radiation (in MJ/m2). From Granier & Ohmura (1968)
* Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800
*
*  latrad - latitude of the slope, in radians
*  asprad - Azimuth of slope, in radians from north
*  slorad - Zenith angle of the vector normal to the slope (= slope) in radians
*  J - Julian day (1 to 365)
*  step - Number of seconds step (for integration of instant radiation)
*/
// [[Rcpp::export("radiation_potentialRadiation")]]
double RpotDay(double latrad,  double slorad, double asprad, double delta) {
  double step = 1200.0;
  double hradstep = step*(5.0/1200.0)*(PI/180.0);
  double Rpot = 0.0;
  NumericVector srs = sunRiseSet(latrad, slorad, asprad, delta);
  for(double hrad=srs[0];hrad<srs[1];hrad+=hradstep) { //72 steps (3 per hour)
    Rpot += step*std::max(0.0, RpotInstant(latrad, slorad, asprad, delta, hrad));
  }
  return(Rpot/1000.0);
}



/**
 * One-day solar radiation (in MJ/m2). From Thornton & Running (1999)
 *
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
// [[Rcpp::export("radiation_solarRadiation")]]
double RDay(double latrad, double elevation, double slorad, double asprad, double delta,
            double diffTemp, double diffTempMonth, double vpa, double precipitation) {
  //Number of seconds step (for integration of instant potential radiation and maximum transmittance)
  double step = 1200.0;
  double Rpot = 0.0;
  double B = 0.031+0.201*exp(-0.185*diffTempMonth);
  double Tfmax = 1.0-0.9*exp(-B*pow(diffTemp,1.5));
  if(precipitation>0.0) Tfmax *=0.75; //Correction for wet days
  double Ttmax = 0.0, RpotInst = 0.0, theta=0.0;
  double pratio = pow(1.0 -2.2569e-5*elevation,5.2553); // from Pearcy et al. 1991
  double hradstep = step*(5.0/1200.0)*(PI/180.0);
  NumericVector srs = sunRiseSet(latrad, slorad, asprad, delta);
  for(double hrad=srs[0];hrad<srs[1];hrad+=hradstep) { //72 steps (3 per hour)
    RpotInst = RpotInstant(latrad, slorad, asprad, delta, hrad);
    if(RpotInst>0.0) {
      //Solar zenith angle
      theta = sin(latrad)*sin(delta)+cos(latrad)*cos(delta)*cos(hrad);
      Ttmax +=step*RpotInst*pow(0.87,pratio*(1.0/cos(theta)));
      Rpot += step*RpotInst;
    }
  }
  Ttmax = (Ttmax/Rpot) -6.1e-2*vpa; //vpa in kPa
  //  Rcout<<Rpot<<" "<<Ttmax<<" "<<Tfmax<<"\n";
  if(Rpot==0.0) return(0.0);
  return(Rpot*Ttmax*Tfmax/1000.0); //Radiation in MJ/m2
}



/**
*   Estimates daily net outgoing longwave radiation from incoming solar radiation data
*   
*   REF:  McMahon et al. (2013) 
*   Estimating actual, potential, reference crop and pan evaporation using standard meteorological data: a pragmatic synthesis
*   Hydrology & Earth System Sciences
*   See also:  http://www.fao.org/docrep/x0490e/x0490e06.htm
*   
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
// [[Rcpp::export("radiation_outgoingLongwaveRadiation")]]
double outgoingLongwaveRadiation(double latrad, double elevation,  double slorad,  double asprad, double delta, 
                                 double vpa, double tmin, double tmax, double R_s){
  double R_a = RpotDay(latrad,  slorad, asprad, delta); //Extraterrestrial (potential) radiation
  double R_so = (0.75 + (2.0 * 0.00001) * elevation) * R_a; //Clear sky radiation
  // Rcout<<R_s<<" "<<R_so<<" "<<R_s/R_so<<"\n";
  //Net outgoing longwave radiation (MJ.m^-2.day^-1)
  double R_nl = SIGMA_MJ_day * (0.34 - 0.14 * sqrt(vpa)) * (pow(tmax + 273.2,4.0) + pow(tmin + 273.2,4.0))/2.0 * (1.35 * std::min(R_s/R_so,1.0) - 0.35);
  return(R_nl);
}



/**
*   Estimates daily net solar radiation from incoming solar radiation data
*   
*   REF:  McMahon et al. (2013) 
*   Estimating actual, potential, reference crop and pan evaporation using standard meteorological data: a pragmatic synthesis
*   Hydrology & Earth System Sciences
*   See also:  http://www.fao.org/docrep/x0490e/x0490e06.htm
*   
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
// [[Rcpp::export("radiation_netRadiation")]]
double netRadiation(double latrad,  double elevation, double slorad, double asprad, double delta, 
                    double vpa, double tmin, double tmax, double R_s, 
                    double alpha = 0.08) {
  
  //Net outgoing longwave radiation (MJ.m^-2.day^-1)
  double R_nl = outgoingLongwaveRadiation(latrad, elevation, slorad, asprad, delta, 
                                          vpa, tmin, tmax, R_s);
  //Net incoming shortwave radiation (MJ.m^-2.day^-1, after acounting for surface albedo)
  double R_ns = (1.0 - alpha) * R_s; 
  //Net radiation
  return(std::max(0.0,R_ns - R_nl)); 
}


/**
* Returns a vector with potential daily radiation (in MJ/m2).
*
*  latrad - latitude of the slope, in radians
*  slorad - Zenith angle of the vector normal to the slope (= slope?) in radians
*  asprad - Azimuth of slope, in radians from north
*  J - A vector with julian days
*/
// [[Rcpp::export(".potentialRadiationSeries")]]
NumericVector potentialRadiationSeries(double latrad, double slorad,  double asprad, NumericVector J) {
  NumericVector Rpot(J.size());
  for(int i=0;i<J.size(); i++) {
    Rpot[i] = RpotDay(latrad,slorad, asprad, solarDeclination(J[i]));
  }
  return(Rpot);
}
// [[Rcpp::export(".potentialRadiationPoints")]]
NumericVector potentialRadiationPoints(double latrad, NumericVector slorad, NumericVector asprad, int J) {
  NumericVector Rpot(slorad.size());
  double delta = solarDeclination(J);
  for(int i=0;i<slorad.size(); i++) {
    Rpot[i] = RpotDay(latrad,slorad[i], asprad[i], delta);
  }
  return(Rpot);
}

/**
* Returns a vector with daily radiation (in MJ/m2).
*
*  latrad - latitude of the slope, in degrees
*  elevation - Elevation in m
*  slorad - Zenith angle of the vector normal to the slope (= slope?) in radians
*  asprad - Azimuth of slope, in radians from north
*  J - A vector with julian days
*/
// [[Rcpp::export(".radiationSeries")]]
NumericVector radiationSeries(double latrad, double elevation, double slorad, double asprad, NumericVector J,
                              NumericVector diffTemp, NumericVector diffTempMonth, NumericVector VP, NumericVector P) {
  NumericVector Rpot(J.size());
  for(int i=0;i<J.size(); i++) {
    Rpot[i] = RDay(latrad,elevation, slorad,asprad, solarDeclination(J[i]), 
                   diffTemp[i], diffTempMonth[i], VP[i], P[i]);
  }
  return(Rpot);
}

// [[Rcpp::export(".radiationPoints")]]
NumericVector radiationPoints(NumericVector latrad, NumericVector elevation, NumericVector slorad, NumericVector asprad, int J,
                              NumericVector diffTemp, NumericVector diffTempMonth, NumericVector VP, NumericVector P) {
  int npoints = slorad.size();
  NumericVector Rpot(npoints);
  for(int i=0;i<npoints; i++) {
    Rpot[i] = RDay(latrad[i],elevation[i], slorad[i],asprad[i],solarDeclination(J), 
                   diffTemp[i], diffTempMonth[i],VP[i], P[i]);
  }
  return(Rpot);
}


