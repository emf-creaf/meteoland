// [[Rcpp::interfaces(r,cpp)]]
#include <Rcpp.h>
#include "meteoland/radiation_c.hpp"
using namespace Rcpp;



//' Solar radiation utility functions
//'
//' Set of functions used in the calculation of incoming solar radiation and net
//' radiation.
//'
//'
//' @aliases radiation_dateStringToJulianDays radiation_daylength
//' radiation_daylengthseconds radiation_directDiffuseInstant
//' radiation_directDiffuseDay radiation_potentialRadiation radiation_julianDay
//' radiation_skyLongwaveRadiation radiation_outgoingLongwaveRadiation
//' radiation_netRadiation radiation_solarRadiation radiation_solarConstant
//' radiation_solarElevation radiation_solarDeclination radiation_sunRiseSet
//' @param dateStrings A character vector with dates in format "YYYY-MM-DD".
//' @param latrad Latitude (in radians North).
//' @param slorad Slope (in radians).
//' @param asprad Aspect (in radians from North).
//' @param delta Solar declination (in radians).
//' @param solarConstant Solar constant (in kW·m-2).
//' @param hrad Solar hour (in radians).
//' @param R_s Daily incident solar radiation (MJ·m-2).
//' @param clearday Boolean flag to indicate a clearsky day (vs. overcast).
//' @param nsteps Number of daily substeps.
//' @param J Julian day (integer), number of days since January 1, 4713 BCE at
//' noon UTC.
//' @param year,month,day Year, month and day as integers.
//' @param alpha Surface albedo (from 0 to 1).
//' @param Tair Air temperature (in degrees Celsius).
//' @param vpa Average daily vapor pressure (kPa).
//' @param c Proportion of sky covered by clouds (0-1).
//' @param tmin,tmax Minimum and maximum daily temperature (ºC).
//' @param elevation Elevation above sea level (in m).
//' @param precipitation Precipitation (in mm).
//' @param diffTemp Difference between maximum and minimum temperature (ºC).
//' @param diffTempMonth Difference between maximum and minimum temperature,
//' averaged over 30 days (ºC).
//' @return Values returned for each function are: \itemize{
//' \item\code{radiation_dateStringToJulianDays}: A vector of Julian days (i.e.
//' number of days since January 1, 4713 BCE at noon UTC).
//' \item\code{radiation_daylength}: Day length (in hours).
//' \item\code{radiation_daylengthseconds}: Day length (in seconds).
//' \item\code{radiation_directDiffuseInstant}: A vector with instantaneous
//' direct and diffusive radiation rates (for both SWR and PAR).
//' \item\code{radiation_directDiffuseDay}: A data frame with instantaneous
//' direct and diffusive radiation rates (for both SWR and PAR) for each
//' subdaily time step. \item\code{radiation_potentialRadiation}: Daily
//' (potential) solar radiation (in MJ·m-2). \item\code{radiation_julianDay}:
//' Number of days since January 1, 4713 BCE at noon UTC.
//' \item\code{radiation_skyLongwaveRadiation}: Instantaneous incoming (sky)
//' longwave radiation (W·m-2).
//' \item\code{radiation_outgoingLongwaveRadiation}: Daily outgoing longwave
//' radiation (MJ·m-2·day-1).  \item\code{radiation_netRadiation}: Daily net
//' solar radiation (MJ·m-2·day-1).  \item\code{radiation_solarConstant}: Solar
//' constant (in kW·m-2). \item\code{radiation_solarDeclination}: Solar
//' declination (in radians). \item\code{radiation_solarElevation}: Angle of
//' elevation of the sun with respect to the horizon (in radians).
//' \item\code{radiation_solarRadiation}: Daily incident solar radiation
//' (MJ·m-2·day-1). \item\code{radiation_sunRiseSet}: Sunrise and sunset hours
//' in hour angle (radians). }
//' @note Code for \code{radiation_julianDay()},
//' \code{radiation_solarConstant()} and \code{radiation_solarDeclination()} was
//' translated to C++ from R code in package 'insol' (by J. G. Corripio).
//' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
//' @seealso \code{\link{interpolate_data}}
//' @references Danby, J. M. Eqn. 6.16.4 in Fundamentals of Celestial Mechanics,
//' 2nd ed. Richmond, VA: Willmann-Bell, p. 207, 1988.
//'
//' Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct
//' shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800
//'
//' McMahon, T. A., M. C. Peel, L. Lowe, R. Srikanthan, and T. R. McVicar. 2013.
//' Estimating actual, potential, reference crop and pan evaporation using
//' standard meteorological data: a pragmatic synthesis. Hydrology & Earth
//' System Sciences 17:1331–1363. See also:
//' http://www.fao.org/docrep/x0490e/x0490e06.htm.
//'
//' Reda, I. and Andreas, A. 2003. Solar Position Algorithm for Solar Radiation
//' Applications. 55 pp.; NREL Report No. TP-560-34302, Revised January 2008.
//' http://www.nrel.gov/docs/fy08osti/34302.pdf
//'
//' Spitters, C.J.T., Toussaint, H.A.J.M. and Goudriaan, J. (1986). Separating
//' the diffuse and direct components of global radiation and its implications
//' for modeling canopy photosynthesis. I. Components of incoming radiation.
//' Agricultural and Forest Meteorology, 38, 231–242.
//' @export
// [[Rcpp::export("radiation_julianDay")]]
int julianDay(int year, int month, int day) {
  return(julianDay_c(year, month, day));
}

//' @describeIn radiation_julianDay Date string to julian days
//' @export
// [[Rcpp::export("radiation_dateStringToJulianDays")]]
IntegerVector dateStringToJulianDays(CharacterVector dateStrings) {
  int numDays = dateStrings.size();
  IntegerVector jd(numDays);
  for(int i=0;i<numDays;i++) {
    std::string c = as<std::string>(dateStrings[i]);
    jd[i] = julianDay_c(std::atoi(c.substr(0, 4).c_str()),std::atoi(c.substr(5,2).c_str()),std::atoi(c.substr(8,2).c_str()));
  }
  return(jd);
}


//' @describeIn radiation_julianDay solar declination
//' @export
// [[Rcpp::export("radiation_solarDeclination")]]
double solarDeclination(int J) {
  return(solarDeclination_c(J));
}

//' @describeIn radiation_julianDay solar constant
//' @export
// [[Rcpp::export("radiation_solarConstant")]]
double solarConstant(int J) {
  return(solarConstant_c(J));
}

/**
 * Returns the sunrise and sunset hours in hour angle (radians)
 *
 * latrad - Latitude of actual slope, in radians
 * slorad - Inclination of slope, in radians above horizontal
 * asprad - Azimuth of slope (aspect), in radians from north
 * delta - Solar declination, in radians
 */
//' @describeIn radiation_julianDay sun rise and set
//' @export
// [[Rcpp::export("radiation_sunRiseSet")]]
NumericVector sunRiseSet(double latrad, double slorad, double asprad, double delta){
  SunriseSet ss;
  sunRiseSet_c(ss, latrad, slorad, asprad, delta);
  return(NumericVector::create(ss.sunrise,ss.set));
}


//' @describeIn radiation_julianDay solar elevation
//' @export
// [[Rcpp::export("radiation_solarElevation")]]
double solarElevation(double latrad, double delta, double hrad) {
  return(solarElevation_c(latrad, delta, hrad));
}


//' @describeIn radiation_julianDay Day length
//' @export
// [[Rcpp::export("radiation_daylength")]]
double daylength(double latrad, double slorad, double asprad, double delta) {
  return(daylength_c(latrad, slorad, asprad, delta));
}

//' @describeIn radiation_julianDay Day length seconds
//' @export
// [[Rcpp::export("radiation_daylengthseconds")]]
double daylengthseconds(double latrad, double slorad, double asprad, double delta) {
  return(daylengthseconds_c(latrad,slorad, asprad, delta));
}

//' @describeIn radiation_julianDay Potential radiation
//' @export
// [[Rcpp::export("radiation_potentialRadiation")]]
double RpotDay(double solarConstant, double latrad,  double slorad, double asprad, double delta) {
  return(RpotDay_c(solarConstant, latrad, slorad, asprad, delta));
}

//' @describeIn radiation_julianDay solar Radiation
//' @export
// [[Rcpp::export("radiation_solarRadiation")]]
double RDay(double solarConstant, double latrad, double elevation, double slorad, double asprad, double delta,
            double diffTemp, double diffTempMonth, double vpa, double precipitation) {
  return(RDay_c(solarConstant, latrad, elevation, slorad, asprad, delta,
                diffTemp, diffTempMonth, vpa, precipitation));
} 


/**
 * Calculates instantaneous direct and diffuse radiation (in kW) from daily global radiation
 *
 *  solarConstant - Solar constant (in kW/m2)
 *  latrad - Latitude (in radians)
 *  slorad - Zenith angle of the vector normal to the slope (= slope?) in radians
 *  asprad - Azimuth of slope, in radians from north
 *  delta - Solar declination (in radians)
 *  hrad - Solar hour (in radians)
 *  R_p - Potential daily solar radiation (MJ·m-2)
 *  R_s - Global daily solar radiation (MJ·m-2)
 *  clearday - Boolean to indicate that is a clearsky (TRUE) vs overcast (FALSE)
 *
 * Spitters, C.J.T., Toussaint, H.A.J.M. & Goudriaan, J. (1986). Separating the diffuse and direct components of global radiation and its implications for modeling canopy photosynthesis. I. Components of incoming radiation.
 * Agricultural and Forest Meteoroloogy, 38, 231–242.
 */
NumericVector directDiffuseInstant(double solarConstant, double latrad, double slorad, double asprad, double delta,
                                   double hrad, double R_s,
                                   double R_p_flat, double Rpotinst_flat, double R_p_topo, double Rpotinst_topo,
                                   bool clearday) {
  // Rcout<< slorad<<" "<<R_p_topo<<"\n";
  //Solar elevation (for corrections)
  double beta = solarElevation(latrad, delta, hrad);

  //Estimation of SgSo ratio (transmittance) assuming flat surface.
  double SgSoday = R_s/R_p_flat;
  double SdfSgday = NA_REAL;
  if(SgSoday<0.07){
    SdfSgday = 1.0;
  } else if(SgSoday<0.35) {
    SdfSgday = 1.0 - 2.3*pow(SgSoday-0.07,2.0);
  } else if(SgSoday<0.75) {
    SdfSgday = 1.33 -1.46*SgSoday;
  } else {
    SdfSgday = 0.23;
  }
  double SdfSgday2 = SdfSgday;
  //If clear day (e.g. not rainy) modify for circumsolar part of diffuse radiation
  if(clearday) {
    SdfSgday2 = SdfSgday/(1.0+(1.0-pow(SdfSgday,2.0))*pow(cos(M_PI/4.0-beta),2.0)*pow(cos(beta),3.0));
  }

  double PARday = R_s*0.5; //Daily PAR radiation (MJ)
  double SdfSdPAR = (1.0+0.3*(1.0-pow(SdfSgday,2.0)))*SdfSgday2;
  double Sdfday = SdfSgday2*R_s; //MJ Diffuse daily radiation
  double Sdrday = R_s - Sdfday; //MJ Direct daily radiation
  double Sdrinst = (Sdrday*1000.0)*(Rpotinst_topo/(R_p_topo*1000.0));//kW Direct light is affected by topography
  double Sdfinst = (Sdfday*1000.0)*(Rpotinst_flat/(R_p_flat*1000.0));//kW Diffuse light not affected by topography
  if(R_p_topo==0.0) {
    Sdrinst = 0.0;
  }
  if(R_p_flat==0.0) {
    Sdfinst = 0.0;
  }
  double Sginst = Sdfinst + Sdrinst;
  double SdfdayPAR = SdfSdPAR*PARday;
  double SginstPAR = Sginst*0.5;
  double SdfinstPAR = std::min((SdfdayPAR*1000.0)*(Rpotinst_flat/(R_p_flat*1000.0)), SginstPAR);//kW
  if(R_p_flat==0.0) {
    SdfinstPAR = 0.0;
  }
  double SdrinstPAR = SginstPAR-SdfinstPAR;

  NumericVector res = NumericVector::create(Named("SolarElevation") = beta,
                                            Named("Rpot") = Rpotinst_topo,
                                            Named("Rpot_flat") = Rpotinst_flat,
                                            Named("Rg") = Sginst,
                                            Named("SWR_direct") = Sdrinst,
                                            Named("SWR_diffuse") = Sdfinst,
                                            Named("PAR_direct") = SdrinstPAR,
                                            Named("PAR_diffuse") = SdfinstPAR);
  return(res);
}
//' @describeIn radiation_julianDay Direct diffuse instant
//' @export
// [[Rcpp::export("radiation_directDiffuseInstant")]]
NumericVector directDiffuseInstant(double solarConstant, double latrad, double slorad, double asprad, double delta,
                                   double hrad, double R_s, bool clearday) {
  //Instantaneous potential radiation NOT accounting for topography
  double R_p_flat = RpotDay(solarConstant, latrad, 0.0, 0.0, delta);
  double Rpotinst_flat = std::max(0.0,RpotInstant_c(solarConstant, latrad, 0.0, 0.0, delta, hrad));//kW
  //Instantaneous potential radiation accounting for topography
  double R_p_topo = R_p_flat;
  double Rpotinst_topo = Rpotinst_flat;
  if(slorad>0.0) {
    NumericVector srs = sunRiseSet(latrad, slorad, asprad, delta);
    R_p_topo = RpotDay(solarConstant, latrad, slorad, asprad, delta);
    if(hrad >= srs[0] && hrad< srs[1]) Rpotinst_topo = std::max(0.0,RpotInstant_c(solarConstant, latrad, slorad, asprad, delta, hrad));//kW
    else Rpotinst_topo = 0.0;
  }
  return(directDiffuseInstant(solarConstant, latrad, slorad, asprad, delta, hrad, R_s, R_p_flat, Rpotinst_flat,
                              R_p_topo, Rpotinst_topo, clearday));
}



/**
* Calculates daily variation of direct and diffuse radiation (in kW) from daily global radiation
*
*  solarConstant - Solar constant (in kW/m2)
*  latrad - Latitude (in radians)
*  slorad - Zenith angle of the vector normal to the slope (= slope?) in radians
*  asprad - Azimuth of slope, in radians from north
*  delta - Solar declination (in radians)
*  R_s - Global daily solar radiation (MJ·m-2)
*  clearday - Boolean to indicate that is a clearsky (TRUE) vs overcast (FALSE)
*  nsteps - Number of steps to divide the day
*
* Spitters, C.J.T., Toussaint, H.A.J.M. & Goudriaan, J. (1986). Separating the diffuse and direct components of global radiation and its implications for modeling canopy photosynthesis. I. Components of incoming radiation.
* Agricultural and Forest Meteoroloogy, 38, 231–242.
*/
//' @describeIn radiation_julianDay Direct diffuse day
//' @export
// [[Rcpp::export("radiation_directDiffuseDay")]]
DataFrame directDiffuseDay(double solarConstant, double latrad, double slorad, double asprad, double delta,
                           double R_s, bool clearday, int nsteps = 24) {
  NumericVector Rpot(nsteps),Rpot_flat(nsteps), Rg(nsteps), SWR_direct(nsteps), SWR_diffuse(nsteps), PAR_direct(nsteps), PAR_diffuse(nsteps);
  NumericVector Hrad(nsteps), beta(nsteps);
  double R_p_flat = RpotDay(solarConstant, latrad, 0.0, 0.0, delta);
  double R_p_topo = R_p_flat;
  if(slorad>0.0) {
    R_p_topo = RpotDay(solarConstant, latrad, slorad, asprad, delta);
  }
  NumericVector srs = sunRiseSet(latrad, slorad, asprad, delta);
  for(int i=0;i<nsteps;i++) {
    Hrad[i] = -M_PI + (((double)i)+0.5)*(2.0*M_PI/((double)nsteps));
    double Rpotinst_flat = std::max(0.0,RpotInstant_c(solarConstant, latrad, 0.0, 0.0, delta, Hrad[i]));//kW
    double Rpotinst_topo = Rpotinst_flat;
    if(slorad>0.0) {
      Rpotinst_topo = 0.0;
      if(Hrad[i] >= srs[0] && Hrad[i]< srs[1]) Rpotinst_topo = std::max(0.0,RpotInstant_c(solarConstant, latrad, slorad, asprad, delta, Hrad[i]));//kW
    }
    NumericVector ddi = directDiffuseInstant(solarConstant, latrad, slorad, asprad, delta,
                                             Hrad[i], R_s, R_p_flat, Rpotinst_flat, R_p_topo, Rpotinst_topo, clearday);
    beta[i] = ddi["SolarElevation"];
    Rpot[i] = Rpotinst_topo;
    Rpot_flat[i] = Rpotinst_flat;
    Rg[i] = ddi["Rg"];
    SWR_direct[i] = ddi["SWR_direct"];
    SWR_diffuse[i] = ddi["SWR_diffuse"];
    PAR_direct[i] = ddi["PAR_direct"];
    PAR_diffuse[i] = ddi["PAR_diffuse"];
  }

  DataFrame res = DataFrame::create(Named("SolarHour") = Hrad,
                                    Named("SolarElevation") = beta,
                                    Named("Rpot") = Rpot,
                                    Named("Rpot_flat") = Rpot_flat,
                                    Named("Rg") = Rg,
                                    Named("SWR_direct") = SWR_direct,
                                    Named("SWR_diffuse") = SWR_diffuse,
                                    Named("PAR_direct") = PAR_direct,
                                    Named("PAR_diffuse") = PAR_diffuse);
  return(res);
}


//' @describeIn radiation_julianDay Sky longwave radiation
//' @export
// [[Rcpp::export("radiation_skyLongwaveRadiation")]]
double skyLongwaveRadiation(double Tair, double vpa, double c = 0) {
  return(skyLongwaveRadiation_c(Tair, vpa, c)); // eq. 10.9 Energy emitted by gray bodies (no dependence of emissivity on wavelength)
}



//' @describeIn radiation_julianDay Outgoing longwave radiation
//' @export
// [[Rcpp::export("radiation_outgoingLongwaveRadiation")]]
double outgoingLongwaveRadiation(double solarConstant, double latrad, double elevation,  double slorad,  double asprad, double delta,
                                 double vpa, double tmin, double tmax, double R_s) {
    return(outgoingLongwaveRadiation_c(solarConstant, latrad, elevation, slorad, asprad, delta,
                                       vpa, tmin, tmax, R_s));                                      
}


//' @describeIn radiation_julianDay Net radiation
//' @export
// [[Rcpp::export("radiation_netRadiation")]]
double netRadiation(double solarConstant, double latrad,  double elevation, double slorad, double asprad, double delta,
                    double vpa, double tmin, double tmax, double R_s,
                    double alpha = 0.08) {
  return(netRadiation_c(solarConstant, latrad, elevation, slorad, asprad, delta,
                        vpa, tmin, tmax, R_s,
                        alpha));
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
    Rpot[i] = RpotDay(solarConstant(J[i]),latrad,slorad, asprad, solarDeclination(J[i]));
  }
  return(Rpot);
}
// [[Rcpp::export(".potentialRadiationPoints")]]
NumericVector potentialRadiationPoints(double latrad, NumericVector slorad, NumericVector asprad, int J) {
  NumericVector Rpot(slorad.size());
  double delta = solarDeclination(J);
  double Gsc = solarConstant(J);
  for(int i=0;i<slorad.size(); i++) {
    Rpot[i] = RpotDay(Gsc,latrad,slorad[i], asprad[i], delta);
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
  NumericVector Rs(J.size());
  for(int i=0;i<J.size(); i++) {
    Rs[i] = RDay(solarConstant(J[i]), latrad,elevation, slorad,asprad, solarDeclination(J[i]),
                   diffTemp[i], diffTempMonth[i], VP[i], P[i]);
    // Rcout<<solarDeclination(J[i])<<" "<< slorad<< " "<< asprad <<" "<< Rs[i] <<"\n";
  }
  return(Rs);
}

// [[Rcpp::export(".radiationPoints")]]
NumericVector radiationPoints(NumericVector latrad, NumericVector elevation, NumericVector slorad, NumericVector asprad, int J,
                              NumericVector diffTemp, NumericVector diffTempMonth, NumericVector VP, NumericVector P) {
  int npoints = slorad.size();
  NumericVector Rpot(npoints);
  double delta = solarDeclination(J);
  double Gsc = solarConstant(J);
  for(int i=0;i<npoints; i++) {
    Rpot[i] = RDay(Gsc, latrad[i],elevation[i], slorad[i],asprad[i],delta,
                   diffTemp[i], diffTempMonth[i],VP[i], P[i]);
  }
  return(Rpot);
}


