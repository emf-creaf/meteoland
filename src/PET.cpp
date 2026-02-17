// [[Rcpp::interfaces(r,cpp)]]
#include <Rcpp.h>
#include "meteoland/utils_c.hpp"
#include "meteoland/pet_c.hpp"


using namespace Rcpp;

/*
 Daily equilibrium evapotranspiration
The equation requires the daily radiation, and neglects
effects of diurnal temperature variations on s, g and L
We also neglect long-wave flux (but see Linacre 1968).
Long-wave radiation (Linacre 1968)
R_longwave = 32*10^(-5)*(1+4*(n/N))*(100-T) (cal/cm2.min)
Cal2Joules = 4.18400 cal/J
min2day = 24*60 min/day
*/
// [[Rcpp::export(".dailyEquilibriumPET")]]
NumericVector dailyEquilibriumPET(NumericVector Temp, NumericVector Rn){
  int n = Temp.size();
  NumericVector D(n);
  double s = 0.0;
  for(int i=0;i<n;i++){
    s = 2503000*exp(17.269*Temp[i]/(237.3+Temp[i]))/pow(237.3+Temp[i],2.0);
    D[i] = 10000*(s/(s+65))*Rn[i]/2500000; // L = 2.5*10^6
  }
  return(D);
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
                 double u, double z = 10.0, double z0 = 0.001,
                 double alpha = 0.25, String windfun = "1956") {
  return(PenmanPET_c(latrad, elevation,slorad, asprad, J,
                     Tmin, Tmax, RHmin,RHmax, R_s,
                     u, z, z0, alpha, windfun.get_cstring()));
}


//' @describeIn penman Penman Monteith method
//' @export
// [[Rcpp::export("penmanmonteith")]]
double PenmanMonteithPET(double rc, double elevation,
                         double Tmin, double Tmax,
                         double RHmin, double RHmax,
                         double Rn, double u = NA_REAL) {
  return(PenmanMonteithPET_c(rc, elevation,
                             Tmin, Tmax,
                             RHmin, RHmax,
                             Rn, u));
}

// [[Rcpp::export(".penmanpoint")]]
NumericVector PenmanPETPointSeries(double latrad, double elevation, double slorad, double asprad, IntegerVector J,
                 NumericVector Tmin, NumericVector Tmax, NumericVector RHmin, NumericVector RHmax, NumericVector R_s,
                 NumericVector u, double z = 10.0,
                 double z0 = 0.001, double alpha = 0.25, String windfun ="1956") {
  int ndays = J.size();
  NumericVector PET(ndays);
  for(int d=0; d<ndays;d++) {
    if(NumericVector::is_na(z)) PET[d] = PenmanPET(latrad, elevation, slorad, asprad, J[d],Tmin[d], Tmax[d], RHmin[d],RHmax[d], R_s[d], R_NaReal, R_NaReal, z0, alpha);
    else PET[d] = PenmanPET_c(latrad, elevation,slorad, asprad, J[d],
                             Tmin[d], Tmax[d], RHmin[d],RHmax[d], R_s[d],
                             u[d], z, z0, alpha, windfun.get_cstring());
  }
  return(PET);
}

// [[Rcpp::export(".penmanmonteithpoint")]]
NumericVector PenmanMonteithPETPointSeries(double rc, double elevation, 
                                           NumericVector Tmin, NumericVector Tmax,
                                           NumericVector RHmin, NumericVector RHmax,
                                           NumericVector Rn, NumericVector u) {
  int ndays = Tmin.size();
  NumericVector PET(ndays);
  for(int d=0; d<ndays;d++) {
    PET[d] = PenmanMonteithPET_c(rc, elevation,Tmin[d],Tmax[d], RHmin[d],RHmax[d], Rn[d], u[d]);
  }
  return(PET);
}


// [[Rcpp::export(".PenmanPETPointsDay")]]
NumericVector PenmanPETPointsDay(NumericVector latrad, NumericVector elevation, NumericVector slorad, NumericVector asprad,  int J,
                                 NumericVector Tmin, NumericVector Tmax, NumericVector RHmin, NumericVector RHmax, NumericVector R_s,
                                 NumericVector u, double z = 10.0,
                                 double z0 = 0.001, double alpha = 0.25,
                                 String windfun ="1956") {
  int points = latrad.size();
  NumericVector PET(points);
  for(int d=0; d<points;d++) {
    PET[d] = PenmanPET_c(latrad[d], elevation[d], slorad[d], asprad[d], J,
                         Tmin[d], Tmax[d], RHmin[d],RHmax[d], R_s[d],
                         u[d], z, z0, alpha, windfun.get_cstring());
  }
  return(PET);
}


