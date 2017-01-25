#include <Rcpp.h>
#include "utils.h"


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



// [[Rcpp::export(".penmanpoint")]]
NumericVector PenmanPETPointSeries(double latrad, double elevation, double slorad, double asprad, IntegerVector J,
                 NumericVector Tmin, NumericVector Tmax, NumericVector RHmin, NumericVector RHmax, NumericVector R_s,
                 NumericVector u, double z = 2.0,
                 double z0 = 0.001, double alpha = 0.08, String windfun ="1956") {
  int ndays = J.size();
  NumericVector PET(ndays);
  for(int d=0; d<ndays;d++) {
    if(NumericVector::is_na(z)) PET[d] = PenmanPET(latrad, elevation, slorad, asprad, J[d],Tmin[d], Tmax[d], RHmin[d],RHmax[d], R_s[d], R_NaReal, R_NaReal, z0, alpha);
    else PET[d] = PenmanPET(latrad, elevation,slorad, asprad, J[d],
                            Tmin[d], Tmax[d], RHmin[d],RHmax[d], R_s[d],
                            u[d], z, z0, alpha, windfun);
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
    PET[d] = PenmanMonteithPET(rc, elevation,Tmin[d],Tmax[d], RHmin[d],RHmax[d], Rn[d], u[d]);
  }
  return(PET);
}


// [[Rcpp::export(".PenmanPETPointsDay")]]
NumericVector PenmanPETPointsDay(NumericVector latrad, NumericVector elevation, NumericVector slorad, NumericVector asprad,  int J,
                                 NumericVector Tmin, NumericVector Tmax, NumericVector RHmin, NumericVector RHmax, NumericVector R_s,
                                 NumericVector u, double z = 2.0,
                                 double z0 = 0.001, double alpha = 0.08,
                                 String windfun ="1956") {
  int points = latrad.size();
  NumericVector PET(points);
  for(int d=0; d<points;d++) {
    PET[d] = PenmanPET(latrad[d], elevation[d], slorad[d], asprad[d], J,
                       Tmin[d], Tmax[d], RHmin[d],RHmax[d], R_s[d],
                       u[d], z, z0, alpha, windfun);
  }
  return(PET);
}


