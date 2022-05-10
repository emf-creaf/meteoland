#include <Rcpp.h>
#include <numeric>
#include "interpolationutils.h"

using namespace Rcpp;
double interpolateTemperaturePoint(double xp, double yp, double zp, 
                                   NumericVector X, NumericVector Y, NumericVector Z, NumericVector T, 
                                   NumericVector zDif, NumericVector tDif,
                                   double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3,
                                   bool debug = false){
  int nstations = X.size();
  int nDif = tDif.size();
  NumericVector r(nstations);
  for(int i=0;i<nstations;i++) {
    r[i] = sqrt(pow(xp-X[i],2.0)+pow(yp-Y[i],2.0));
  }
  double Rp = estimateRp(r, iniRp, alpha, N, iterations);
  NumericVector W = gaussianFilter(r, Rp, alpha);
  //Weights for weighted regression
  NumericVector WDif(nDif,0.0);
  int c = 0;
  for(int i=0;i<nstations;i++) {
    for(int j=0;j<i;j++) {
      WDif[c] = W[i]*W[j];
      // if(debug) Rcout<<" "<<tDif[c]<<" "<<WDif[c]<<"\n";
      c++;
    }
  }
  //Weighted regression
  NumericVector wr = weightedRegression(tDif, zDif, WDif);
  double Wnum = 0.0;
  //Apply weighting
  for(int i=0;i<nstations;i++) {
    // Rcout<<T[i]<<"/";
    Wnum +=W[i]*(T[i]+wr[0]+wr[1]*(zp-Z[i]));
  }
  if(debug) Rcout<< " nstations: "<< nstations<<" wr0: "<<wr[0]<<" wr1: "<< wr[1]<<" Wnum: "<<Wnum<<" sumW: "<<std::accumulate(W.begin(), W.end(), 0.0)<<"\n";
  return(Wnum/std::accumulate(W.begin(), W.end(), 0.0));
}


// [[Rcpp::export("interpolation_temperature")]]
NumericVector interpolateTemperaturePoints(NumericVector Xp, NumericVector Yp, NumericVector Zp, NumericVector X, NumericVector Y, NumericVector Z, NumericVector T,  
                                           double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3, bool debug = false){
  int npoints = Xp.size();
  int nstations = X.size();
  NumericVector Tp(npoints);
  NumericVector zDif(nstations*(nstations-1)/2);
  NumericVector tDif(nstations*(nstations-1)/2);
  int c = 0;
  for(int i=0;i<nstations;i++) {
    for(int j=0;j<i;j++) {
      zDif[c] = Z[i]-Z[j];
      tDif[c] = T[i]-T[j];
      c++;
    }
  }
  for(int i=0;i<npoints;i++) {
    Tp[i] = interpolateTemperaturePoint(Xp[i], Yp[i], Zp[i], X,Y,Z,T, zDif, tDif, 
                                        iniRp, alpha, N, iterations, debug);
  }
  return(Tp);
}

// [[Rcpp::export(".interpolateTemperatureSeriesPoints")]]
NumericMatrix interpolateTemperatureSeriesPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp, 
                                                 NumericVector X, NumericVector Y, NumericVector Z, NumericMatrix T,  
                                                 double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3, bool debug = false){
  int npoints = Xp.size();
  int nstations = X.size();
  int nDays = T.ncol();
  NumericMatrix Tp(npoints,nDays);
  LogicalVector missing(nstations);
  for(int d = 0;d<nDays;d++) {
    // Rcout<<"Day: "<<d<<"\n";
    int nmis = 0;
    for(int i=0;i<nstations;i++) {
      missing[i] = (NumericVector::is_na(T(i,d)) || NumericVector::is_na(X[i]) || NumericVector::is_na(Y[i]) || NumericVector::is_na(Z[i]));
      if(missing[i]) nmis++;
    }
    if(debug) Rcout << "Day "<< d << " nexcluded = " << nmis;
    NumericVector Tday(nstations-nmis);
    NumericVector Xday(nstations-nmis);
    NumericVector Yday(nstations-nmis);
    NumericVector Zday(nstations-nmis);
    int c = 0;
    for(int i=0;i<nstations;i++) {
      if(!missing[i]) {
        Tday[c] = T(i,d);
        Xday[c] = X[i];
        Yday[c] = Y[i];
        Zday[c] = Z[i];
        c++;
      }
    }
    // Rcout<<"non-missing: "<<c; 
    NumericVector Tpday = interpolateTemperaturePoints(Xp, Yp,Zp, 
                                                       Xday, Yday, Zday, Tday,
                                                       iniRp, alpha, N, iterations, debug);
    for(int p=0;p<npoints;p++) {
      Tp(p,d) = Tpday[p];
      // Rcout<<" value: "<<Tpday[p]<<"\n"; 
    }
  }
  return(Tp);
}