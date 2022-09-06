#include <Rcpp.h>
using namespace Rcpp;
#include "interpolationutils.h"



/**
* WIND FUNCTIONS
*/


double vectorDistance(double r1, double d1, double r2, double d2) {
  //subtract coordinates
  double x = r1*sin(d1/57.29578)-r2*sin(d2/57.29578);
  double y = r1*cos(d1/57.29578)-r2*cos(d2/57.29578);
  return(sqrt(pow(x,2.0)+pow(y,2.0)));
}

NumericVector vectorAverage(NumericVector r, NumericVector theta, NumericVector weight) {
  int nvec = r.size();
  double x = 0.0, y=0.0;
  double wsum = 0.0;
  for(int i=0;i<nvec;i++) {
    // Rcout<<r[i]<< " "<< theta[i]<<" "<<weight[i]<<"\n";
    x += weight[i]*r[i]*sin(theta[i]/57.29578);
    y += weight[i]*r[i]*cos(theta[i]/57.29578);
    wsum +=weight[i];
  }
  x = x/wsum;
  y = y/wsum;
  double rout = sqrt(pow(x,2.0)+pow(y,2.0));
  double thetaout = 57.29578*atan2 (x, y);
  if(thetaout<0) thetaout = 360.0 + thetaout;
  // Rcout<<"res: "<<rout<< ", "<<thetaout<<"\n";
  return(NumericVector::create(rout, thetaout));
}



int getWindFieldIndex(double pws, double pwd, NumericVector wfSpeed, NumericVector wfDirection) {
  int nf = wfSpeed.size();
  double dmin = 9999999.9;
  int imin = -1;
  double d;
  for(int i=0;i<nf;i++) {
    d = vectorDistance(pws,pwd,wfSpeed[i], wfDirection[i]);
    if(d<dmin) {
      dmin = d;
      imin = i;
    }
  }
  return(imin);
}

// [[Rcpp::export(".getWindFieldIndexAndFactor")]]
List getWindFieldIndexAndFactor(NumericMatrix windSpeed, NumericMatrix windDirection,
                                NumericMatrix wfSpeed, NumericMatrix wfDirection) {
  int nrow = windSpeed.nrow();
  int ncol = windSpeed.ncol();
  IntegerMatrix index(nrow, ncol);
  NumericMatrix factor(nrow, ncol);
  double pws, pwd;
  NumericVector wsvec, wdvec;
  for(int i=0;i<nrow; i++) {
    wsvec = wfSpeed(i,_);
    wdvec = wfDirection(i,_);
    for(int j=0;j<ncol; j++) {
      pws = windSpeed(i,j);
      pwd = windDirection(i,j);
      if((!NumericVector::is_na(pws)) && (!NumericVector::is_na(pwd))) {
        index(i,j) = getWindFieldIndex(pws,pwd,wsvec, wdvec);
        factor(i,j) = pws/wsvec[index(i,j)];
        // Rcout<<index(i,j)<<"\n";
      } else {
        index(i,j) = R_NaInt;
        factor(i,j) = R_NaReal;
      }
    }
  }
  return(List::create(_["Index"]=index, _["Factor"] = factor));
}

/*
 * Interpolates wind direction and wind speed using weighed polar average
 * with weights from Gaussian kernel.
 */
NumericVector interpolateWindPoint(double xp, double yp, NumericVector ws, NumericVector wd,
                                   NumericVector X, NumericVector Y,
                                   double iniRp = 140000, double alpha = 2.0, int N = 1, int iterations = 3,
                                   double directionsAvailable = TRUE){
  int nstations = X.size();
  NumericVector r(nstations);
  for(int i=0;i<nstations;i++) {
    r[i] = sqrt(pow(xp-X[i],2.0)+pow(yp-Y[i],2.0));
  }
  double Rp = estimateRp(r, iniRp, alpha, N, iterations);
  // Rcout<<Rp<<"\n";
  NumericVector W = gaussianFilter(r, Rp, alpha);
  if(directionsAvailable) return(vectorAverage(ws, wd, W));
  else {
    NumericVector wws = ws*W;
    double num  = std::accumulate(wws.begin(),wws.end(),0.0);
    double den = std::accumulate(W.begin(),W.end(),0.0);
    double fws = num/den;
    if(den==0.0) fws = NA_REAL;
    // Rcout<<" num: "<<num<<" den: "<<den<<"\n";
    return(NumericVector::create(fws, NA_REAL));
  }
}
/*
 * Interpolates wind direction and wind speed of meteorological stations
 * for a set of points (no use of wind fields).
 */
// [[Rcpp::export("interpolation_wind")]]
NumericMatrix interpolateWindStationPoints(NumericVector Xp, NumericVector Yp, NumericVector WS, NumericVector WD,
                                         NumericVector X, NumericVector Y,
                                         double iniRp = 140000, double alpha = 2.0, int N = 1, int iterations = 3,
                                         double directionsAvailable = true){
  int npoints = Xp.size();
  NumericMatrix Wp(npoints,2);
  NumericVector wvec;

  for(int i=0;i<npoints;i++) {
    wvec = interpolateWindPoint(Xp[i], Yp[i], WS, WD,
                                X,Y,
                                iniRp, alpha, N, iterations, directionsAvailable);
    Wp(i,0) = wvec[0];
    Wp(i,1) = wvec[1];
  }
  return(Wp);
}

/*
 * Interpolates wind direction and wind speed of meteorological stations
 * for a set of points, using wind fields to have estimates of the local wind
 * at the target point according to each station.
 */
NumericMatrix interpolateWindFieldPoints(NumericVector Xp, NumericVector Yp, NumericMatrix WS, NumericMatrix WD,
                                    NumericVector X, NumericVector Y, IntegerVector I, NumericVector F,
                                    double iniRp = 140000, double alpha = 2.0, int N = 1, int iterations = 3){
  int npoints = Xp.size();
  int nstations = X.size();
  NumericMatrix Wp(npoints,2);
  NumericVector wvec;
  NumericVector wsp(nstations), wdp(nstations);

  for(int i=0;i<npoints;i++) {
    for(int j=0;j<nstations;j++) {
      wsp[j] = WS(i,I(j)) * F(j);
      wdp[j] = WD(i,I(j));
    }
    wvec = interpolateWindPoint(Xp[i], Yp[i], wsp, wdp,
                                X,Y,
                                iniRp, alpha, N, iterations, true);
    Wp(i,0) = wvec[0];
    Wp(i,1) = wvec[1];
  }
  return(Wp);
}

// [[Rcpp::export(".interpolateWindFieldSeriesPoints")]]
List interpolateWindFieldSeriesPoints(NumericVector Xp, NumericVector Yp, NumericMatrix WS, NumericMatrix WD,
                                 NumericVector X, NumericVector Y, IntegerMatrix I, NumericMatrix F,
                                 double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3){
  int npoints = Xp.size();
  int nstations = X.size();
  int nDays = I.ncol();
  NumericMatrix WSp(npoints,nDays);
  NumericMatrix WDp(npoints,nDays);
  LogicalVector missing(nstations);
  for(int d = 0;d<nDays;d++) {
    //    Rcout<<"Day: "<<d<<"\n";
    int nmis = 0;
    for(int i=0;i<nstations;i++) {
      missing[i] = IntegerVector::is_na(I(i,d));
      if(missing[i]) nmis++;
    }
    IntegerVector Iday(nstations-nmis);
    NumericVector Fday(nstations-nmis);
    NumericVector Xday(nstations-nmis);
    NumericVector Yday(nstations-nmis);
    int c = 0;
    for(int i=0;i<nstations;i++) {
      if(!missing[i]) {
        Iday[c] = I(i,d);
        Fday[c] = F(i,d);
        Xday[c] = X[i];
        Yday[c] = Y[i];
        c++;
      }
    }
    NumericMatrix Wday = interpolateWindFieldPoints(Xp, Yp, WS, WD,
                                               Xday, Yday, Iday, Fday,
                                               iniRp, alpha, N, iterations);
    for(int p=0;p<npoints;p++) {
      WSp(p,d) = Wday(p,0);
      WDp(p,d) = Wday(p,1);
    }
  }
  return(List::create(_["WS"] = WSp, _["WD"] = WDp));
}

// [[Rcpp::export(".interpolateWindStationSeriesPoints")]]
List interpolateWindStationSeriesPoints(NumericVector Xp, NumericVector Yp, NumericMatrix WS, NumericMatrix WD,
                                      NumericVector X, NumericVector Y,
                                      double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3){
  int npoints = Xp.size();
  int nstations = X.size();
  int nDays = WS.ncol();
  NumericMatrix WSp(npoints,nDays);
  NumericMatrix WDp(npoints,nDays);
  LogicalVector missing(nstations);
  for(int d = 0;d<nDays;d++) {
       // Rcout<<"Day: "<<d<<"\n";
    int nmis = 0, nmisdir = 0;
    for(int i=0;i<nstations;i++) {
      missing[i] = NumericVector::is_na(WS(i,d));
      if(missing[i]) nmis++;
      if((!missing[i]) && (NumericVector::is_na(WD(i,d)))) nmisdir++;
    }
    if(nstations> nmis) {
      NumericVector Xday(nstations-nmis);
      NumericVector Yday(nstations-nmis);
      NumericVector WSday(nstations-nmis);
      NumericVector WDday(nstations-nmis);
      int c = 0;
      for(int i=0;i<nstations;i++) {
        if(!missing[i]) {
          Xday[c] = X[i];
          Yday[c] = Y[i];
          WSday[c] = WS(i,d);
          WDday[c] = WD(i,d);
          c++;
        }
      }
      bool directionsAvailable = (nmisdir==0);
      NumericMatrix Wday = interpolateWindStationPoints(Xp, Yp, WSday, WDday,
                                                        Xday, Yday,
                                                        iniRp, alpha, N, iterations, directionsAvailable);
      // Rcout<< directionsAvailable<< " "<< Wday<<"\n";
      for(int p=0;p<npoints;p++) {
        WSp(p,d) = Wday(p,0);
        WDp(p,d) = Wday(p,1);
      }
    } else {
      for(int p=0;p<npoints;p++) {
        WSp(p,d) = NA_REAL;
        WDp(p,d) = NA_REAL;
      }
    }
  }
  return(List::create(_["WS"] = WSp, _["WD"] = WDp));
}
