#include <Rcpp.h>
#include "utils.h"
#include "interpolationutils.h"

using namespace Rcpp;

// [[Rcpp::export(".meteo")]]
DataFrame meteo(DataFrame MeteoMonth, NumericVector landscapeRainfall = NumericVector::create(), double ERconv=0.05, double ERsyn = 0.2, double shape = 2.0, double scale = 4.0, double albedo = 0.17, int firstMonth = 1, bool cyclic = false) {
  RNGScope scope;

  NumericVector RainM = MeteoMonth["Rainfall"];
  NumericVector TempM = MeteoMonth["Temp"];
  NumericVector RadM = MeteoMonth["Rad"];
  NumericVector daysMonthAll = MeteoMonth["NumDays"];


  int nMonths = RainM.size();
  NumericVector ERMonthAll(nMonths);
  //Determines the number of days
  int numDays = 0;
  int month = firstMonth;
  for(int i=0;i<nMonths;i++) {
    numDays +=daysMonthAll[i];
    if(month<=5 || month==12) ERMonthAll[i] = ERsyn;
    else ERMonthAll[i] = ERconv;
    if(month==12) month=1;
    else month +=1;
  }

  NumericVector Rainfall=rep(0.0,numDays);
  NumericVector Temp=rep(0.0,numDays);
  NumericVector Rad=rep(0.0,numDays);
  NumericVector ER=rep(0.0,numDays);
  NumericVector g = rgamma(numDays,shape, scale);
  NumericVector pd = runif(numDays);
  int day = 0, cg = 0;
  int cumDays = 0;
  int prevM = 0;
  int nextM = 0;
  double NMD = 0, pMonth, rd, dd, nN, Rs;

  for(int m=0; m<nMonths; m++) {
    NMD = daysMonthAll[m];
    if(landscapeRainfall.size()>0) {
      for(int d=cumDays;d<cumDays+NMD;d++){
        Rainfall[d] = landscapeRainfall[d]*RainM[m];
      }
    } else {
      pMonth = RainM[m];
      while(pMonth>0){
        rd = g[cg];
        if(cg==numDays) cg=0;
        if(rd>pMonth) rd = pMonth;
        Rainfall[cumDays+((int)(pd[cg]*NMD))] += rd;
        pMonth = pMonth - rd;
        cg++;
      }
    }
    prevM = m-1;
    nextM = m+1;
    if(cyclic && prevM<0) prevM = 11;
    else if(prevM<0) prevM = 0;
    if(cyclic && m == nMonths-1) nextM = 0;
    else if(!cyclic && m == nMonths-1) nextM = m;
    for(int d=0;d<NMD;d++){
      dd = (double) d;
      if(dd<=(NMD/2.0)) {
        Temp[day] = TempM[m]*(((NMD/2.0)+dd)/NMD)+TempM[prevM]*(((NMD/2.0)-dd)/NMD);
        Rs = RadM[m]*(((NMD/2.0)+dd)/NMD)+RadM[prevM]*(((NMD/2.0)-dd)/NMD);
      } else {
        Temp[day] = TempM[m]*(((NMD*3.0/2.0)-dd)/NMD)+TempM[nextM]*((dd-(NMD/2.0))/NMD);
        Rs = RadM[m]*(((NMD*3.0/2.0)-dd)/NMD)+RadM[nextM]*((dd-(NMD/2.0))/NMD);
      }
      if(Rainfall[day]>0) nN = 0.25;
      else nN = 0.75;
      Rad[day] = (1.0-albedo)*Rs-0.1927987*(1.0+4.0*nN)*(100.0-Temp[day]);
      if(Rad[day]< 0.0) Rad[day] = 0.0;
      ER[day] = ERMonthAll[m];
      day++;
    }
    cumDays += (int) NMD;
  }
  Rcpp::DataFrame df = DataFrame::create(_["Rainfall"] = Rainfall,_["Temp"] = Temp,
                                         _["Rn"] = Rad,_["ER"] = ER);
  return(df);
}



// [[Rcpp::export(".vapourPressureFromRH")]]
NumericMatrix vapourPressureFromRH(NumericMatrix T, NumericMatrix RH) {
  int nr = T.nrow();
  int nc = T.ncol();
  NumericMatrix VP(nr, nc);
  for(int i=0;i<nr;i++){
    for(int j=0;j<nc;j++) {
      if(NumericVector::is_na(T(i,j)) || NumericVector::is_na(RH(i,j)) ) {
        VP(i,j) = NA_REAL;
      } else {
        VP(i,j) = vapourPressureFromRH(T(i,j), RH(i,j));
      }
    }
  }
  return(VP);
}

// [[Rcpp::export(".dewpointTemperatureFromRH")]]
NumericMatrix dewpointTemperatureFromRH(NumericMatrix T, NumericMatrix RH) {
  int nr = T.nrow();
  int nc = T.ncol();
  NumericMatrix DT(nr, nc);
  for(int i=0;i<nr;i++){
    for(int j=0;j<nc;j++) {
      if(NumericVector::is_na(T(i,j)) || NumericVector::is_na(RH(i,j)) ) {
        DT(i,j) = NA_REAL;
      } else {
        DT(i,j) = dewpointTemperatureFromRH(T(i,j), RH(i,j));
      }
    }
  }
  return(DT);
}

/**
 * Assumes that minimum daily temperature is a reasonable surrogate of dew-point temperature!
 */

// [[Rcpp::export(".temp2SVP")]]
NumericVector temp2SVP(NumericVector TD) {
  NumericVector vp(TD.size());
  for(int i=0;i<TD.size();i++) vp[i] = temp2SVP(TD[i]);
  return(vp);
}


// [[Rcpp::export(".relativeHumidityFromMinMaxTemp")]]
NumericVector relativeHumidityFromMinMaxTemp(NumericVector Tmin,NumericVector Tmax) {
  NumericVector rh(Tmin.size());
  for(int i=0;i<Tmin.size();i++) rh[i] = relativeHumidity(Tmax[i]*0.606+Tmin[i]*0.394, Tmin[i]);
  return(rh);
}

// [[Rcpp::export(".relativeHumidityFromDewpointTemp")]]
NumericVector relativeHumidityFromDewpointTemp(NumericVector T, NumericVector TD) {
  NumericVector rh(T.size());
  for(int i=0;i<T.size();i++) rh[i] = relativeHumidity(T[i], TD[i]);
  return(rh);
}

double interpolateTdewPoint(double xp, double yp, double zp, 
                            NumericVector X, NumericVector Y, NumericVector Z, NumericVector T, NumericVector zDif, NumericVector tDif,
                            double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3, bool debug = false){
  int nstations = X.size();
  // int nDif = tDif.size();
  NumericVector r(nstations);
  for(int i=0;i<nstations;i++) {
    r[i] = sqrt(pow(xp-X[i],2.0)+pow(yp-Y[i],2.0));
  }
  double Rp = estimateRp(r, iniRp, alpha, N, iterations);
  NumericVector W = gaussianFilter(r, Rp, alpha);
  double Wnum = 0.0;
  //Apply weighting
  for(int i=0;i<nstations;i++) {
    Wnum +=W[i]*T[i];
  }
  if(debug) Rcout<< " nst: "<< nstations<<" Wnum: "<<Wnum<<" sumW: "<<std::accumulate(W.begin(), W.end(), 0.0)<<"\n";
  return(Wnum/std::accumulate(W.begin(), W.end(), 0.0));
}


// [[Rcpp::export("interpolation_dewtemperature")]]
NumericVector interpolateTdewPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp, NumericVector X, NumericVector Y, NumericVector Z, NumericVector T,  
                                    double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3, bool debug = false){
  int npoints = Xp.size();
  int nstations = X.size();
  NumericVector Tp(npoints);
  NumericVector zDif(nstations*(nstations-1));
  NumericVector tDif(nstations*(nstations-1));
  int c = 0;
  for(int i=0;i<nstations;i++) {
    for(int j=0;j<i;j++) {
      zDif[c] = Z[i]-Z[j];
      tDif[c] = T[i]-T[j];
      c++;
    }
  }
  for(int i=0;i<npoints;i++) {
    Tp[i] = interpolateTdewPoint(Xp[i], Yp[i], Zp[i], X,Y,Z,T, zDif, tDif, iniRp, alpha, N, iterations, debug);
  }
  return(Tp);
}

// [[Rcpp::export(".interpolateTdewSeriesPoints")]]
NumericMatrix interpolateTdewSeriesPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp, NumericVector X, NumericVector Y, NumericVector Z, NumericMatrix T,  
                                          double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3, bool debug = false){
  int npoints = Xp.size();
  int nstations = X.size();
  int nDays = T.ncol();
  NumericMatrix Tp(npoints,nDays);
  LogicalVector missing(nstations);
  for(int d = 0;d<nDays;d++) {
    int nmis = 0;
    for(int i=0;i<nstations;i++) {
      missing[i] = (NumericVector::is_na(T(i,d)) || NumericVector::is_na(X[i]) || NumericVector::is_na(Y[i]));
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
    NumericVector Tpday = interpolateTdewPoints(Xp, Yp,Zp, Xday, Yday, Zday, Tday,
                                                iniRp, alpha, N, iterations, debug);
    for(int p=0;p<npoints;p++) {
      Tp(p,d) = Tpday[p];
    }
  }
  return(Tp);
}
