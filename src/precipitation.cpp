#include <Rcpp.h>
#include "interpolationutils.h"
using namespace Rcpp;

double interpolatePrecipitationPoint(double xp, double yp, double zp, NumericVector X, NumericVector Y, NumericVector Z, NumericVector P, NumericVector zDif, NumericVector pRat,
                                     double iniRp = 140000, double alpha_event = 6.25, double alpha_amount = 6.25, int N_event = 20,int N_amount = 20, int iterations = 3,
                                     double popcrit = 0.5, double fmax = 0.95,
                                     bool debug = false){
  int nstations = X.size();
  int nDif = pRat.size();
  NumericVector r(nstations);
  for(int i=0;i<nstations;i++) {
    r[i] = sqrt(pow(xp-X[i],2.0)+pow(yp-Y[i],2.0));
  }
  double Rp = estimateRp(r, iniRp, alpha_event, N_event, iterations);
  NumericVector Wevent = gaussianFilter(r, Rp, alpha_event);
  double Weventsum = std::accumulate(Wevent.begin(), Wevent.end(), 0.0);
  //Probability of precipitation
  double pop = 0.0;
  for(int i=0;i<nstations;i++) {
    if(P[i]>0) {
      pop += Wevent[i];
    }
  }
  pop = pop/Weventsum;
  // Rcout<<" Rp "<<Rp<<" Weventsum "<<Weventsum<<" pop "<< pop<<"\n";
  //If precipitation occurs then calculate amount
  if(pop >=popcrit) {
    Rp = estimateRp(r, iniRp, alpha_amount, N_amount, iterations);
    NumericVector Wamount = gaussianFilter(r, Rp, alpha_amount);
    //Weights for weighted regression
    NumericVector WDif(nDif);
    int c = 0;
    for(int i=0;i<nstations;i++) {
      for(int j=0;j<i;j++) {
        WDif[c] = Wamount[i]*Wamount[j];
        c++;
      }
    }
    //Weighted regression
    NumericVector wr = weightedRegression(pRat, zDif, WDif);
       // Rcout<<Rp<<" " <<wr[0]<<" " << wr[1]<<"\n";
    double Wnum = 0.0, Wden = 0.0, f = 0.0;
    //Apply weighting
    for(int i=0;i<nstations;i++) {
      if(P[i]>0) {
        f = wr[0]+wr[1]*(zp-Z[i]);
               // Rcout<<f<<"\n";
        if(f>fmax) f = fmax;
        else if(f< ((-1.0)*fmax)) f = -1.0*fmax;
        // if(Wevent[i]>0.0) Rcout<<" z:"<<Z[i]<<" P:"<<P[i]<<" f: "<<f<<" p:"<<((1.0+f)/(1.0-f))<< " wev:"<< Wevent[i]<<"\n";
        Wnum +=Wamount[i]*P[i]*((1.0+f)/(1.0-f));
        Wden +=Wamount[i];
      }
    }
    // Rcout <<" PRED: "<< Wnum<<" "<<Wden<<" "<<Wnum/Wden<<"\n";
    return(Wnum/Wden);
  }
  return(0.0);
}

double interpolatePrecipitationEventPoint(double xp, double yp, double zp, NumericVector X, NumericVector Y, NumericVector Z, NumericVector Pevent,
                                          double iniRp = 140000, double alpha = 6.25, int N = 20, int iterations = 3, double popcrit = 0.5){
  int nstations = X.size();
  NumericVector r(nstations);
  for(int i=0;i<nstations;i++) {
    r[i] = sqrt(pow(xp-X[i],2.0)+pow(yp-Y[i],2.0));
  }
  double Rp = estimateRp(r, iniRp, alpha, N, iterations);
  NumericVector W = gaussianFilter(r, Rp, alpha);
  double Wsum = std::accumulate(W.begin(), W.end(), 0.0);
  //Probability of precipitation
  double pop = 0.0;
  for(int i=0;i<nstations;i++) {
    if(Pevent[i]>0) {
      pop += W[i];
    }
  }
  pop = pop/Wsum;
  //If precipitation occurs then calculate amount
  if(pop >=popcrit) return(1.0);
  return(0.0);
}

// [[Rcpp::export("interpolation_precipitation")]]
NumericVector interpolatePrecipitationPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp, NumericVector X, NumericVector Y, NumericVector Z, NumericVector P, NumericVector Psmooth,
                                             double iniRp = 140000, double alpha_event = 6.25, double alpha_amount = 6.25,
                                             int N_event = 20, int N_amount = 20,int iterations = 3, double popcrit = 0.5, double fmax = 0.95,
                                             bool debug = false){
  int npoints = Xp.size();
  int nstations = X.size();
  NumericVector Pp(npoints);
  NumericVector zDif(nstations*(nstations-1)/2);
  NumericVector pRat(nstations*(nstations-1)/2);
  int c = 0;
  for(int i=0;i<nstations;i++) {
    for(int j=0;j<i;j++) {
      zDif[c] = Z[i]-Z[j];
      if((Psmooth[i]+Psmooth[j])>0.0) pRat[c] = (Psmooth[i]-Psmooth[j])/(Psmooth[i]+Psmooth[j]);
      c++;
    }
  }
  for(int i=0;i<npoints;i++) {
    Pp[i] = interpolatePrecipitationPoint(Xp[i], Yp[i], Zp[i], X,Y,Z,P, zDif, pRat,
                                          iniRp, alpha_event, alpha_amount, N_event, N_amount, iterations, popcrit, fmax,
                                          debug);
  }
  return(Pp);
}

NumericVector interpolatePrecipitationEventPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp,
                                                  NumericVector X, NumericVector Y, NumericVector Z, NumericVector Pevent,
                                                  double iniRp = 140000, double alpha = 6.25, int N = 20, int iterations = 3, double popcrit = 0.5){
  int npoints = Xp.size();
  NumericVector Pp(npoints);
  for(int i=0;i<npoints;i++) {
    Pp[i] = interpolatePrecipitationEventPoint(Xp[i], Yp[i], Zp[i], X,Y,Z,Pevent,
                                               iniRp, alpha, N, iterations, popcrit);
  }
  return(Pp);
}

// [[Rcpp::export(".interpolatePrecipitationSeriesPoints")]]
NumericMatrix interpolatePrecipitationSeriesPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp, NumericVector X, NumericVector Y, NumericVector Z, NumericMatrix P, NumericMatrix Psmooth,
                                                   double iniRp = 140000, double alpha_event = 6.25, double alpha_amount = 6.25, int N_event = 20, int N_amount = 20,
                                                   int iterations = 3, double popcrit = 0.5, double fmax = 0.95,
                                                   bool debug = false){
  int npoints = Xp.size();
  int nstations = X.size();
  int nDays = P.ncol();
  NumericMatrix Pp(npoints,nDays);
  LogicalVector missing(nstations);
  for(int d = 0;d<nDays;d++) {
    int nmis = 0;
    for(int i=0;i<nstations;i++) {
      missing[i] = (NumericVector::is_na(P(i,d)) | NumericVector::is_na(X[i])| NumericVector::is_na(Y[i])| NumericVector::is_na(Z[i]));
      if(missing[i]) nmis++;
    }
    if(debug) Rcout << "Day "<< d << " nexcluded = " << nmis;
    NumericVector Pday(nstations-nmis);
    NumericVector Psmoothday(nstations-nmis);
    NumericVector Xday(nstations-nmis);
    NumericVector Yday(nstations-nmis);
    NumericVector Zday(nstations-nmis);
    int c = 0;
    for(int i=0;i<nstations;i++) {
      if(!missing[i]) {
        Pday[c] = P(i,d);
        Psmoothday[c] = Psmooth(i,d);
        Xday[c] = X[i];
        Yday[c] = Y[i];
        Zday[c] = Z[i];
        c++;
      }
    }
    NumericVector Ppday = interpolatePrecipitationPoints(Xp, Yp,Zp, Xday, Yday, Zday, Pday,Psmoothday, iniRp, alpha_event, alpha_amount,
                                                         N_event, N_amount, iterations, popcrit, fmax, debug);
    for(int p=0;p<npoints;p++) {
      Pp(p,d) = Ppday[p];
    }
  }
  return(Pp);
}

// [[Rcpp::export(".interpolatePrecipitationEventSeriesPoints")]]
NumericMatrix interpolatePrecipitationEventSeriesPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp,
                                                        NumericVector X, NumericVector Y, NumericVector Z, NumericMatrix Pevent,
                                                        double iniRp = 140000, double alpha = 6.25, int N = 20, int iterations = 3, double popcrit = 0.5){
  int npoints = Xp.size();
  int nstations = X.size();
  int nDays = Pevent.ncol();
  NumericMatrix Pp(npoints,nDays);
  LogicalVector missing(nstations);
  for(int d = 0;d<nDays;d++) {
    //    Rcout<<"Day: "<<d<<"\n";
    int nmis = 0;
    for(int i=0;i<nstations;i++) {
      missing[i] = NumericVector::is_na(Pevent(i,d));
      if(missing[i]) nmis++;
    }
    NumericVector Pevday(nstations-nmis);
    NumericVector Xday(nstations-nmis);
    NumericVector Yday(nstations-nmis);
    NumericVector Zday(nstations-nmis);
    int c = 0;
    for(int i=0;i<nstations;i++) {
      if(!missing[i]) {
        Pevday[c] = Pevent(i,d);
        Xday[c] = X[i];
        Yday[c] = Y[i];
        Zday[c] = Z[i];
        c++;
      }
    }
    NumericVector Ppday = interpolatePrecipitationEventPoints(Xp, Yp,Zp,
                                                              Xday, Yday, Zday, Pevday,
                                                              iniRp, alpha, N, iterations, popcrit);
    for(int p=0;p<npoints;p++) {
      Pp(p,d) = Ppday[p];
    }
  }
  return(Pp);
}

// [[Rcpp::export(".pseudoRainfall")]]
NumericVector pseudoRainfall(NumericVector RainM, NumericVector daysMonthAll, double shape = 2.0, double scale = 4.0, int firstMonth = 1) {
  RNGScope scope;
  
  int nMonths = RainM.size();
  //Determines the number of days
  int numDays = 0;
  int month = firstMonth;
  for(int i=0;i<nMonths;i++) {
    numDays +=daysMonthAll[i];
    if(month==12) month=1;
    else month +=1;
  }
  
  NumericVector Rainfall=rep(0.0,numDays);
  NumericVector g = rgamma(numDays,shape, scale);
  NumericVector pd = runif(numDays);
  int cg = 0;
  int cumDays = 0;
  double NMD = 0, pMonth, rd;
  
  for(int m=0; m<nMonths; m++) {
    NMD = daysMonthAll[m];
    pMonth = RainM[m];
    while(pMonth>0){
      rd = g[cg];
      if(cg==numDays) cg=0;
      if(rd>pMonth) rd = pMonth;
      Rainfall[cumDays+((int)(pd[cg]*NMD))] += rd;
      pMonth = pMonth - rd;
      cg++;
    }
    if(RainM[m]>0) for(int d = cumDays; d<(cumDays+NMD);d++) Rainfall[d] /=RainM[m];
    cumDays += (int) NMD;
  }
  return(Rainfall);
}

