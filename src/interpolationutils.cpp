#include <Rcpp.h>
#include <numeric>
using namespace Rcpp;

double gaussianFilter(double r, double Rp, double alpha) {
  if(r>Rp) return(0.0);
  return(exp(-alpha*pow(r/Rp,2.0))-exp(-alpha));
}

NumericVector gaussianFilter(NumericVector r, double Rp, double alpha) {
  int n = r.size();
  NumericVector w(n);
  for(int i=0;i<n;i++) {
    w[i] = gaussianFilter(r[i], Rp, alpha);
  }
  return(w);
}

double estimateRp(NumericVector r, double iniRp, double alpha, int N, int iterations = 3) {
  //Initialize with initial value
  double Rpest = iniRp;
  NumericVector wIni;
  double Dp;
  double Wmean = ((1.0-exp(-alpha))/alpha)-exp(-alpha);
  double Nstar = (double) 2.0*N;
  double Wsum;
  for(int it = 0;it<iterations;it++) {
    //Estimate weights and station density
    wIni = gaussianFilter(r,Rpest,alpha);
    Wsum = std::accumulate(wIni.begin(), wIni.end(), 0.0);
    Dp = (Wsum/Wmean)/(3.141592*pow(Rpest,2.0));
    if(it==(iterations-1)) Nstar = (double) N;
    Rpest = sqrt(Nstar/(Dp*3.141592));
    // Rcout<<Dp<< " "<<Wsum<<" "<< Wmean<<" "<<Rpest<<"\n";
  }
  return(Rpest);
}

NumericVector weightedRegression(NumericVector Y, NumericVector X, NumericVector W) {
  NumericVector XW = X*W;
  NumericVector YW = Y*W;
  int n = X.size();
  double Wsum = std::accumulate(W.begin(), W.end(), 0.0);
  W = (((double) n)/Wsum)*W; //Normalize weights to sum n
  double wMeanX = std::accumulate(XW.begin(), XW.end(), 0.0)/((double)n);
  double wMeanY = std::accumulate(YW.begin(), YW.end(), 0.0)/((double)n);
  double cov = 0.0, var = 0.0;
  for (int k = 0; k < n; k++) {
    var = var + W[k] * X[k] * X[k];
    cov = cov + W[k] * X[k] * Y[k];
  }
  cov = cov -((double)n)*wMeanX*wMeanY;
  var = var - ((double) n)*wMeanX*wMeanX;
  double b = cov/var;
  // Rcout<<" Xsum: "<< std::accumulate(X.begin(), X.end(), 0.0)<<"Wsum:"<<std::accumulate(W.begin(), W.end(), 0.0)<<" wMeanX: "<< wMeanX<<" wMeanY: "<<wMeanY<<" cov: "<< cov<<" var: "<< var<<"\n";
  double a = wMeanY-b*wMeanX;
  return(NumericVector::create(a,b));
}

// [[Rcpp::export(".temporalSmoothing")]]
NumericMatrix temporalSmoothing(NumericMatrix input, int numDays, bool prec) {
  int nrows = input.nrow();
  int ncols = input.ncol();
  int vec_size = (2*numDays) + 1;
  NumericVector filter(vec_size, 0.0), weights(vec_size,0.0);
  NumericMatrix output(nrows,ncols);
  for(int r = 0;r<nrows;r++) {  //Station loop
    for(int c = 0;c<ncols;c++) { //Day loop
      for(int fpos = -numDays;fpos<=numDays;fpos++) {
        if((c+fpos>-1) & (c+fpos<ncols)) {
          if(!NumericVector::is_na(input(r,c+fpos))) {
            filter[fpos+numDays] = input(r,c+fpos);
            weights[fpos+numDays] = 1.0;
            if(prec & (filter[fpos+numDays]==0.0)) weights[fpos+numDays] = 0.0;
          } else {
            filter[fpos+numDays] = 0.0;
            weights[fpos+numDays] = 0.0;
          }
        } else {
          filter[fpos+numDays] = 0.0;
          weights[fpos+numDays] = 0.0;
        }
      }
      double den = std::accumulate(weights.begin(), weights.end(), 0.0);
      if(den>0.0) output(r,c) = std::accumulate(filter.begin(), filter.end(), 0.0)/den;
      else output(r,c) = R_NaReal;
    }
  }
  return(output);
}



