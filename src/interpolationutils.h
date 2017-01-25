#include <Rcpp.h>

#ifndef INTERPOLATIONUTILS_H
#define INTERPOLATIONUTILS_H
#endif
using namespace Rcpp;

double gaussianFilter(double r, double Rp, double alpha);
NumericVector gaussianFilter(NumericVector r, double Rp, double alpha);
double estimateRp(NumericVector r, double iniRp, double alpha, int N, int iterations = 3);

NumericVector weightedRegression(NumericVector Y, NumericVector X, NumericVector W);
NumericMatrix temporalSmoothing(NumericMatrix input, int numDays, bool prec);