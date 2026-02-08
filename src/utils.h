#include <Rcpp.h>

#ifndef UTILS_H
#define UTILS_H
using namespace Rcpp;


double temp2SVP(double TD);
double relativeHumidity(double T,double TD);
double vapourPressureFromRH(double T, double RH);
double dewpointTemperatureFromRH(double T, double RH);
#endif
