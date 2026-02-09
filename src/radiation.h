#include <Rcpp.h>

#ifndef RADIATION_H
#define RADIATION_H
#endif
using namespace Rcpp;

IntegerVector dateStringToJulianDays(CharacterVector dateStrings);

NumericVector directDiffuseInstant(double solarConstant, double latrad, double slorad, double asprad, double delta, 
                                   double hrad, double R_s, bool clearday);
DataFrame directDiffuseDay(double solarConstant, double latrad, double slorad, double asprad, double delta, 
                           double R_s, bool clearday, int nsteps = 24);

