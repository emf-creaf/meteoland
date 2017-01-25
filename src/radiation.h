#include <Rcpp.h>

#ifndef RADIATION_H
#define RADIATION_H
#endif
using namespace Rcpp;


int julianDay(int year, int month, int day);
IntegerVector dateStringToJulianDays(CharacterVector dateStrings);
double solarDeclination(int J);
NumericVector sunRiseSet(double latrad, double slorad, double asprad, double delta);
double daylength(double latrad, double slorad, double asprad, double delta);
double daylengthseconds(double latrad, double slorad, double asprad, double delta);
double RpotInstant(double latrad, double slorad, double asprad, double delta, double H);
double RpotDay(double latrad,  double slorad, double asprad, double delta);
double RDay(double phi, double elevation, double slorad, double asprad, double delta,
            double diffTemp, double diffTempMonth, double VP, double P);


double outgoingLongwaveRadiation(double latrad, double elevation,  double slorad,  double asprad, double delta, 
                                 double vpa, double tmin, double tmax, double R_s);

double netRadiation(double latrad,  double elevation, double slorad, double asprad, double delta, 
                    double vpa, double tmin, double tmax, double R_s, 
                    double alpha = 0.08);