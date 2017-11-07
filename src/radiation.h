#include <Rcpp.h>

#ifndef RADIATION_H
#define RADIATION_H
#endif
using namespace Rcpp;


int julianDay(int year, int month, int day);
IntegerVector dateStringToJulianDays(CharacterVector dateStrings);
double solarDeclination(int J);
double solarConstant(int J);
double solarElevation(double latrad, double delta, double h);

NumericVector sunRiseSet(double latrad, double slorad, double asprad, double delta);
double daylength(double latrad, double slorad, double asprad, double delta);
double daylengthseconds(double latrad, double slorad, double asprad, double delta);

double RpotInstant(double solarConstant, double latrad, double slorad, double asprad, double delta, double H);
double RpotDay(double solarConstant, double latrad,  double slorad, double asprad, double delta);
double RDay(double solarConstant, double phi, double elevation, double slorad, double asprad, double delta,
            double diffTemp, double diffTempMonth, double VP, double P);

NumericVector directDiffuseInstant(double solarConstant, double latrad, double slorad, double asprad, double delta, 
                                   double hrad, double R_p, double R_s, bool clearday);
DataFrame directDiffuseDay(double solarConstant, double latrad, double slorad, double asprad, double delta, 
                           double R_s, bool clearday, int nsteps = 24);

double skyLongwaveRadiation(double Tair, double vpa, double c = 0);

double outgoingLongwaveRadiation(double solarConstant, double latrad, double elevation,  double slorad,  double asprad, double delta, 
                                 double vpa, double tmin, double tmax, double R_s);

double netRadiation(double solarConstant, double latrad,  double elevation, double slorad, double asprad, double delta, 
                    double vpa, double tmin, double tmax, double R_s, 
                    double alpha = 0.08);