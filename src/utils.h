#include <Rcpp.h>

#ifndef UTILS_H
#define UTILS_H
#endif
using namespace Rcpp;


double atmosphericPressure(double elevation);
double airDensity(double temperature, double Patm);

double averageDaylightTemperature(double Tmin, double Tmax);
double saturationVapourPressure(double temperature);
double temp2SVP(double TD);
double relativeHumidity(double T,double TD);
double vapourPressureFromRH(double T, double RH);
double dewpointTemperatureFromRH(double T, double RH);
double averageDailyVapourPressure(double Tmin, double Tmax, double RHmin, double RHmax);

double latentHeatVaporisationMol(double temperature);
double latentHeatVaporisation(double temperature);
double saturationVaporPressureCurveSlope(double temperature);
double psychrometricConstant(double temperature, double Patm);

double PenmanPET(double latrad, double elevation, double slorad, double asprad, int J,
          double Tmin, double Tmax, double RHmin, double RHmax, double R_s,
          double u, double z = 10.0, double z0 = 0.001,
          double alpha = 0.25, String windfun = "1956");
double PenmanMonteithPET(double rc, double elevation, 
                         double Tmin, double Tmax,
                         double RHmin, double RHmax,
                         double Rn, double u = NA_REAL);