#include <Rcpp.h>
#include <numeric>
#include "interpolationutils.h"

using namespace Rcpp;
double interpolateTemperaturePoint(double xp, double yp, double zp,
                                   NumericVector X, NumericVector Y, NumericVector Z, NumericVector T,
                                   NumericVector zDif, NumericVector tDif,
                                   double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3,
                                   bool debug = false){
  int nstations = X.size();
  int nDif = tDif.size();
  NumericVector r(nstations);
  for(int i=0;i<nstations;i++) {
    r[i] = sqrt(pow(xp-X[i],2.0)+pow(yp-Y[i],2.0));
  }
  double Rp = estimateRp(r, iniRp, alpha, N, iterations);
  NumericVector W = gaussianFilter(r, Rp, alpha);
  //Weights for weighted regression
  NumericVector WDif(nDif,0.0);
  int c = 0;
  for(int i=0;i<nstations;i++) {
    for(int j=0;j<i;j++) {
      WDif[c] = W[i]*W[j];
      // if(debug) Rcout<<" "<<tDif[c]<<" "<<WDif[c]<<"\n";
      c++;
    }
  }
  //Weighted regression
  NumericVector wr = weightedRegression(tDif, zDif, WDif);
  double Wnum = 0.0;
  //Apply weighting
  for(int i=0;i<nstations;i++) {
    // Rcout<<T[i]<<"/";
    Wnum +=W[i]*(T[i]+wr[0]+wr[1]*(zp-Z[i]));
  }
  if(debug) Rcout<< " nstations: "<< nstations<<" wr0: "<<wr[0]<<" wr1: "<< wr[1]<<" Wnum: "<<Wnum<<" sumW: "<<std::accumulate(W.begin(), W.end(), 0.0)<<"\n";
  return(Wnum/std::accumulate(W.begin(), W.end(), 0.0));
}


//' Low-level interpolation functions
//'
//' @description
//' `r lifecycle::badge("deprecated")`
//'
//' Low-level functions to interpolate meteorology (one day) on a set of points.
//'
//' @details
//' This functions exposes internal low-level interpolation functions written in C++
//' not intended to be used directly in any script or function. The are maintained for
//' compatibility with older versions of the package and future versions of meteoland
//' will remove this functions (they will be still accessible through the triple colon
//' notation (\code{:::}), but their use is not recommended)
//'
//'
//' @aliases interpolation_dewtemperature interpolation_temperature
//' interpolation_precipitation interpolation_wind
//' @param Xp,Yp,Zp Spatial coordinates and elevation (Zp; in m.a.s.l) of target
//' points.
//' @param X,Y,Z Spatial coordinates and elevation (Zp; in m.a.s.l) of reference
//' locations (e.g. meteorological stations).
//' @param T Temperature (e.g., minimum, maximum or dew temperature) at the
//' reference locations (in degrees).
//' @param P Precipitation at the reference locations (in mm).
//' @param Psmooth Temporally-smoothed precipitation at the reference locations
//' (in mm).
//' @param WS,WD Wind speed (in m/s) and wind direction (in degrees from north
//' clock-wise) at the reference locations.
//' @param iniRp Initial truncation radius.
//' @param iterations Number of station density iterations.
//' @param debug Boolean flag to show extra console output.
//' @param alpha,alpha_amount,alpha_event Gaussian shape parameter.
//' @param N,N_event,N_amount Average number of stations with non-zero weights.
//' @param popcrit Critical precipitation occurrence parameter.
//' @param fmax Maximum value for precipitation regression extrapolations (0.6
//' equals to a maximum of 4 times extrapolation).
//' @param directionsAvailable A flag to indicate that wind directions are
//' available (i.e. non-missing) at the reference locations.
//' @return All functions return a vector with interpolated values for the
//' target points.
//' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
//' @seealso \code{\link{defaultInterpolationParams}}
//' @references Thornton, P.E., Running, S.W., White, M. A., 1997. Generating
//' surfaces of daily meteorological variables over large regions of complex
//' terrain. J. Hydrol. 190, 214–251. doi:10.1016/S0022-1694(96)03128-9.
//'
//' De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating
//' daily meteorological data and downscaling climate models over landscapes.
//' Environmental Modelling and Software 108: 186-196.
//' @examples
//'
//' Xp <- as.numeric(sf::st_coordinates(points_to_interpolate_example)[,1])
//' Yp <- as.numeric(sf::st_coordinates(points_to_interpolate_example)[,2])
//' Zp <- points_to_interpolate_example$elevation
//' X <- as.numeric(
//'   sf::st_coordinates(stars::st_get_dimension_values(meteoland_interpolator_example, "station"))[,1]
//' )
//' Y <- as.numeric(
//'   sf::st_coordinates(stars::st_get_dimension_values(meteoland_interpolator_example, "station"))[,2]
//' )
//' Z <- as.numeric(meteoland_interpolator_example[["elevation"]][1,])
//' Temp <- as.numeric(meteoland_interpolator_example[["MinTemperature"]][1,])
//' P <- as.numeric(meteoland_interpolator_example[["Precipitation"]][1,])
//' Psmooth <- as.numeric(meteoland_interpolator_example[["SmoothedPrecipitation"]][1,])
//' WS <- as.numeric(meteoland_interpolator_example[["WindSpeed"]][1,])
//' WD <- as.numeric(meteoland_interpolator_example[["WindDirection"]][1,])
//' iniRp <- get_interpolation_params(meteoland_interpolator_example)$initial_Rp
//' alpha <- get_interpolation_params(meteoland_interpolator_example)$alpha_MinTemperature
//' N <- get_interpolation_params(meteoland_interpolator_example)$N_MinTemperature
//' alpha_event <- get_interpolation_params(meteoland_interpolator_example)$alpha_PrecipitationEvent
//' N_event <- get_interpolation_params(meteoland_interpolator_example)$N_PrecipitationEvent
//' alpha_amount <- get_interpolation_params(meteoland_interpolator_example)$alpha_PrecipitationAmount
//' N_amount <- get_interpolation_params(meteoland_interpolator_example)$N_PrecipitationAmount
//' alpha_wind <- get_interpolation_params(meteoland_interpolator_example)$alpha_Wind
//' N_wind <- get_interpolation_params(meteoland_interpolator_example)$N_Wind
//' iterations <- get_interpolation_params(meteoland_interpolator_example)$iterations
//' popcrit <- get_interpolation_params(meteoland_interpolator_example)$pop_crit
//' fmax <- get_interpolation_params(meteoland_interpolator_example)$f_max
//' debug <- get_interpolation_params(meteoland_interpolator_example)$debug
//'
//' interpolation_temperature(
//'   Xp, Yp, Zp,
//'   X[!is.na(Temp)], Y[!is.na(Temp)], Z[!is.na(Temp)],
//'   Temp[!is.na(Temp)],
//'   iniRp, alpha, N, iterations, debug
//' )
//'
//' interpolation_wind(
//'   Xp, Yp,
//'   WS[!is.na(WD)], WD[!is.na(WD)],
//'   X[!is.na(WD)], Y[!is.na(WD)],
//'   iniRp, alpha_wind, N_wind, iterations, directionsAvailable = FALSE
//' )
//'
//' interpolation_precipitation(
//'   Xp, Yp, Zp,
//'   X[!is.na(P)], Y[!is.na(P)], Z[!is.na(P)],
//'   P[!is.na(P)], Psmooth[!is.na(P)],
//'   iniRp, alpha_event, alpha_amount, N_event, N_amount,
//'   iterations, popcrit, fmax, debug
//' )
//'
//' @export
// [[Rcpp::export("interpolation_temperature")]]
NumericVector interpolateTemperaturePoints(NumericVector Xp, NumericVector Yp, NumericVector Zp, NumericVector X, NumericVector Y, NumericVector Z, NumericVector T,
                                           double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3, bool debug = false){
  int npoints = Xp.size();
  int nstations = X.size();
  NumericVector Tp(npoints);
  NumericVector zDif(nstations*(nstations-1)/2);
  NumericVector tDif(nstations*(nstations-1)/2);
  int c = 0;
  for(int i=0;i<nstations;i++) {
    for(int j=0;j<i;j++) {
      zDif[c] = Z[i]-Z[j];
      tDif[c] = T[i]-T[j];
      c++;
    }
  }
  for(int i=0;i<npoints;i++) {
    Tp[i] = interpolateTemperaturePoint(Xp[i], Yp[i], Zp[i], X,Y,Z,T, zDif, tDif,
                                        iniRp, alpha, N, iterations, debug);
  }
  return(Tp);
}

// [[Rcpp::export(".interpolateTemperatureSeriesPoints")]]
NumericMatrix interpolateTemperatureSeriesPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp,
                                                 NumericVector X, NumericVector Y, NumericVector Z, NumericMatrix T,
                                                 double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3, bool debug = false){
  int npoints = Xp.size();
  int nstations = X.size();
  int nDays = T.ncol();
  NumericMatrix Tp(npoints,nDays);
  LogicalVector missing(nstations);
  for(int d = 0;d<nDays;d++) {
    // Rcout<<"Day: "<<d<<"\n";
    int nmis = 0;
    for(int i=0;i<nstations;i++) {
      missing[i] = (NumericVector::is_na(T(i,d)) || NumericVector::is_na(X[i]) || NumericVector::is_na(Y[i]) || NumericVector::is_na(Z[i]));
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
    // Rcout<<"non-missing: "<<c;
    NumericVector Tpday = interpolateTemperaturePoints(Xp, Yp,Zp,
                                                       Xday, Yday, Zday, Tday,
                                                       iniRp, alpha, N, iterations, debug);
    for(int p=0;p<npoints;p++) {
      Tp(p,d) = Tpday[p];
      // Rcout<<" value: "<<Tpday[p]<<"\n";
    }
  }
  return(Tp);
}
