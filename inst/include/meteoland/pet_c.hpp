#include "utils_c.hpp"
#include "radiation_c.hpp"
#ifndef PET_C_H
#define PET_C_H

inline double PenmanPET_c(double latrad, double elevation, double slorad, double asprad, int J,
                   double Tmin, double Tmax, double RHmin, double RHmax, double R_s,
                   double u, double z = 10.0, double z0 = 0.001,
                   double alpha = 0.25, std::string windfun = "1956") {
  double Tday = (Tmax + Tmin)/2.0;
  double RHmean = (RHmax + RHmin)/2.0;
  //Atmospheric pressure kPa
  double Patm = atmosphericPressure_c(elevation);
  //Slope of the saturation vapor pressure curve kPa.Celsius^-1
  double delta = saturationVaporPressureCurveSlope_c(Tday);
  //Latent heat of vaporisation MJ.kg^-1
  double lambda = latentHeatVaporisation_c(Tday);
  //Psychrometric constant kPa.Celsius^-1
  double gamma = psychrometricConstant_c(Tday, Patm);
  //Solar declination
  double delta2 =  solarDeclination_c(J);
  //Solar constant
  double Gsc = solarConstant_c(J);
  // double d_r2 = 1.0 + 0.033 * cos(2.0 * PI/365.0 * ((double)J));
  // double w_s = acos(-tan(latitude) * tan(delta2));
  // double N = 24.0/PI*w_s; (N not used)
  
  double PET = 0.0;
  if(!std::isnan(u)) {
    double u2 = u * log(2.0/z0)/log(z/z0);
    double vs_Tmax = saturationVapourPressure_c(Tmax);
    double vs_Tmin = saturationVapourPressure_c(Tmin);
    double vas = (vs_Tmax + vs_Tmin)/2.0;
    double vpa = (vs_Tmin * (RHmax/100.0) + vs_Tmax * (RHmin/100.0))/2.0;
    double R_n = netRadiation_c(Gsc, latrad, elevation, slorad, asprad, delta2,
                                vpa, Tmin, Tmax, R_s,
                                alpha);//Net radiation
    double Ea = (vas - vpa); //Saturation vapor deficit
    if(windfun == "1956") {
      Ea = Ea*(1.313 + 1.381 * u2);
    } else if(windfun == "1948") {
      Ea = Ea*(2.626 + 1.381*u2);
    }
    PET = delta/(delta + gamma) * (R_n/lambda) + gamma/(delta + gamma) * Ea;
  } else {
    //Equation by Valiantzas (2006, eq 33) for situations where wind is not available
    //Valiantzas JD (2006) Simplified versions for the Penman evaporation equation using routine weather data. Journal of Hydrology 331, 690–702. doi:10.1016/j.jhydrol.2006.06.012.
    double R_a = RpotDay_c(Gsc, latrad,  slorad, asprad, delta2); //Extraterrestrial (potential) radiation
    double R_ratio = R_s/R_a;
    if(R_a==0.0) R_ratio = 0.0;
    R_ratio = std::min(std::max(R_ratio, 0.0), 1.0);
    double wf = 0.09;
    if(windfun == "1956") {
      wf = 0.06;
    } else if(windfun == "1948") {
      wf = 0.09;
    }
    PET = 0.047 * R_s * sqrt(Tday + 9.5) - 2.4 * pow(R_ratio,2.0) + wf * (Tday + 20.0) * (1.0 - RHmean/100.0);
  }
  if(PET<0.0) PET = 0.0;
  return(PET);
}

/**
 *   Calculates daily potential evapotranspiration (mm.m^-2.day^-1) for a surface given microenvironmental conditions
 *   (absorbed radiation, wind, temperature, relative humidity) and given a canopy stomatal resistance.
 *   Basically is the Penman-Monteith equation for daily steps, where aerodynamic resistance to heat flux
 *   is calculated internally (from wind and temperature) and the canopy (stomatal) resistance is an input.
 *
 *
 *   REF:  McMahon et al. (2013)
 *   Estimating actual, potential, reference crop and pan evaporation using standard meteorological data: a pragmatic synthesis
 *   Hydrology & Earth System Sciences
 *   See also:  http://www.fao.org/docrep/x0490e/x0490e06.htm
 *
 *   rc[s/m] - Canopy vapour flux (stomatal) resistance
 *   elevation [m] - Elevation over sea level
 *   Tmin, Tmax [Celsius] - Minimum and maximum temperature
 *   RHmin, RHmax [%] - Minimum and maximum relative humidity
 *   Rn [MJ.m^-2.day^-1] - daily net radiation
 *   u [m.s^-1] - wind speed
 */
inline double PenmanMonteithPET_c(double rc, double elevation,
                           double Tmin, double Tmax,
                           double RHmin, double RHmax,
                           double Rn, double u) {
  double Tday = (Tmax + Tmin)/2.0;
  //Saturation vapour pressure at temperature kPa
  double vs_Tmax = saturationVapourPressure_c(Tmax);
  double vs_Tmin = saturationVapourPressure_c(Tmin);
  //Daily saturation vapour pressure kPa
  double vas = (vs_Tmax + vs_Tmin)/2.0;
  //Mean daily actual vapour pressure, kPa
  double vabar = (vs_Tmin * RHmax/100.0 + vs_Tmax * RHmin/100.0)/2.0;
  //Atmospheric pressure kPa
  double Patm = atmosphericPressure_c(elevation);
  //Slope of the saturation vapor pressure curve kPa.Celsius^-1
  double delta = saturationVaporPressureCurveSlope_c(Tday);
  //Vapour pressure deficit, kPa
  double vpd = (vas - vabar);
  //Latent heat of vaporisation MJ.kg^-1
  double lambda = latentHeatVaporisation_c(Tday);
  // Rcout<<"Lambda: "<< lambda<<"\n";
  //Psychrometric constant kPa.Celsius^-1
  double gamma = psychrometricConstant_c(Tday, Patm);
  // Rcout<<"Gamma: "<< gamma<<"\n";
  //Density of air kg.m^-3
  double rho = airDensity_c(Tday, Patm);
  //Unit conversion from s^-1 to day^-1
  double Kt = 86400.0;
  
  //Radiative resistance to heat flux s.m^-1
  // double rr = Kt*(rho*Cp)/(4.0*SIGMA*pow(Tday+273.0,3.0));
  //Convective resistance to heat flux s.m^-1
  if(std::isnan(u)) u = 2.0;
  u = std::max(u, 0.000001);
  double rh = 208.0/u; //Aerodynamic resistance to convective heat transfer
  //Resistance to sensible heat flux (from Biome BGC) s.m^-1
  // double ra = (rh*rr)/(rh+rr);
  rc = std::max(rc, 70.0);
  double ra = rh;
  //   double N1fao = (0.408*delta*(Rn));
  //   double N2fao = gamma*(900.0/(Tday+273.0))*u*vpd;
  //   double Dfao = delta + gamma*(1.0+0.34*u);
  //   double Efao = (N1fao+N2fao)/(Dfao);
  //   Rcout<<"N1(FAO) : "<<N1fao<< " N2(FAO) : "<<N2fao<<" D(FAO) "<<Dfao<<" E(FAO) "<< Efao <<"\n";
  double N1 = ((delta/lambda)*Rn);
  double N2 = Kt*(vpd*(rho*Cp_MJKG)/(lambda*ra));
  double D = delta + gamma*(1.0+(rc/ra));
  double E = (N1+ N2)/(D);
  // Rcout<<"N1 : "<<N1<< " N2 : "<<N2<<" D "<<D<<" E "<< E <<"\n";
  return(E);
}


#endif
