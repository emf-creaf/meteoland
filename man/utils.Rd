\encoding{UTF-8}
\name{utils}
\alias{utils_airDensity}
\alias{utils_atmosphericPressure}
\alias{utils_averageDailyVP}
\alias{utils_averageDaylightTemperature}
\alias{utils_latentHeatVaporisation}
\alias{utils_latentHeatVaporisationMol}
\alias{utils_psychrometricConstant}
\alias{utils_saturationVP}
\alias{utils_saturationVaporPressureCurveSlope}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Physical utility functions
}
\description{
Set of functions used in the calculation of physical variables. 
}
\usage{
utils_airDensity(temperature, Patm)
utils_atmosphericPressure(elevation)
utils_averageDailyVP(Tmin, Tmax, RHmin, RHmax)
utils_averageDaylightTemperature(Tmin, Tmax)
utils_latentHeatVaporisation(temperature)
utils_latentHeatVaporisationMol(temperature)
utils_psychrometricConstant(temperature, Patm)
utils_saturationVP(temperature)
utils_saturationVaporPressureCurveSlope(temperature)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temperature}{Air temperature (ºC).}
  \item{Tmin, Tmax}{Minimum and maximum daily temperature (ºC).}
  \item{RHmin, RHmax}{Minimum and maximum relative humidity (\%).}
  \item{Patm}{Atmospheric air pressure (in kPa).}
  \item{elevation}{Elevation above sea level (in m).}
}

\value{
Values returned for each function are:
\itemize{
\item{\code{utils_airDensity}: air density (in kg·m-3).}
\item{\code{utils_atmosphericPressure}: Air atmospheric pressure (in kPa).}
\item{\code{utils_averageDailyVP}: average (actual) vapour pressure (in kPa).}
\item{\code{utils_averageDaylightTemperature}: average daylight air temperature (in ºC).}
\item{\code{utils_latentHeatVaporisation}: Latent heat of vaporisation (MJ·kg-1).} 
\item{\code{utils_latentHeatVaporisationMol}: Latent heat of vaporisation (J·mol-1).} 
\item{\code{utils_psychrometricConstant}: Psychrometric constant (kPa·ºC-1).} 
\item{\code{utils_saturationVP}: saturation vapour pressure (in kPa).} 
\item{\code{utils_saturationVaporPressureCurveSlope}: Slope of the saturation vapor pressure curve (kPa·ºC-1).} 
}
}
\references{
McMurtrie, R. E., D. A. Rook, and F. M. Kelliher. 1990. Modelling the yield of Pinus radiata on a site limited by water and nitrogen. Forest Ecology and Management 30:381–413.
 
McMahon, T. A., M. C. Peel, L. Lowe, R. Srikanthan, and T. R. McVicar. 2013. Estimating actual, potential, reference crop and pan evaporation using standard meteorological data: a pragmatic synthesis. Hydrology & Earth System Sciences 17:1331–1363. See also:  http://www.fao.org/docrep/x0490e/x0490e06.htm
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya
}