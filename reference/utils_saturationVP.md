# Physical utility functions

Set of functions used in the calculation of physical variables.

## Usage

``` r
utils_saturationVP(temperature)

utils_averageDailyVP(Tmin, Tmax, RHmin, RHmax)

utils_atmosphericPressure(elevation)

utils_airDensity(temperature, Patm)

utils_averageDaylightTemperature(Tmin, Tmax)

utils_latentHeatVaporisation(temperature)

utils_latentHeatVaporisationMol(temperature)

utils_psychrometricConstant(temperature, Patm)

utils_saturationVaporPressureCurveSlope(temperature)
```

## Arguments

- temperature:

  Air temperature (ºC).

- Tmin, Tmax:

  Minimum and maximum daily temperature (ºC).

- RHmin, RHmax:

  Minimum and maximum relative humidity (%).

- elevation:

  Elevation above sea level (in m).

- Patm:

  Atmospheric air pressure (in kPa).

## Value

Values returned for each function are:

- `utils_airDensity`: air density (in kg·m-3).

- `utils_atmosphericPressure`: Air atmospheric pressure (in kPa).

- `utils_averageDailyVP`: average (actual) vapour pressure (in kPa).

- `utils_averageDaylightTemperature`: average daylight air temperature
  (in ºC). `utils_latentHeatVaporisation`: Latent heat of vaporisation
  (MJ·kg-1). `utils_latentHeatVaporisationMol`: Latent heat of
  vaporisation (J·mol-1).

- `utils_psychrometricConstant`: Psychrometric constant (kPa·ºC-1).

- `utils_saturationVP`: saturation vapour pressure (in kPa).

- `utils_saturationVaporPressureCurveSlope`: Slope of the saturation
  vapor pressure curve (kPa·ºC-1).

## Functions

- `utils_averageDailyVP()`: Average daily VP

- `utils_atmosphericPressure()`: Atmospheric pressure

- `utils_airDensity()`: Air density

- `utils_averageDaylightTemperature()`: Daylight temperature

- `utils_latentHeatVaporisation()`: latent heat vaporisation

- `utils_latentHeatVaporisationMol()`: Heat vaporisation mol

- `utils_psychrometricConstant()`: psychrometric constant

- `utils_saturationVaporPressureCurveSlope()`: Saturation VP curve slope

## References

McMurtrie, R. E., D. A. Rook, and F. M. Kelliher. 1990. Modelling the
yield of Pinus radiata on a site limited by water and nitrogen. Forest
Ecology and Management 30:381–413.

McMahon, T. A., M. C. Peel, L. Lowe, R. Srikanthan, and T. R. McVicar.
2013. Estimating actual, potential, reference crop and pan evaporation
using standard meteorological data: a pragmatic synthesis. Hydrology &
Earth System Sciences 17:1331–1363. See also:
http://www.fao.org/docrep/x0490e/x0490e06.htm

## Author

Miquel De Cáceres Ainsa, CREAF
