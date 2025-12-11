# Package index

## Daily weather interpolation

Functions for daily weather interpolation

- [`add_topo()`](https://emf-creaf.github.io/meteoland/reference/add_topo.md)
  : Add topography data to meteo object
- [`create_meteo_interpolator()`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md)
  : Meteoland interpolator creation
- [`get_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/get_interpolation_params.md)
  : Retrieving interpolation parameters from interpolator object
- [`interpolate_data()`](https://emf-creaf.github.io/meteoland/reference/interpolate_data.md)
  : Interpolation process for spatial data
- [`defaultInterpolationParams()`](https://emf-creaf.github.io/meteoland/reference/defaultInterpolationParams.md)
  : Default interpolation parameters
- [`set_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/set_interpolation_params.md)
  : Setting interpolation parameters in an interpolator object
- [`summarise_interpolated_data()`](https://emf-creaf.github.io/meteoland/reference/summarise_interpolated_data.md)
  **\[experimental\]** : Summarise interpolated data by temporal
  dimension
- [`summarise_interpolator()`](https://emf-creaf.github.io/meteoland/reference/summarise_interpolator.md)
  **\[experimental\]** : Summarise interpolator objects by temporal
  dimension
- [`interpolation_cross_validation()`](https://emf-creaf.github.io/meteoland/reference/interpolator_calibration.md)
  [`interpolator_calibration()`](https://emf-creaf.github.io/meteoland/reference/interpolator_calibration.md)
  : Calibration and validation of interpolation procedures
- [`with_meteo()`](https://emf-creaf.github.io/meteoland/reference/with_meteo.md)
  : Ensure meteo object is ready to create an interpolator object

## Input/output functions

Input/output functions

- [`read_interpolator()`](https://emf-creaf.github.io/meteoland/reference/read_interpolator.md)
  : Read interpolator files
- [`write_interpolator()`](https://emf-creaf.github.io/meteoland/reference/write_interpolator.md)
  : Write the interpolator object

## Weather data reshape functions

Functions to reshape weather data from other packages

- [`meteospain2meteoland()`](https://emf-creaf.github.io/meteoland/reference/meteospain2meteoland.md)
  : From meteospain to meteoland meteo objects
- [`worldmet2meteoland()`](https://emf-creaf.github.io/meteoland/reference/worldmet2meteoland.md)
  : From worldmet to meteoland meteo objects
- [`complete_meteo()`](https://emf-creaf.github.io/meteoland/reference/complete_meteo.md)
  : Complete missing meteo variables

## Example datasets

Example datasets

- [`meteoland_interpolator_example`](https://emf-creaf.github.io/meteoland/reference/meteoland_interpolator_example.md)
  **\[experimental\]** : Example interpolator object
- [`meteoland_meteo_example`](https://emf-creaf.github.io/meteoland/reference/meteoland_meteo_example.md)
  **\[experimental\]** : Example data set for meteo data from weather
  stations
- [`meteoland_meteo_no_topo_example`](https://emf-creaf.github.io/meteoland/reference/meteoland_meteo_no_topo_example.md)
  **\[experimental\]** : Example data set for meteo data from weather
  stations, without topography
- [`meteoland_topo_example`](https://emf-creaf.github.io/meteoland/reference/meteoland_topo_example.md)
  **\[experimental\]** : Example data set for topography data from
  weather stations, without meteo
- [`points_to_interpolate_example`](https://emf-creaf.github.io/meteoland/reference/points_to_interpolate_example.md)
  **\[experimental\]** : Example data set of points for interpolation of
  weather variables
- [`raster_to_interpolate_example`](https://emf-creaf.github.io/meteoland/reference/raster_to_interpolate_example.md)
  **\[experimental\]** : Example raster data set for interpolation of
  weather variables

## Low-level interpolation functions

Low-level interpolation functions

- [`interpolation_precipitation()`](https://emf-creaf.github.io/meteoland/reference/interpolation_temperature.md)
  [`interpolation_dewtemperature()`](https://emf-creaf.github.io/meteoland/reference/interpolation_temperature.md)
  [`interpolation_temperature()`](https://emf-creaf.github.io/meteoland/reference/interpolation_temperature.md)
  [`interpolation_wind()`](https://emf-creaf.github.io/meteoland/reference/interpolation_temperature.md)
  : Low-level interpolation functions

## Solar radiation utility functions

Solar radiation utility functions

- [`radiation_julianDay()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_dateStringToJulianDays()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_solarDeclination()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_solarConstant()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_sunRiseSet()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_solarElevation()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_daylength()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_daylengthseconds()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_potentialRadiation()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_solarRadiation()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_directDiffuseInstant()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_directDiffuseDay()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_skyLongwaveRadiation()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_outgoingLongwaveRadiation()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  [`radiation_netRadiation()`](https://emf-creaf.github.io/meteoland/reference/radiation_julianDay.md)
  : Solar radiation utility functions

## Other utility functions

Meteorological and biophysical utility functions

- [`utils_saturationVP()`](https://emf-creaf.github.io/meteoland/reference/utils_saturationVP.md)
  [`utils_averageDailyVP()`](https://emf-creaf.github.io/meteoland/reference/utils_saturationVP.md)
  [`utils_atmosphericPressure()`](https://emf-creaf.github.io/meteoland/reference/utils_saturationVP.md)
  [`utils_airDensity()`](https://emf-creaf.github.io/meteoland/reference/utils_saturationVP.md)
  [`utils_averageDaylightTemperature()`](https://emf-creaf.github.io/meteoland/reference/utils_saturationVP.md)
  [`utils_latentHeatVaporisation()`](https://emf-creaf.github.io/meteoland/reference/utils_saturationVP.md)
  [`utils_latentHeatVaporisationMol()`](https://emf-creaf.github.io/meteoland/reference/utils_saturationVP.md)
  [`utils_psychrometricConstant()`](https://emf-creaf.github.io/meteoland/reference/utils_saturationVP.md)
  [`utils_saturationVaporPressureCurveSlope()`](https://emf-creaf.github.io/meteoland/reference/utils_saturationVP.md)
  : Physical utility functions
- [`penman()`](https://emf-creaf.github.io/meteoland/reference/penman.md)
  [`penmanmonteith()`](https://emf-creaf.github.io/meteoland/reference/penman.md)
  : Potential evapotranspiration
- [`humidity_relative2dewtemperature()`](https://emf-creaf.github.io/meteoland/reference/humidity-conversion-tools.md)
  [`humidity_dewtemperature2relative()`](https://emf-creaf.github.io/meteoland/reference/humidity-conversion-tools.md)
  [`humidity_specific2relative()`](https://emf-creaf.github.io/meteoland/reference/humidity-conversion-tools.md)
  [`humidity_relative2specific()`](https://emf-creaf.github.io/meteoland/reference/humidity-conversion-tools.md)
  : Humidity conversion tools
- [`precipitation_concentration()`](https://emf-creaf.github.io/meteoland/reference/precipitation_concentration.md)
  : Precipitation daily concentration
- [`precipitation_rainfall_erosivity()`](https://emf-creaf.github.io/meteoland/reference/precipitation_rainfall_erosivity.md)
  **\[experimental\]** : Precipitation rainfall erosivity
