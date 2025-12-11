# Default interpolation parameters

Returns a list with the default parameterization for interpolation. Most
parameter values are set according to Thornton et al. (1997).

## Usage

``` r
defaultInterpolationParams()
```

## Value

A list with the following items (default values in brackets):

- `initial_Rp [= 140000]`: Initial truncation radius.

- `iterations [= 3]`: Number of station density iterations.

- `alpha_MinTemperature [= 3.0]`: Gaussian shape parameter for minimum
  temperature.

- `alpha_MaxTemperature [= 3.0]`: Gaussian shape parameter for maximum
  temperature.

- `alpha_DewTemperature [= 3.0]`: Gaussian shape parameter for dew-point
  temperature.

- `alpha_PrecipitationEvent [= 5.0]`: Gaussian shape parameter for
  precipitation events.

- `alpha_PrecipitationAmount [= 5.0]`: Gaussian shape parameter for the
  regression of precipitation amounts.

- `alpha_Wind [= 3.0]`: Gaussian shape parameter for wind.

- `N_MinTemperature [= 30]`: Average number of stations with non-zero
  weights for minimum temperature.

- `N_MaxTemperature [= 30]`: Average number of stations with non-zero
  weights for maximum temperature.

- `N_DewTemperature [= 30]`: Average number of stations with non-zero
  weights for dew-point temperature.

- `N_PrecipitationEvent [= 5]`: Average number of stations with non-zero
  weights for precipitation events.

- `N_PrecipitationAmount [= 20]`: Average number of stations with
  non-zero weights for the regression of precipitation amounts.

- `N_Wind [= 2]`: Average number of stations with non-zero weights for
  wind.

- `St_Precipitation [= 5]`: Number of days for the temporal smoothing of
  precipitation.

- `St_TemperatureRange [= 15]`: Number of days for the temporal
  smoothing of temperature range.

- `pop_crit [= 0.50]`: Critical precipitation occurrence parameter.

- `f_max [= 0.6]`: Maximum value for precipitation regression
  extrapolations (0.6 equals to a maximum of 4 times extrapolation).

- `wind_height [= 10]`: Wind measurement height (in m).

- `wind_roughness_height [= 0.001]`: Wind roughness height (in m), for
  PET calculations.

- `penman_albedo [= 0.25]`: Albedo for PET calculations.

- `penman_windfun [= "1956"]`: Wind speed function version, either
  "1948" or "1956", for PET calculation.

- `debug [= FALSE]`: Boolean flag to show extra console output.

## References

Thornton, P.E., Running, S.W., White, M. A., 1997. Generating surfaces
of daily meteorological variables over large regions of complex terrain.
J. Hydrol. 190, 214–251. doi:10.1016/S0022-1694(96)03128-9.

De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018)
Estimating daily meteorological data and downscaling climate models over
landscapes. Environmental Modelling and Software 108: 186-196.

## See also

[`interpolate_data`](https://emf-creaf.github.io/meteoland/reference/interpolate_data.md)

## Author

Miquel De Cáceres Ainsa, CREAF
