# Meteoland interpolator creation

Function to create the meteoland interpolator

## Usage

``` r
create_meteo_interpolator(
  meteo_with_topo,
  params = NULL,
  verbose = getOption("meteoland_verbosity", TRUE)
)
```

## Arguments

- meteo_with_topo:

  Meteo object, as returned by
  [`with_meteo`](https://emf-creaf.github.io/meteoland/reference/with_meteo.md)

- params:

  Interpolation parameters as a list. Typically the result of
  [`defaultInterpolationParams`](https://emf-creaf.github.io/meteoland/reference/defaultInterpolationParams.md).

- verbose:

  Logical indicating if the function must show messages and info.
  Default value checks `"meteoland_verbosity"` option and if not set,
  defaults to TRUE. It can be turned off for the function with FALSE, or
  session wide with `options(meteoland_verbosity = FALSE)`

## Value

an interpolator object (stars)

## Details

This function takes meteorology information and a list of interpolation
parameters and creates the interpolator object to be ready to use.

## See also

Other interpolator functions:
[`add_topo()`](https://emf-creaf.github.io/meteoland/reference/add_topo.md),
[`get_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/get_interpolation_params.md),
[`read_interpolator()`](https://emf-creaf.github.io/meteoland/reference/read_interpolator.md),
[`set_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/set_interpolation_params.md),
[`with_meteo()`](https://emf-creaf.github.io/meteoland/reference/with_meteo.md),
[`write_interpolator()`](https://emf-creaf.github.io/meteoland/reference/write_interpolator.md)

## Author

Victor Granda García, EMF-CREAF

Miquel De Cáceres Ainsa, EMF-CREAF

## Examples

``` r
# example meteo data
data(meteoland_meteo_example)

# create the interpolator with default params
with_meteo(meteoland_meteo_example) |>
  create_meteo_interpolator()
#> ℹ Checking meteorology object...
#> ✔ meteorology object ok
#> ℹ Creating interpolator...
#> Warning: No interpolation parameters provided, using defaults
#> ℹ Set the `params` argument to modify parameter default values
#> • Calculating smoothed variables...
#> • Updating intial_Rp parameter with the actual stations mean distance...
#> ✔ Interpolator created.
#> stars object with 2 dimensions and 13 attributes
#> attribute(s):
#>                                 Min.    1st Qu.    Median       Mean   3rd Qu.
#> Temperature               -14.200000   8.800000  12.60000  11.324991  14.80000
#> MinTemperature            -15.900000   3.300000   6.90000   5.883189   9.40000
#> MaxTemperature            -13.000000  13.900000  18.30000  17.364292  21.80000
#> RelativeHumidity           18.000000  57.000000  67.00000  67.720000  78.00000
#> Precipitation               0.000000   0.000000   0.00000   1.925434   0.10000
#> Radiation                   7.707484  17.717235  22.04597  20.762628  23.61352
#> WindDirection               0.000000 116.000000 217.00000 196.861908 275.00000
#> WindSpeed                   0.200000   0.900000   1.30000   1.603907   2.00000
#> elevation                   0.000000 147.000000 317.00000 515.629630 668.00000
#> aspect                      0.000000   0.000000   0.00000   0.000000   0.00000
#> slope                       0.000000   0.000000   0.00000   0.000000   0.00000
#> SmoothedPrecipitation       0.100000   1.862500   5.55000   6.197496   9.07500
#> SmoothedTemperatureRange    4.695000   9.863542  11.66307  11.400932  13.26868
#>                                 Max. NA's
#> Temperature                 23.40000  148
#> MinTemperature              20.10000  138
#> MaxTemperature              29.90000  139
#> RelativeHumidity           100.00000  145
#> Precipitation              160.90000   79
#> Radiation                   28.10878  139
#> WindDirection              359.00000 4171
#> WindSpeed                    6.90000 4160
#> elevation                 2535.00000    0
#> aspect                       0.00000    0
#> slope                        0.00000    0
#> SmoothedPrecipitation       65.10000  818
#> SmoothedTemperatureRange    17.72778  124
#> dimension(s):
#>         from  to         offset  delta  refsys point
#> date       1  30 2022-04-01 UTC 1 days POSIXct FALSE
#> station    1 189             NA     NA  WGS 84  TRUE
#>                                                       values
#> date                                                    NULL
#> station POINT (0.95172 41.6566),...,POINT (1.89716 42.32211)

# create the interpolator with some params changed
with_meteo(meteoland_meteo_example) |>
  create_meteo_interpolator(params = list(debug = TRUE))
#> ℹ Checking meteorology object...
#> ✔ meteorology object ok
#> ℹ Creating interpolator...
#> ℹ Some interpolation parameters are missing, using default values for those
#> • Calculating smoothed variables...
#> • Updating intial_Rp parameter with the actual stations mean distance...
#> ✔ Interpolator created.
#> stars object with 2 dimensions and 13 attributes
#> attribute(s):
#>                                 Min.    1st Qu.    Median       Mean   3rd Qu.
#> Temperature               -14.200000   8.800000  12.60000  11.324991  14.80000
#> MinTemperature            -15.900000   3.300000   6.90000   5.883189   9.40000
#> MaxTemperature            -13.000000  13.900000  18.30000  17.364292  21.80000
#> RelativeHumidity           18.000000  57.000000  67.00000  67.720000  78.00000
#> Precipitation               0.000000   0.000000   0.00000   1.925434   0.10000
#> Radiation                   7.707484  17.717235  22.04597  20.762628  23.61352
#> WindDirection               0.000000 116.000000 217.00000 196.861908 275.00000
#> WindSpeed                   0.200000   0.900000   1.30000   1.603907   2.00000
#> elevation                   0.000000 147.000000 317.00000 515.629630 668.00000
#> aspect                      0.000000   0.000000   0.00000   0.000000   0.00000
#> slope                       0.000000   0.000000   0.00000   0.000000   0.00000
#> SmoothedPrecipitation       0.100000   1.862500   5.55000   6.197496   9.07500
#> SmoothedTemperatureRange    4.695000   9.863542  11.66307  11.400932  13.26868
#>                                 Max. NA's
#> Temperature                 23.40000  148
#> MinTemperature              20.10000  138
#> MaxTemperature              29.90000  139
#> RelativeHumidity           100.00000  145
#> Precipitation              160.90000   79
#> Radiation                   28.10878  139
#> WindDirection              359.00000 4171
#> WindSpeed                    6.90000 4160
#> elevation                 2535.00000    0
#> aspect                       0.00000    0
#> slope                        0.00000    0
#> SmoothedPrecipitation       65.10000  818
#> SmoothedTemperatureRange    17.72778  124
#> dimension(s):
#>         from  to         offset  delta  refsys point
#> date       1  30 2022-04-01 UTC 1 days POSIXct FALSE
#> station    1 189             NA     NA  WGS 84  TRUE
#>                                                       values
#> date                                                    NULL
#> station POINT (0.95172 41.6566),...,POINT (1.89716 42.32211)
```
