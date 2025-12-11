# Summarise interpolator objects by temporal dimension

**\[experimental\]**

Summarises an interpolator object by the desired temporal scale.

## Usage

``` r
summarise_interpolator(
  interpolator,
  fun = "mean",
  frequency = NULL,
  vars_to_summary = c("Temperature", "MinTemperature", "MaxTemperature", "Precipitation",
    "RelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity", "Radiation",
    "WindSpeed", "WindDirection", "PET", "SmoothedPrecipitation",
    "SmoothedTemperatureRange", "elevation", "slope", "aspect"),
  dates_to_summary = NULL,
  months_to_summary = 1:12,
  verbose = getOption("meteoland_verbosity", TRUE),
  ...
)
```

## Arguments

- interpolator:

  An interpolator object as created by
  [`create_meteo_interpolator`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md).

- fun:

  The function to use for summarising the data.

- frequency:

  A string indicating the interval specification (allowed ones are
  "week", "month", "quarter" and "year"). If NULL (default), aggregation
  is done in one interval for all the dates present.

- vars_to_summary:

  A character vector with one or more variable names to summarise. By
  default, all interpolated variables are summarised.

- dates_to_summary:

  A Date object to define the dates to be summarised. If NULL (default),
  all dates in the interpolated data are processed.

- months_to_summary:

  A numeric vector with the month numbers to subset the data before
  summarising. (e.g. `c(7,8)` for July and August). This parameter
  allows studying particular seasons, when combined with `frequency`.
  For example `frequency = "years"` and `months = 6:8` leads to
  summarizing summer months of each year.

- verbose:

  Logical indicating if the function must show messages and info.
  Default value checks `"meteoland_verbosity"` option and if not set,
  defaults to TRUE. It can be turned off for the function with FALSE, or
  session wide with `options(meteoland_verbosity = FALSE)`

- ...:

  Arguments needed for `fun`

## Value

`summarise_interpolator` function returns the same interpolator object
provided with the temporal dimension aggregated to desired frequency.

## Author

Víctor Granda García, CREAF

## Examples

``` r
# \donttest{
# example interpolator
meteoland_interpolator_example
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

# aggregate all dates in the interpolator, calculating the maximum values
summarise_interpolator(meteoland_interpolator_example, fun = "max")
#> stars object with 2 dimensions and 13 attributes
#> attribute(s):
#>                                Min.    1st Qu.    Median       Mean   3rd Qu.
#> Temperature                 4.40000  16.200000  17.50000  16.893443  18.90000
#> MinTemperature              1.90000  10.000000  11.75000  11.290761  12.92500
#> MaxTemperature              8.20000  24.300000  25.80000  24.790761  27.50000
#> Precipitation               6.80000  18.025000  23.80000  25.743548  29.47500
#> RelativeHumidity           84.00000  92.000000  96.00000  95.295082  99.00000
#> Radiation                  22.23314  24.933313  25.69993  25.581177  26.20250
#> WindSpeed                   1.40000   3.200000   3.90000   3.991837   5.10000
#> WindDirection             216.00000 306.000000 340.00000 327.346939 354.00000
#> SmoothedPrecipitation       4.45000   9.876667  11.36000  12.175889  14.76500
#> SmoothedTemperatureRange    5.93750  10.535570  12.40588  12.117069  14.03814
#> elevation                   0.00000 147.000000 317.00000 515.629630 668.00000
#> slope                       0.00000   0.000000   0.00000   0.000000   0.00000
#> aspect                      0.00000   0.000000   0.00000   0.000000   0.00000
#>                                 Max. NA's
#> Temperature                 23.40000    6
#> MinTemperature              20.10000    5
#> MaxTemperature              29.90000    5
#> Precipitation              160.90000    3
#> RelativeHumidity           100.00000    6
#> Radiation                   28.10878    5
#> WindSpeed                    6.90000  140
#> WindDirection              359.00000  140
#> SmoothedPrecipitation       29.30000  129
#> SmoothedTemperatureRange    17.72778    5
#> elevation                 2535.00000    0
#> slope                        0.00000    0
#> aspect                       0.00000    0
#> dimension(s):
#>         from  to  refsys point
#> date       1   1 POSIXct    NA
#> station    1 189  WGS 84  TRUE
#>                                                       values
#> date                                         2022-04-01 CEST
#> station POINT (0.95172 41.6566),...,POINT (1.89716 42.32211)

# aggregate weekly, calculating mean values
summarise_interpolator(meteoland_interpolator_example, frequency = "week")
#> stars object with 2 dimensions and 13 attributes
#> attribute(s):
#>                                 Min.    1st Qu.      Median       Mean
#> Temperature               -13.600000   7.714286  12.1071429  10.581211
#> MinTemperature            -15.666667   2.246429   6.3666667   5.168547
#> MaxTemperature            -10.000000  13.500000  18.3357143  16.610800
#> Precipitation               0.000000   0.000000   0.2666667   1.727146
#> RelativeHumidity           36.666667  57.857143  66.6666667  65.878442
#> Radiation                  12.820177  19.166860  20.6693138  20.719565
#> WindSpeed                   0.600000   1.161905   1.5142857   1.739622
#> WindDirection              43.285714 168.285714 206.8571429 202.485561
#> SmoothedPrecipitation       0.100000   2.810238   5.4536905   6.050388
#> SmoothedTemperatureRange    4.822367   9.903581  11.7322300  11.440517
#> elevation                   0.000000 147.000000 317.0000000 515.629630
#> slope                       0.000000   0.000000   0.0000000   0.000000
#> aspect                      0.000000   0.000000   0.0000000   0.000000
#>                              3rd Qu.       Max. NA's
#> Temperature                14.367857   18.15000   25
#> MinTemperature              8.707143   14.83333   23
#> MaxTemperature             20.571429   25.03333   23
#> Precipitation               2.759524   29.51429   13
#> RelativeHumidity           75.000000   94.85714   25
#> Radiation                  22.198744   27.06309   23
#> WindSpeed                   2.085714    5.70000  694
#> WindDirection             242.000000  350.00000  696
#> SmoothedPrecipitation       8.487679   46.68286  299
#> SmoothedTemperatureRange   13.316941   17.39137   21
#> elevation                 668.000000 2535.00000    0
#> slope                       0.000000    0.00000    0
#> aspect                      0.000000    0.00000    0
#> dimension(s):
#>         from  to          offset  delta  refsys point
#> date       1   5 2022-03-28 CEST 7 days POSIXct    NA
#> station    1 189              NA     NA  WGS 84  TRUE
#>                                                       values
#> date                                                    NULL
#> station POINT (0.95172 41.6566),...,POINT (1.89716 42.32211)

# }
```
