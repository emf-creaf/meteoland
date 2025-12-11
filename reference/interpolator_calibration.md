# Calibration and validation of interpolation procedures

Calibration and validation of interpolation procedures

## Usage

``` r
interpolation_cross_validation(
  interpolator,
  stations = NULL,
  verbose = getOption("meteoland_verbosity", TRUE)
)

interpolator_calibration(
  interpolator,
  stations = NULL,
  update_interpolation_params = FALSE,
  variable = "MinTemperature",
  N_seq = seq(10, 30, by = 5),
  alpha_seq = seq(0.25, 10, by = 0.25),
  verbose = getOption("meteoland_verbosity", TRUE)
)
```

## Arguments

- interpolator:

  A meteoland interpolator object, as created by
  [`create_meteo_interpolator`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md)

- stations:

  A vector with the stations (numeric for station indexes or character
  for stations id) to be used to calculate `"MAE"`. All stations with
  data are included in the training set but predictive `"MAE"` are
  calculated for the stations subset indicated in `stations` param only.
  If `NULL` all stations are used in the predictive `"MAE"` calculation.

- verbose:

  Logical indicating if the function must show messages and info.
  Default value checks `"meteoland_verbosity"` option and if not set,
  defaults to TRUE. It can be turned off for the function with FALSE, or
  session wide with `options(meteoland_verbosity = FALSE)`

- update_interpolation_params:

  Logical indicating if the interpolator object must be updated with the
  calculated parameters. Default to FALSE

- variable:

  A string indicating the meteorological variable for which
  interpolation parameters `"N"` and `"alpha"` will be calibrated.
  Accepted values are:

  - `MinTemperature` (kernel for minimum temperature)

  - `MaxTemperature` (kernel for maximum temperature)

  - `DewTemperature` (kernel for dew-temperature (i.e. relative
    humidity))

  - `Precipitation` (to calibrate the same kernel for both precipitation
    events and regression of precipitation amounts; not recommended)

  - `PrecipitationAmount` (kernel for regression of precipitation
    amounts)

  - `PrecipitationEvent` (kernel for precipitation events)

- N_seq:

  Numeric vector with `"N"` values to be tested

- alpha_seq:

  Numeric vector with `"alpha"`

## Value

`interpolation_cross_validation` returns a list with the following items

- errors: Data frame with each combination of station and date with
  observed variables, predicated variables and the total error
  (predicted - observed) calculated for each variable

- station_stats: Data frame with error and bias statistics aggregated by
  station

- dates_stats: Data frame with error and bias statistics aggregated by
  date

- r2: correlation indexes between observed and predicted values for each
  meteorological variable

If `update_interpolation_params` is FALSE (default),
`interpolator_calibration` returns a list with the following items

- MAE: A numeric matrix with the mean absolute error values, averaged
  across stations, for each combination of parameters `"N"` and
  `"alpha"`

- minMAE: Minimum MAE value

- N: Value of parameter `"N"` corresponding to the minimum MAE

- alpha: Value of parameter `"alpha"` corresponding the the minimum MAE

- observed: matrix with observed values (meteorological measured values)

- predicted: matrix with interpolated values for the optimum parameter
  combination

If `update_interpolation_params` is FALSE, `interpolator_calibration`
returns the interpolator provided with the parameters updated

## Details

Function `interpolator_calibration` determines optimal interpolation
parameters `"N"` and `"alpha"` for a given meteorological variable.
Optimization is done by minimizing mean absolute error ("MAE") (Thornton
*et al.* 1997). Function `interpolation_cross_validation` calculates
average mean absolute errors ("MAE") for the prediction period of the
interpolator object. In both calibration and cross validation
procedures, predictions for each meteorological station are made using a
*leave-one-out* procedure (i.e. after excluding the station from the
predictive set).

## Functions

- `interpolation_cross_validation()`:

## References

Thornton, P.E., Running, S.W., 1999. An improved algorithm for
estimating incident daily solar radiation from measurements of
temperature, humidity, and precipitation. Agric. For. Meteorol. 93,
211–228. doi:10.1016/S0168-1923(98)00126-9.

De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018)
Estimating daily meteorological data and downscaling climate models over
landscapes. Environmental Modelling and Software 108: 186-196.

## Author

Miquel De Cáceres Ainsa, EMF-CREAF

Victor Granda García, EMF-CREAF

## Examples

``` r
# \donttest{
# example interpolator
data("meteoland_interpolator_example")

# As the cross validation for all stations can be time consuming, we are
# gonna use only for the first 5 stations of the 198
cv <- interpolation_cross_validation(meteoland_interpolator_example, stations = 1:5)
#> ℹ Starting Cross Validation process...
#> • Interpolating stations...
#> • Calculating R squared...
#> • calculating errors, MAE and bias for interpolated variables...
#> ✔ Cross validation done.

# Inspect the results
cv$errors
#> # A tibble: 150 × 21
#>    dates               station stationID MinTemperature_error
#>    <dttm>                <int> <chr>                    <dbl>
#>  1 2022-04-01 00:00:00       1 C6                      0.120 
#>  2 2022-04-02 00:00:00       1 C6                     -0.296 
#>  3 2022-04-03 00:00:00       1 C6                      0.178 
#>  4 2022-04-04 00:00:00       1 C6                     -0.164 
#>  5 2022-04-05 00:00:00       1 C6                     -0.0108
#>  6 2022-04-06 00:00:00       1 C6                     -0.287 
#>  7 2022-04-07 00:00:00       1 C6                     -0.773 
#>  8 2022-04-08 00:00:00       1 C6                      1.22  
#>  9 2022-04-09 00:00:00       1 C6                     -0.599 
#> 10 2022-04-10 00:00:00       1 C6                      1.08  
#> # ℹ 140 more rows
#> # ℹ 17 more variables: MaxTemperature_error <dbl>,
#> #   RangeTemperature_error <dbl>, RelativeHumidity_error <dbl>,
#> #   Radiation_error <dbl>, Precipitation_error <dbl>,
#> #   MinTemperature_predicted <dbl>, MaxTemperature_predicted <dbl>,
#> #   RangeTemperature_predicted <dbl>, RelativeHumidity_predicted <dbl>,
#> #   Radiation_predicted <dbl>, Precipitation_predicted <dbl>, …
cv$station_stats
#> # A tibble: 5 × 20
#>   station stationID MinTemperature_station_bias MaxTemperature_station_bias
#>     <int> <chr>                           <dbl>                       <dbl>
#> 1       1 C6                            -0.0601                     -0.489 
#> 2       2 C7                            -1.37                       -0.644 
#> 3       3 C8                            -0.224                       0.0301
#> 4       4 C9                            -0.257                       0.519 
#> 5       5 CC                             0.178                      -0.808 
#> # ℹ 16 more variables: RangeTemperature_station_bias <dbl>,
#> #   RelativeHumidity_station_bias <dbl>, Radiation_station_bias <dbl>,
#> #   MinTemperature_station_mae <dbl>, MaxTemperature_station_mae <dbl>,
#> #   RangeTemperature_station_mae <dbl>, RelativeHumidity_station_mae <dbl>,
#> #   Radiation_station_mae <dbl>, TotalPrecipitation_station_observed <dbl>,
#> #   TotalPrecipitation_station_predicted <dbl>,
#> #   TotalPrecipitation_station_bias <dbl>, …
cv$dates_stats
#> # A tibble: 30 × 19
#>    dates               MinTemperature_date_bias MaxTemperature_date_bias
#>    <dttm>                                 <dbl>                    <dbl>
#>  1 2022-04-01 00:00:00                  -0.0883                  -0.174 
#>  2 2022-04-02 00:00:00                  -0.434                   -0.226 
#>  3 2022-04-03 00:00:00                  -0.392                   -0.477 
#>  4 2022-04-04 00:00:00                  -0.550                   -0.197 
#>  5 2022-04-05 00:00:00                  -0.172                   -0.365 
#>  6 2022-04-06 00:00:00                  -0.345                    0.0511
#>  7 2022-04-07 00:00:00                  -1.09                    -0.188 
#>  8 2022-04-08 00:00:00                  -0.730                   -0.236 
#>  9 2022-04-09 00:00:00                  -0.576                   -0.422 
#> 10 2022-04-10 00:00:00                  -0.568                    0.0402
#> # ℹ 20 more rows
#> # ℹ 16 more variables: RangeTemperature_date_bias <dbl>,
#> #   RelativeHumidity_date_bias <dbl>, Radiation_date_bias <dbl>,
#> #   MinTemperature_date_mae <dbl>, MaxTemperature_date_mae <dbl>,
#> #   RangeTemperature_date_mae <dbl>, RelativeHumidity_date_mae <dbl>,
#> #   Radiation_date_mae <dbl>, TotalPrecipitation_date_observed <dbl>,
#> #   TotalPrecipitation_date_predicted <dbl>, …
cv$r2
#> $MinTemperature
#> [1] 0.9720093
#> 
#> $MaxTemperature
#> [1] 0.9845019
#> 
#> $RangeTemperature
#> [1] 0.9557687
#> 
#> $RelativeHumidity
#> [1] 0.9591181
#> 
#> $Radiation
#> [1] 0.8943052
#> 
# }


# \donttest{
# example interpolator
data("meteoland_interpolator_example")

# As the calibration for all stations can be time consuming, we are gonna
# interpolate only for the first 5 stations of the 198 and only a handful
# of parameter combinations
calibration <- interpolator_calibration(
  meteoland_interpolator_example,
  stations = 1:5,
  variable = "MaxTemperature",
  N_seq = seq(10, 20, by = 5),
  alpha_seq = seq(8, 9, by = 0.25)
)
#> ℹ Total number of stations: 189
#> ℹ Number of stations with available data: 185
#> ℹ Number of stations used for MAE calculation: 5
#> ℹ Number of parameters combinations to test: 15
#> ℹ Starting evaluation of parameter combinations for "MaxTemperature"...
#> • Evaluating N: 10, alpha: 8...
#> • Evaluating N: 10, alpha: 8.25...
#> • Evaluating N: 10, alpha: 8.5...
#> • Evaluating N: 10, alpha: 8.75...
#> • Evaluating N: 10, alpha: 9...
#> • Evaluating N: 15, alpha: 8...
#> • Evaluating N: 15, alpha: 8.25...
#> • Evaluating N: 15, alpha: 8.5...
#> • Evaluating N: 15, alpha: 8.75...
#> • Evaluating N: 15, alpha: 9...
#> • Evaluating N: 20, alpha: 8...
#> • Evaluating N: 20, alpha: 8.25...
#> • Evaluating N: 20, alpha: 8.5...
#> • Evaluating N: 20, alpha: 8.75...
#> • Evaluating N: 20, alpha: 9...
#> ✔ Calibration done: Minimum MAE: 0.558727762077721; N: 10; alpha: 9

# we can update the interpolator params directly:
updated_interpolator <- interpolator_calibration(
  meteoland_interpolator_example,
  stations = 1:5,
  update_interpolation_params = TRUE,
  variable = "MaxTemperature",
  N_seq = seq(10, 20, by = 5),
  alpha_seq = seq(8, 9, by = 0.25)
)
#> ℹ Total number of stations: 189
#> ℹ Number of stations with available data: 185
#> ℹ Number of stations used for MAE calculation: 5
#> ℹ Number of parameters combinations to test: 15
#> ℹ Starting evaluation of parameter combinations for "MaxTemperature"...
#> • Evaluating N: 10, alpha: 8...
#> • Evaluating N: 10, alpha: 8.25...
#> • Evaluating N: 10, alpha: 8.5...
#> • Evaluating N: 10, alpha: 8.75...
#> • Evaluating N: 10, alpha: 9...
#> • Evaluating N: 15, alpha: 8...
#> • Evaluating N: 15, alpha: 8.25...
#> • Evaluating N: 15, alpha: 8.5...
#> • Evaluating N: 15, alpha: 8.75...
#> • Evaluating N: 15, alpha: 9...
#> • Evaluating N: 20, alpha: 8...
#> • Evaluating N: 20, alpha: 8.25...
#> • Evaluating N: 20, alpha: 8.5...
#> • Evaluating N: 20, alpha: 8.75...
#> • Evaluating N: 20, alpha: 9...
#> ✔ Calibration done: Minimum MAE: 0.558727762077721; N: 10; alpha: 9


# check the new interpolator have the parameters updated
get_interpolation_params(updated_interpolator)$N_MaxTemperature
#> [1] 10
get_interpolation_params(updated_interpolator)$alpha_MaxTemperature
#> [1] 9
# }
```
