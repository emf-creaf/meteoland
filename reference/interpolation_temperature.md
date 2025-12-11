# Low-level interpolation functions

Low-level functions to interpolate meteorology (one day) on a set of
points.

## Usage

``` r
interpolation_precipitation(
  Xp,
  Yp,
  Zp,
  X,
  Y,
  Z,
  P,
  Psmooth,
  iniRp = 140000,
  alpha_event = 6.25,
  alpha_amount = 6.25,
  N_event = 20L,
  N_amount = 20L,
  iterations = 3L,
  popcrit = 0.5,
  fmax = 0.95,
  debug = FALSE
)

interpolation_dewtemperature(
  Xp,
  Yp,
  Zp,
  X,
  Y,
  Z,
  T,
  iniRp = 140000,
  alpha = 3,
  N = 30L,
  iterations = 3L,
  debug = FALSE
)

interpolation_temperature(
  Xp,
  Yp,
  Zp,
  X,
  Y,
  Z,
  T,
  iniRp = 140000,
  alpha = 3,
  N = 30L,
  iterations = 3L,
  debug = FALSE
)

interpolation_wind(
  Xp,
  Yp,
  WS,
  WD,
  X,
  Y,
  iniRp = 140000,
  alpha = 2,
  N = 1L,
  iterations = 3L,
  directionsAvailable = TRUE
)
```

## Arguments

- Xp, Yp, Zp:

  Spatial coordinates and elevation (Zp; in m.a.s.l) of target points.

- X, Y, Z:

  Spatial coordinates and elevation (Zp; in m.a.s.l) of reference
  locations (e.g. meteorological stations).

- P:

  Precipitation at the reference locations (in mm).

- Psmooth:

  Temporally-smoothed precipitation at the reference locations (in mm).

- iniRp:

  Initial truncation radius.

- iterations:

  Number of station density iterations.

- popcrit:

  Critical precipitation occurrence parameter.

- fmax:

  Maximum value for precipitation regression extrapolations (0.6 equals
  to a maximum of 4 times extrapolation).

- debug:

  Boolean flag to show extra console output.

- T:

  Temperature (e.g., minimum, maximum or dew temperature) at the
  reference locations (in degrees).

- alpha, alpha_amount, alpha_event:

  Gaussian shape parameter.

- N, N_event, N_amount:

  Average number of stations with non-zero weights.

- WS, WD:

  Wind speed (in m/s) and wind direction (in degrees from north
  clock-wise) at the reference locations.

- directionsAvailable:

  A flag to indicate that wind directions are available (i.e.
  non-missing) at the reference locations.

## Value

All functions return a vector with interpolated values for the target
points.

## Details

This functions exposes internal low-level interpolation functions
written in C++ not intended to be used directly in any script or
function. The are maintained for compatibility with older versions of
the package and future versions of meteoland will remove this functions
(they will be still accessible through the triple colon notation
(`:::`), but their use is not recommended)

## Functions

- `interpolation_precipitation()`: Precipitation

- `interpolation_dewtemperature()`: Dew temperature

- `interpolation_wind()`: Wind

## References

Thornton, P.E., Running, S.W., White, M. A., 1997. Generating surfaces
of daily meteorological variables over large regions of complex terrain.
J. Hydrol. 190, 214–251. doi:10.1016/S0022-1694(96)03128-9.

De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018)
Estimating daily meteorological data and downscaling climate models over
landscapes. Environmental Modelling and Software 108: 186-196.

## See also

[`defaultInterpolationParams`](https://emf-creaf.github.io/meteoland/reference/defaultInterpolationParams.md)

## Author

Miquel De Cáceres Ainsa, CREAF

## Examples

``` r
Xp <- as.numeric(sf::st_coordinates(points_to_interpolate_example)[,1])
Yp <- as.numeric(sf::st_coordinates(points_to_interpolate_example)[,2])
Zp <- points_to_interpolate_example$elevation
X <- as.numeric(
  sf::st_coordinates(stars::st_get_dimension_values(meteoland_interpolator_example, "station"))[,1]
)
Y <- as.numeric(
  sf::st_coordinates(stars::st_get_dimension_values(meteoland_interpolator_example, "station"))[,2]
)
Z <- as.numeric(meteoland_interpolator_example[["elevation"]][1,])
Temp <- as.numeric(meteoland_interpolator_example[["MinTemperature"]][1,])
P <- as.numeric(meteoland_interpolator_example[["Precipitation"]][1,])
Psmooth <- as.numeric(meteoland_interpolator_example[["SmoothedPrecipitation"]][1,])
WS <- as.numeric(meteoland_interpolator_example[["WindSpeed"]][1,])
WD <- as.numeric(meteoland_interpolator_example[["WindDirection"]][1,])
iniRp <- get_interpolation_params(meteoland_interpolator_example)$initial_Rp
alpha <- get_interpolation_params(meteoland_interpolator_example)$alpha_MinTemperature
N <- get_interpolation_params(meteoland_interpolator_example)$N_MinTemperature
alpha_event <- get_interpolation_params(meteoland_interpolator_example)$alpha_PrecipitationEvent
N_event <- get_interpolation_params(meteoland_interpolator_example)$N_PrecipitationEvent
alpha_amount <- get_interpolation_params(meteoland_interpolator_example)$alpha_PrecipitationAmount
N_amount <- get_interpolation_params(meteoland_interpolator_example)$N_PrecipitationAmount
alpha_wind <- get_interpolation_params(meteoland_interpolator_example)$alpha_Wind
N_wind <- get_interpolation_params(meteoland_interpolator_example)$N_Wind
iterations <- get_interpolation_params(meteoland_interpolator_example)$iterations
popcrit <- get_interpolation_params(meteoland_interpolator_example)$pop_crit
fmax <- get_interpolation_params(meteoland_interpolator_example)$f_max
debug <- get_interpolation_params(meteoland_interpolator_example)$debug

interpolation_temperature(
  Xp, Yp, Zp,
  X[!is.na(Temp)], Y[!is.na(Temp)], Z[!is.na(Temp)],
  Temp[!is.na(Temp)],
  iniRp, alpha, N, iterations, debug
)
#>  [1] -2.5259554 -1.2274925  0.4602637 -0.1155003 -9.7153282  2.5290313
#>  [7] -2.4239096 -2.1036076 -1.9277582 -0.8422971 -1.4108765 -2.2973953
#> [13] -0.7649598 -3.5780859  0.2170589

interpolation_wind(
  Xp, Yp,
  WS[!is.na(WD)], WD[!is.na(WD)],
  X[!is.na(WD)], Y[!is.na(WD)],
  iniRp, alpha_wind, N_wind, iterations, directionsAvailable = FALSE
)
#>           [,1] [,2]
#>  [1,] 1.980286   NA
#>  [2,] 1.995742   NA
#>  [3,] 0.900000   NA
#>  [4,] 1.613360   NA
#>  [5,] 1.964632   NA
#>  [6,] 6.196058   NA
#>  [7,]       NA   NA
#>  [8,] 2.058176   NA
#>  [9,] 3.356163   NA
#> [10,] 3.221483   NA
#> [11,]       NA   NA
#> [12,] 4.370816   NA
#> [13,] 4.132147   NA
#> [14,]       NA   NA
#> [15,] 3.342795   NA

interpolation_precipitation(
  Xp, Yp, Zp,
  X[!is.na(P)], Y[!is.na(P)], Z[!is.na(P)],
  P[!is.na(P)], Psmooth[!is.na(P)],
  iniRp, alpha_event, alpha_amount, N_event, N_amount,
  iterations, popcrit, fmax, debug
)
#>  [1] 0.0000000 0.0000000 0.1049001 0.1724466 1.8571120 0.1968337 2.0475055
#>  [8] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 2.8098406
#> [15] 0.0000000
```
