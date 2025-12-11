# Setting interpolation parameters in an interpolator object

Changing or updating interpolation parameters in an interpolator object

## Usage

``` r
set_interpolation_params(
  interpolator,
  params = NULL,
  verbose = getOption("meteoland_verbosity", TRUE)
)
```

## Arguments

- interpolator:

  interpolator object to update

- params:

  list with the parameters provided by the user

- verbose:

  Logical indicating if the function must show messages and info.
  Default value checks `"meteoland_verbosity"` option and if not set,
  defaults to TRUE. It can be turned off for the function with FALSE, or
  session wide with `options(meteoland_verbosity = FALSE)`

## Value

The same interpolator object provided, with the updated interpolation
parameters

## Details

This function ensures that if no parameters are provided, the default
ones are used (see
[`defaultInterpolationParams`](https://emf-creaf.github.io/meteoland/reference/defaultInterpolationParams.md)).
Also, if params are partially provided, this function ensures that the
rest of the parameters are not changed.

## See also

Other interpolator functions:
[`add_topo()`](https://emf-creaf.github.io/meteoland/reference/add_topo.md),
[`create_meteo_interpolator()`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md),
[`get_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/get_interpolation_params.md),
[`read_interpolator()`](https://emf-creaf.github.io/meteoland/reference/read_interpolator.md),
[`with_meteo()`](https://emf-creaf.github.io/meteoland/reference/with_meteo.md),
[`write_interpolator()`](https://emf-creaf.github.io/meteoland/reference/write_interpolator.md)

## Author

Victor Granda García, EMF-CREAF

## Examples

``` r
# example interpolator
data(meteoland_interpolator_example)
# store the actual parameters
old_parameters <- get_interpolation_params(meteoland_interpolator_example)
# we can provide only the parameter we want to change
meteoland_interpolator_example <- set_interpolation_params(
  meteoland_interpolator_example,
  list(debug = TRUE)
)
#> ℹ Some interpolation parameters are missing, using default values for those
# check
get_interpolation_params(meteoland_interpolator_example)$debug
#> [1] TRUE
# compare with old
old_parameters$debug
#> [1] FALSE
# the rest should be the same
setdiff(old_parameters, get_interpolation_params(meteoland_interpolator_example))
#> [[1]]
#> [1] FALSE
#> 
```
