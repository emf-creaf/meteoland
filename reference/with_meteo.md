# Ensure meteo object is ready to create an interpolator object

Check integrity of meteo objects

## Usage

``` r
with_meteo(meteo, verbose = getOption("meteoland_verbosity", TRUE))
```

## Arguments

- meteo:

  meteo object

- verbose:

  Logical indicating if the function must show messages and info.
  Default value checks `"meteoland_verbosity"` option and if not set,
  defaults to TRUE. It can be turned off for the function with FALSE, or
  session wide with `options(meteoland_verbosity = FALSE)`

## Value

invisible meteo object ready to pipe in the interpolator creation

## Details

This function is the first step in the creation of a meteoland
interpolator, ensuring the meteo provided contains all the required
elements

## See also

Other interpolator functions:
[`add_topo()`](https://emf-creaf.github.io/meteoland/reference/add_topo.md),
[`create_meteo_interpolator()`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md),
[`get_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/get_interpolation_params.md),
[`read_interpolator()`](https://emf-creaf.github.io/meteoland/reference/read_interpolator.md),
[`set_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/set_interpolation_params.md),
[`write_interpolator()`](https://emf-creaf.github.io/meteoland/reference/write_interpolator.md)

## Examples

``` r
# example meteo
data(meteoland_meteo_example)
with_meteo(meteoland_meteo_example)
#> ℹ Checking meteorology object...
#> ✔ meteorology object ok
```
