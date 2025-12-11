# Retrieving interpolation parameters from interpolator object

Retrieve the parameter list from and interpolator object

## Usage

``` r
get_interpolation_params(interpolator)
```

## Arguments

- interpolator:

  interpolator object as returned by
  [`create_meteo_interpolator`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md)

## Value

The complete parameter list from the interpolator object

## See also

Other interpolator functions:
[`add_topo()`](https://emf-creaf.github.io/meteoland/reference/add_topo.md),
[`create_meteo_interpolator()`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md),
[`read_interpolator()`](https://emf-creaf.github.io/meteoland/reference/read_interpolator.md),
[`set_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/set_interpolation_params.md),
[`with_meteo()`](https://emf-creaf.github.io/meteoland/reference/with_meteo.md),
[`write_interpolator()`](https://emf-creaf.github.io/meteoland/reference/write_interpolator.md)

## Author

Victor Granda García, EMF-CREAF

## Examples

``` r
# example interpolator
data(meteoland_interpolator_example)
# get the params from the interpolator
get_interpolation_params(meteoland_interpolator_example)
#> $initial_Rp
#> [1] 1.136444
#> 
#> $iterations
#> [1] 3
#> 
#> $alpha_MinTemperature
#> [1] 3
#> 
#> $alpha_MaxTemperature
#> [1] 3
#> 
#> $alpha_DewTemperature
#> [1] 3
#> 
#> $alpha_PrecipitationEvent
#> [1] 5
#> 
#> $alpha_PrecipitationAmount
#> [1] 5
#> 
#> $alpha_Wind
#> [1] 3
#> 
#> $N_MinTemperature
#> [1] 30
#> 
#> $N_MaxTemperature
#> [1] 30
#> 
#> $N_DewTemperature
#> [1] 30
#> 
#> $N_PrecipitationEvent
#> [1] 5
#> 
#> $N_PrecipitationAmount
#> [1] 20
#> 
#> $N_Wind
#> [1] 2
#> 
#> $St_Precipitation
#> [1] 5
#> 
#> $St_TemperatureRange
#> [1] 15
#> 
#> $pop_crit
#> [1] 0.5
#> 
#> $f_max
#> [1] 0.6
#> 
#> $wind_height
#> [1] 10
#> 
#> $wind_roughness_height
#> [1] 0.001
#> 
#> $penman_albedo
#> [1] 0.25
#> 
#> $penman_windfun
#> [1] "1956"
#> 
#> $debug
#> [1] FALSE
#> 
```
