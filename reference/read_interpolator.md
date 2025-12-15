# Read interpolator files

Read interpolator files created with
[`write_interpolator`](https://emf-creaf.github.io/meteoland/reference/write_interpolator.md)

## Usage

``` r
read_interpolator(filename)
```

## Arguments

- filename:

  interpolator file name

## Value

an interpolator (stars) object

## Details

This function takes the file name of the nc file storing an interpolator
object and load it into the work environment

## See also

Other interpolator functions:
[`add_topo()`](https://emf-creaf.github.io/meteoland/reference/add_topo.md),
[`create_meteo_interpolator()`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md),
[`get_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/get_interpolation_params.md),
[`set_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/set_interpolation_params.md),
[`with_meteo()`](https://emf-creaf.github.io/meteoland/reference/with_meteo.md),
[`write_interpolator()`](https://emf-creaf.github.io/meteoland/reference/write_interpolator.md)

## Author

Victor Granda García, EMF-CREAF

## Examples

``` r
# \donttest{
# example interpolator
data(meteoland_interpolator_example)

# temporal folder
tmp_dir <- tempdir()

# write interpolator
write_interpolator(
  meteoland_interpolator_example,
  file.path(tmp_dir, "meteoland_interpolator_example.nc")
)
#> ℹ Creating nc file following the NetCDF-CF conventions <https://cfconventions.org/cf-conventions/cf-conventions.html>
#> ℹ Adding spatial info to nc file
#> ✔ Done

# check file exists
file.exists(file.path(tmp_dir, "meteoland_interpolator_example.nc"))
#> [1] TRUE

# read it again
read_interpolator(file.path(tmp_dir, "meteoland_interpolator_example.nc"))
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
# }
```
