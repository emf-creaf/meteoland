# Add topography data to meteo object

Add topography data to meteo object

## Usage

``` r
add_topo(meteo, topo, verbose = getOption("meteoland_verbosity", TRUE))
```

## Arguments

- meteo:

  meteo object

- topo:

  topo object

- verbose:

  Logical indicating if the function must show messages and info.
  Default value checks `"meteoland_verbosity"` option and if not set,
  defaults to TRUE. It can be turned off for the function with FALSE, or
  session wide with `options(meteoland_verbosity = FALSE)`

## Value

meteo with the topography info added

## Details

When using meteo data without topography info to create an interpolator,
topography must be added

## See also

Other interpolator functions:
[`create_meteo_interpolator()`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md),
[`get_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/get_interpolation_params.md),
[`read_interpolator()`](https://emf-creaf.github.io/meteoland/reference/read_interpolator.md),
[`set_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/set_interpolation_params.md),
[`with_meteo()`](https://emf-creaf.github.io/meteoland/reference/with_meteo.md),
[`write_interpolator()`](https://emf-creaf.github.io/meteoland/reference/write_interpolator.md)

## Examples

``` r
# example meteo
data(meteoland_meteo_no_topo_example)
# example topo
data(meteoland_topo_example)
# add topo
with_meteo(meteoland_meteo_no_topo_example) |>
  add_topo(meteoland_topo_example)
#> ℹ Checking meteorology object...
#> ✔ meteorology object ok
#> ℹ Checking topography object...
#> ✔ topography object ok
#> ℹ Adding topography to meteo (by station ID)...
#> ✔ Topography added
#> Simple feature collection with 5652 features and 16 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.30565 ymin: 40.55786 xmax: 3.18165 ymax: 42.77011
#> Geodetic CRS:  WGS 84
#> # A tibble: 5,652 × 17
#>    dates               service  stationID station_name          station_province
#>    <dttm>              <chr>    <chr>     <chr>                 <chr>           
#>  1 2022-04-01 00:00:00 meteocat C6        Castellnou de Seana   Lleida          
#>  2 2022-04-01 00:00:00 meteocat C7        Tàrrega               Lleida          
#>  3 2022-04-01 00:00:00 meteocat C8        Cervera               Lleida          
#>  4 2022-04-01 00:00:00 meteocat C9        Mas de Barberans      Tarragona       
#>  5 2022-04-01 00:00:00 meteocat CC        Orís                  Barcelona       
#>  6 2022-04-01 00:00:00 meteocat CD        la Seu d'Urgell - Be… Lleida          
#>  7 2022-04-01 00:00:00 meteocat CE        els Hostalets de Pie… Barcelona       
#>  8 2022-04-01 00:00:00 meteocat CG        Molló - Fabert        Girona          
#>  9 2022-04-01 00:00:00 meteocat CI        Sant Pau de Segúries  Girona          
#> 10 2022-04-01 00:00:00 meteocat CJ        Organyà               Lleida          
#> # ℹ 5,642 more rows
#> # ℹ 12 more variables: MeanTemperature <dbl>, MinTemperature <dbl>,
#> #   MaxTemperature <dbl>, MeanRelativeHumidity <dbl>,
#> #   MinRelativeHumidity <dbl>, MaxRelativeHumidity <dbl>, Precipitation <dbl>,
#> #   WindDirection <dbl>, WindSpeed <dbl>, Radiation <dbl>, geom <POINT [°]>,
#> #   elevation <dbl>
```
