# Interpolation process for spatial data

Interpolate spatial data to obtain downscaled meteorologic variables

## Usage

``` r
interpolate_data(
  spatial_data,
  interpolator,
  dates = NULL,
  variables = NULL,
  ignore_convex_hull_check = FALSE,
  verbose = getOption("meteoland_verbosity", TRUE)
)
```

## Arguments

- spatial_data:

  An sf or stars raster object to interpolate

- interpolator:

  A meteoland interpolator object, as created by
  [`create_meteo_interpolator`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md)

- dates:

  vector with dates to interpolate (must be within the interpolator date
  range). Default to NULL (all dates present in the interpolator object)

- variables:

  vector with variable names to be interpolated. NULL (default), will
  interpolate all variables. Accepted names are "Temperature",
  "Precipitation", "RelativeHumidity", "Radiation" and "Wind"

- ignore_convex_hull_check:

  Logical indicating whether errors in convex hull checks should be
  ignored. Checking for points to be inside the convex hull will
  normally raise an error if \>10% of points are outside. Setting
  `ignore_convex_hull_check = TRUE` means that a warning is raised but
  interpolation is performed, which can be useful to users interpolating
  on a few points close but outside of the convex hull.

- verbose:

  Logical indicating if the function must show messages and info.
  Default value checks `"meteoland_verbosity"` option and if not set,
  defaults to TRUE. It can be turned off for the function with FALSE, or
  session wide with `options(meteoland_verbosity = FALSE)`

## Value

an object with the same class and structure as the provided spatial data
with the results of the interpolation joined. In the case of spatial
data being an sf, the results are added as a list-type column that can
be unnested with
[`unnest`](https://tidyr.tidyverse.org/reference/unnest.html). In the
case of a stars raster object, interpolation results are added as
attributes (variables)

## Details

This function takes a spatial data object (sf or stars raster), an
interpolator object
([`create_meteo_interpolator`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md))
and a vector of dates to perform the interpolation of the meteorologic
variables for the spatial locations present in the `spatial_data`
object.

## Spatial data

The spatial data provided must be of two types. (I) A sf object
containing POINT for each location to interpolate or (II) a stars raster
object for which the interpolation should be done. Independently of the
class of `spatial_data` it has to have some mandatory variables, namely
`elevation`. It should also contain `aspect` and `slope` for a better
interpolation process, though this two variables are not mandatory.

## Author

Victor Granda García, EMF-CREAF

Miquel De Cáceres Ainsa, EMF-CREAF

## Examples

``` r
# \donttest{
# example of data to interpolate and example interpolator
data("points_to_interpolate_example")
data("meteoland_interpolator_example")

# interpolate data
res <- interpolate_data(points_to_interpolate_example, meteoland_interpolator_example)
#> ℹ Starting interpolation...
#> ℹ Temperature interpolation is needed also...
#> • Interpolating temperature...
#> ℹ Precipitation interpolation is needed also...
#> • Interpolating precipitation...
#> ℹ Relative humidity interpolation is needed also...
#> • Interpolating relative humidity...
#> ℹ Radiation calculation is needed also...
#> • Calculating radiation...
#> ℹ Wind interpolation is needed also...
#> • Interpolating wind...
#> • Calculating PET...
#> ✔ Interpolation done...

# check result
# same class as input data
class(res)
#> [1] "sf"         "tbl_df"     "tbl"        "data.frame"
# data
res
#> Simple feature collection with 15 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.7578958 ymin: 41.31541 xmax: 2.98657 ymax: 42.6336
#> Geodetic CRS:  WGS 84
#> # A tibble: 15 × 6
#>    plot_id elevation slope aspect             geometry interpolated_data 
#>    <chr>       <dbl> <dbl>  <dbl>          <POINT [°]> <list>            
#>  1 P_05284      889. 25.2   313.   (2.320167 42.24139) <tibble [30 × 13]>
#>  2 P_06572      680. 18.0    79.9  (2.552233 42.02596) <tibble [30 × 13]>
#>  3 P_07201      141.  4.17   52.9  (2.721874 41.88258) <tibble [30 × 13]>
#>  4 P_07512      254. 14.3   261.     (2.98657 41.9006) <tibble [30 × 13]>
#>  5 P_08207     1860. 36.4   293.   (2.209903 42.33968) <tibble [30 × 13]>
#>  6 P_08299      183.  4.12   92.9  (2.817143 42.24325) <tibble [30 × 13]>
#>  7 P_09341      819  23.4   128.   (1.126766 42.42612) <tibble [30 × 13]>
#>  8 P_10272      860  34.8   347.   (1.398528 42.26791) <tibble [30 × 13]>
#>  9 P_10861      706  22.4    22.6 (0.9314126 42.04226) <tibble [30 × 13]>
#> 10 P_11651      585  22.0   199.   (0.7578958 41.8612) <tibble [30 × 13]>
#> 11 P_12150      674. 30.3   154.   (1.481719 41.81838) <tibble [30 × 13]>
#> 12 P_12227      752.  6.04   27.7    (1.283161 41.591) <tibble [30 × 13]>
#> 13 P_12417      702  11.6    63.1 (0.8727224 41.35875) <tibble [30 × 13]>
#> 14 P_13007      972.  4.21  338.    (1.120383 42.6336) <tibble [30 × 13]>
#> 15 P_14029      556. 14.1    41.4  (1.480716 41.31541) <tibble [30 × 13]>
# results for the first location
res[["interpolated_data"]][1]
#> [[1]]
#> # A tibble: 30 × 13
#>    dates                 DOY MeanTemperature MinTemperature MaxTemperature
#>    <dttm>              <dbl>           <dbl>          <dbl>          <dbl>
#>  1 2022-04-01 00:00:00    91            3.24          -2.53           6.99
#>  2 2022-04-02 00:00:00    92            2.99          -4.20           7.67
#>  3 2022-04-03 00:00:00    93            2.39          -5.61           7.59
#>  4 2022-04-04 00:00:00    94            4.02          -3.93           9.20
#>  5 2022-04-05 00:00:00    95            7.38          -3.29          14.3 
#>  6 2022-04-06 00:00:00    96            8.78           1.31          13.6 
#>  7 2022-04-07 00:00:00    97           12.1            2.44          18.4 
#>  8 2022-04-08 00:00:00    98           12.9            3.32          19.1 
#>  9 2022-04-09 00:00:00    99           10.0            3.83          14.1 
#> 10 2022-04-10 00:00:00   100           11.4            4.85          15.6 
#> # ℹ 20 more rows
#> # ℹ 8 more variables: Precipitation <dbl>, MeanRelativeHumidity <dbl>,
#> #   MinRelativeHumidity <dbl>, MaxRelativeHumidity <dbl>, Radiation <dbl>,
#> #   WindSpeed <dbl>, WindDirection <dbl>, PET <dbl>
#> 
# unnest results
tidyr::unnest(res, cols = "interpolated_data")
#> Simple feature collection with 450 features and 17 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.7578958 ymin: 41.31541 xmax: 2.98657 ymax: 42.6336
#> Geodetic CRS:  WGS 84
#> # A tibble: 450 × 18
#>    plot_id elevation slope aspect            geometry dates              
#>    <chr>       <dbl> <dbl>  <dbl>         <POINT [°]> <dttm>             
#>  1 P_05284      889.  25.2   313. (2.320167 42.24139) 2022-04-01 00:00:00
#>  2 P_05284      889.  25.2   313. (2.320167 42.24139) 2022-04-02 00:00:00
#>  3 P_05284      889.  25.2   313. (2.320167 42.24139) 2022-04-03 00:00:00
#>  4 P_05284      889.  25.2   313. (2.320167 42.24139) 2022-04-04 00:00:00
#>  5 P_05284      889.  25.2   313. (2.320167 42.24139) 2022-04-05 00:00:00
#>  6 P_05284      889.  25.2   313. (2.320167 42.24139) 2022-04-06 00:00:00
#>  7 P_05284      889.  25.2   313. (2.320167 42.24139) 2022-04-07 00:00:00
#>  8 P_05284      889.  25.2   313. (2.320167 42.24139) 2022-04-08 00:00:00
#>  9 P_05284      889.  25.2   313. (2.320167 42.24139) 2022-04-09 00:00:00
#> 10 P_05284      889.  25.2   313. (2.320167 42.24139) 2022-04-10 00:00:00
#> # ℹ 440 more rows
#> # ℹ 12 more variables: DOY <dbl>, MeanTemperature <dbl>, MinTemperature <dbl>,
#> #   MaxTemperature <dbl>, Precipitation <dbl>, MeanRelativeHumidity <dbl>,
#> #   MinRelativeHumidity <dbl>, MaxRelativeHumidity <dbl>, Radiation <dbl>,
#> #   WindSpeed <dbl>, WindDirection <dbl>, PET <dbl>
# }
```
