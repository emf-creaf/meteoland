# Tidy meteoland

``` r
library(meteoland)
#> Package 'meteoland' [ver. 2.2.5]
library(stars)
#> Loading required package: abind
#> Loading required package: sf
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

### A new way of working with `meteoland`

With the [retirement](https://r-spatial.org/r/2022/04/12/evolution.html)
of `rgdal`, `rgeos` and `maptools` R packages, a complete update of
`meteoland` was necessary to remove the hard dependency `meteoland` has
with `sp` and `raster` R packages. Starting with version 2.0.0 of
`meteoland`, any hard dependency on retired packages as well as `sp` and
`raster` has been removed, and now `sf` and `stars` packages are
internally used for working with simple features and raster data.

By June 2023, `sp` and `raster` packages will be completely removed from
the dependency list.

As fundamental changes were required (`meteoland` meteorology classes
were based on `sp` ones), a decision to improve and make meteorological
interpolation process simpler was taken. In this vignette, we provide
insights on the new ways of working with `meteoland`.

If you are interested in the equivalences between older and newer
functions of `meteoland`, please see the [appendix](#appendix) at the
end of the vignette.

### Example datasets

`meteoland` now ships with new example objects, based on the `sf` and
`stars` packages. The following table describes the new data examples:

| Name | Description |
|----|----|
| meteoland_interpolator_example | A `meteoland` interpolator object, with daily meteorological data in Catalonia (Spain) for April 2022. |
| meteoland_meteo_example | Data from meteorological stations in Catalonia (Spain) for April 2022. |
| meteoland_meteo_no_topo_example | Data from meteorological stations in Catalonia (Spain) for April 2022, without topographical information. |
| meteoland_topo_example | Topographical information for meteorological stations in Catalonia (Spain). |
| points_to_interpolate_example | Topographical information for 15 plots located in Catalonia (Spain). |
| raster_to_interpolate_example | Topographical information for a 0.01 degree grid (10x10 cells) raster located in central Catalonia (Spain) |

### The new interpolation process

The interpolation of meteorological information requires two kinds of
information:

1.  The **topographical information** of the target locations to
    interpolate. This includes elevation, aspect and slope. Elevation is
    the only mandatory topography variable, but interpolation results
    improve when also aspect and slope are provided in mountain areas.

2.  The reference **meteorological information** that we will use to
    build the interpolator object. This information can come from
    meteorological stations in the area we are interested on or,
    alternatively, can be extracted from available rasters with
    meteorological variables.

#### Topographical information

In order to interpolate, we need our locations in a format that
`meteoland` can understand. For point locations, this is a `sf` object
including the `elevation` (in m.a.s.l.), `slope` (in degrees) and
`aspect` (in degrees) variables, such as:

``` r
points_to_interpolate_example
#> Simple feature collection with 15 features and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.7578958 ymin: 41.31541 xmax: 2.98657 ymax: 42.6336
#> Geodetic CRS:  WGS 84
#> # A tibble: 15 × 5
#>    plot_id elevation slope aspect             geometry
#>    <chr>       <dbl> <dbl>  <dbl>          <POINT [°]>
#>  1 P_05284      889. 25.2   313.   (2.320167 42.24139)
#>  2 P_06572      680. 18.0    79.9  (2.552233 42.02596)
#>  3 P_07201      141.  4.17   52.9  (2.721874 41.88258)
#>  4 P_07512      254. 14.3   261.     (2.98657 41.9006)
#>  5 P_08207     1860. 36.4   293.   (2.209903 42.33968)
#>  6 P_08299      183.  4.12   92.9  (2.817143 42.24325)
#>  7 P_09341      819  23.4   128.   (1.126766 42.42612)
#>  8 P_10272      860  34.8   347.   (1.398528 42.26791)
#>  9 P_10861      706  22.4    22.6 (0.9314126 42.04226)
#> 10 P_11651      585  22.0   199.   (0.7578958 41.8612)
#> 11 P_12150      674. 30.3   154.   (1.481719 41.81838)
#> 12 P_12227      752.  6.04   27.7    (1.283161 41.591)
#> 13 P_12417      702  11.6    63.1 (0.8727224 41.35875)
#> 14 P_13007      972.  4.21  338.    (1.120383 42.6336)
#> 15 P_14029      556. 14.1    41.4  (1.480716 41.31541)
```

For spatially-continuous data (i.e. a raster), we need a `stars` object
including `elevation` (in m.a.s.l.), `slope` (in degrees) and `aspect`
(in degrees) as attributes, such as:

``` r
raster_to_interpolate_example
#> stars object with 2 dimensions and 3 attributes
#> attribute(s):
#>                  Min.    1st Qu.    Median      Mean   3rd Qu.     Max.
#> elevation  240.000000 370.000000 447.00000 460.32231 525.00000 786.0000
#> slope        1.432096   5.720433  11.34812  13.07343  19.75851  31.0719
#> aspect       5.194427  74.744881 174.36932 181.67923 291.03751 360.0000
#> dimension(s):
#>   from  to  offset    delta refsys x/y
#> x  155 165 0.03648  0.01058 WGS 84 [x]
#> y  110 120   42.92 -0.01058 WGS 84 [y]
```

Both, `sf` and `stars` R packages have the necessary functions to read
most spatial formats, the only thing to consider is ensuring that the
topographical variables are included, have the proper units and
mandatory names (`elevation`, `aspect`, `slope`).

#### Meteorological information

For interpolating the meteorological variables in our locations (see
above), we need a reference meteorological data. This is a `sf` object
with the reference locations, and daily values of the weather variables
needed to perform the interpolation, for example:

``` r
meteoland_meteo_example
#> Simple feature collection with 5652 features and 18 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.30565 ymin: 40.55786 xmax: 3.18165 ymax: 42.77011
#> Geodetic CRS:  WGS 84
#> # A tibble: 5,652 × 19
#>    dates               service stationID station_name station_province elevation
#>  * <dttm>              <chr>   <chr>     <chr>        <chr>                <dbl>
#>  1 2022-04-01 00:00:00 meteoc… C6        Castellnou … Lleida                264 
#>  2 2022-04-01 00:00:00 meteoc… C7        Tàrrega      Lleida                427 
#>  3 2022-04-01 00:00:00 meteoc… C8        Cervera      Lleida                554 
#>  4 2022-04-01 00:00:00 meteoc… C9        Mas de Barb… Tarragona             240 
#>  5 2022-04-01 00:00:00 meteoc… CC        Orís         Barcelona             626 
#>  6 2022-04-01 00:00:00 meteoc… CD        la Seu d'Ur… Lleida                849 
#>  7 2022-04-01 00:00:00 meteoc… CE        els Hostale… Barcelona             316 
#>  8 2022-04-01 00:00:00 meteoc… CG        Molló - Fab… Girona               1405 
#>  9 2022-04-01 00:00:00 meteoc… CI        Sant Pau de… Girona                852 
#> 10 2022-04-01 00:00:00 meteoc… CJ        Organyà      Lleida                566.
#> # ℹ 5,642 more rows
#> # ℹ 13 more variables: MeanTemperature <dbl>, MinTemperature <dbl>,
#> #   MaxTemperature <dbl>, MeanRelativeHumidity <dbl>,
#> #   MinRelativeHumidity <dbl>, MaxRelativeHumidity <dbl>, Precipitation <dbl>,
#> #   WindDirection <dbl>, WindSpeed <dbl>, Radiation <dbl>, geom <POINT [°]>,
#> #   aspect <dbl>, slope <dbl>
```

`meteoland` expects variable names to be as indicated in the example:

``` r
names(meteoland_meteo_example)
#>  [1] "dates"                "service"              "stationID"           
#>  [4] "station_name"         "station_province"     "elevation"           
#>  [7] "MeanTemperature"      "MinTemperature"       "MaxTemperature"      
#> [10] "MeanRelativeHumidity" "MinRelativeHumidity"  "MaxRelativeHumidity" 
#> [13] "Precipitation"        "WindDirection"        "WindSpeed"           
#> [16] "Radiation"            "geom"                 "aspect"              
#> [19] "slope"
```

The only mandatory variables are `MinTemperature` and `MaxTemperature`.
Other variables (`Precipitation`, `WindSpeed`…), when present, allow for
a more complete weather interpolation.

For more information on preparing meteorological data for `meteoland`,
see
[`vignette("reshaping-meteo", package = "meteoland")`](https://emf-creaf.github.io/meteoland/articles/reshaping-meteo.md)

#### Quick interpolation (if everything is ok)

With the necessary data in the correct format we can perform the
interpolation right away:

``` r
# creating the interpolator object
interpolator <- with_meteo(meteoland_meteo_example) |>
  create_meteo_interpolator()
#> ℹ Checking meteorology object...
#> ✔ meteorology object ok
#> ℹ Creating interpolator...
#> Warning: No interpolation parameters provided, using defaults
#> ℹ Set the `params` argument to modify parameter default values
#> • Calculating smoothed variables...
#> • Updating intial_Rp parameter with the actual stations mean distance...
#> ✔ Interpolator created.

# performing the interpolation
points_interpolated <- points_to_interpolate_example |>
  interpolate_data(interpolator)
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
points_interpolated
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
```

Let’s see this step by step.

- `with_meteo(...)` ensures that the provided meteorological information
  is in the correct format for using it with `meteoland`. It performs
  several checks and informs of any error found. For example, if the
  meteorological information doesn’t have the mandatory variables, an
  informative error is shown:

``` r
meteo_without_temp <- meteoland_meteo_example
meteo_without_temp[["MinTemperature"]] <- NULL
meteo_without_temp[["MaxTemperature"]] <- NULL
with_meteo(meteo_without_temp)
#> ℹ Checking meteorology object...
#> Error: Names found in meteo don't comply with the required names:
#> meteo should have the following meteorology variables:
#>   - MinTemperature ***
#>   - MaxTemperature ***
#>   - Precipitation
#>   - RelativeHumidity
#>   - Radiation
#>   - WindSpeed
#>   - WindDirection
#> 
#>  ***: mandatory variables
```

- `create_meteo_interpolator(...)` creates the interpolator object from
  the meteorological information. This object stores not only the
  meteorological information, but also the parameters that will be used
  in the interpolation process. These parameters can be supplied as a
  list in the `params` argument from
  [`create_meteo_interpolator()`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md)
  function. If not supplied, a default set of parameters is used. At any
  point, we can display the interpolation parameters using
  [`get_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/get_interpolation_params.md):

``` r
# parameters
get_interpolation_params(interpolator)
#> $initial_Rp
#> [1] 104929
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
```

- `interpolate_data(...)` performs the interpolation using the
  topography provided and the interpolator object. If the topographical
  information is in an `sf` object, as in the example above,
  `interpolate_data` returns the same `sf` object with an additional
  column called `interpolated_data`:

``` r
# interpolated meteo for the first location
points_interpolated[["interpolated_data"]][1]
#> [[1]]
#> # A tibble: 30 × 13
#>    dates                 DOY MeanTemperature MinTemperature MaxTemperature
#>    <dttm>              <dbl>           <dbl>          <dbl>          <dbl>
#>  1 2022-04-01 00:00:00    91            2.91          -2.57           6.47
#>  2 2022-04-02 00:00:00    92            2.61          -3.70           6.71
#>  3 2022-04-03 00:00:00    93            1.92          -4.79           6.28
#>  4 2022-04-04 00:00:00    94            3.99          -3.57           8.91
#>  5 2022-04-05 00:00:00    95            6.79          -2.01          12.5 
#>  6 2022-04-06 00:00:00    96            8.83           1.96          13.3 
#>  7 2022-04-07 00:00:00    97           12.2            2.90          18.2 
#>  8 2022-04-08 00:00:00    98           13.0            5.48          17.8 
#>  9 2022-04-09 00:00:00    99           10.1            4.34          13.9 
#> 10 2022-04-10 00:00:00   100            9.88           3.65          13.9 
#> # ℹ 20 more rows
#> # ℹ 8 more variables: Precipitation <dbl>, MeanRelativeHumidity <dbl>,
#> #   MinRelativeHumidity <dbl>, MaxRelativeHumidity <dbl>, Radiation <dbl>,
#> #   WindSpeed <dbl>, WindDirection <dbl>, PET <dbl>
```

We can “unnest” the results to get the data in a *long* format (each
combination of location and date in a different row):

``` r
tidyr::unnest(points_interpolated, cols = "interpolated_data")
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
```

### Interpolator object

In older versions of meteoland, the interpolator object inherited the
`MeteorologyInterpolationData` class (based on `sp` classes). Starting
on `meteoland v2.0.0`, `MeteorologyInterpolationData` class is
deprecated, and the interpolator object created by
[`create_meteo_interpolator()`](https://emf-creaf.github.io/meteoland/reference/create_meteo_interpolator.md)
inherits directly from `stars` class.

``` r
class(interpolator)
#> [1] "stars"
```

This object is a data cube, with reference weather locations and dates
as dimensions, and meteorological and topographical variables as
attributes.

``` r
interpolator
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

The object also contains the interpolation parameters as an attribute,
that can be accessed with
[`get_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/get_interpolation_params.md).

``` r
get_interpolation_params(interpolator)
#> $initial_Rp
#> [1] 104929
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
```

Interpolation parameters can also be changed with
[`set_interpolation_params()`](https://emf-creaf.github.io/meteoland/reference/set_interpolation_params.md).

``` r
# wind_height parameter
get_interpolation_params(interpolator)$wind_height
#> [1] 10

# set a new wind_height parameter and check
interpolator <- set_interpolation_params(interpolator, params = list(wind_height = 5))
#> ℹ Some interpolation parameters are missing, using default values for those
get_interpolation_params(interpolator)$wind_height
#> [1] 5
```

#### Writing and reading interpolator objects

Interpolator objects can be reused for different interpolations
exercises within the area covered by the interpolator. To allow
interpolator objects to be shared between sessions, `meteoland` offers
functions to write and read these objects. The interpolator is saved in
NetCDF-CF format
(<https://cfconventions.org/cf-conventions/cf-conventions.html>) and can
be also opened with any GIS software that supports NetCDF-CF.

``` r
temporal_folder <- tempdir()
write_interpolator(interpolator, file.path(temporal_folder, "interpolator.nc"))
#> ℹ Creating nc file following the NetCDF-CF conventions <https://cfconventions.org/cf-conventions/cf-conventions.html>
#> ℹ Adding spatial info to nc file
#> ✔ Done
# file should exists now
file.exists(file.path(temporal_folder, "interpolator.nc"))
#> [1] TRUE
```

To load the interpolator in your session again, you can use the
[`read_interpolator()`](https://emf-creaf.github.io/meteoland/reference/read_interpolator.md)
function.

``` r
file_interpolator <- read_interpolator(file.path(temporal_folder, "interpolator.nc"))
# the read interpolator should be identical to the one we have already
identical(file_interpolator, interpolator)
#> [1] TRUE
```

#### Interpolator calibration

Interpolation parameters can be calibrated for individual variables
before performing the interpolation process. In fact, **it’s
recommended** to calibrate the interpolator object before using it, as
the default interpolation parameters can be not adequate for the studied
area. `meteoland` offers a calibration process with the
[`interpolator_calibration()`](https://emf-creaf.github.io/meteoland/reference/interpolator_calibration.md)
function.

    **Important!** Calibration process for one variable can take a long time to finish,
    as it performs a *leave-one-out* interpolation for all stations present in the
    interpolator and all combinations of N and alpha sequences provided. In this example
    we reduce the N and alpha test values for the process to be faster, but is recommended
    to explore a wider range of these values.

``` r
# min temperature N and alpha before calibration
get_interpolation_params(interpolator)$N_MinTemperature
#> [1] 30
get_interpolation_params(interpolator)$alpha_MinTemperature
#> [1] 3

# calibration
interpolator <- interpolator_calibration(
  interpolator,
  variable = "MinTemperature",
  N_seq = c(10, 20),
  alpha_seq = c(1, 10),
  update_interpolation_params = TRUE
)
#> ℹ Total number of stations: 189
#> ℹ Number of stations with available data: 185
#> ℹ Number of stations used for MAE calculation: 185
#> ℹ Number of parameters combinations to test: 4
#> ℹ Starting evaluation of parameter combinations for "MinTemperature"...
#> • Evaluating N: 10, alpha: 1...
#> • Evaluating N: 10, alpha: 10...
#> • Evaluating N: 20, alpha: 1...
#> • Evaluating N: 20, alpha: 10...
#> ✔ Calibration done: Minimum MAE: 1.60272632009794; N: 10; alpha: 1

# parameters after calibration
get_interpolation_params(interpolator)$N_MinTemperature
#> [1] 10
get_interpolation_params(interpolator)$alpha_MinTemperature
#> [1] 1
```

One advantage of the new data flows in `meteoland` is that we can *pipe*
the creation and the calibration of the interpolator, as well as the
writing:

``` r
interpolator <- with_meteo(meteoland_meteo_example) |>
  create_meteo_interpolator() |>
  interpolator_calibration(
    variable = "MinTemperature",
    N_seq = c(10, 20),
    alpha_seq = c(1, 10),
    update_interpolation_params = TRUE
  ) |>
  interpolator_calibration(
    variable = "MaxTemperature",
    N_seq = c(10, 20),
    alpha_seq = c(1, 10),
    update_interpolation_params = TRUE
  ) |>
  interpolator_calibration(
    variable = "DewTemperature",
    N_seq = c(10, 20),
    alpha_seq = c(1, 10),
    update_interpolation_params = TRUE
  ) |>
  write_interpolator(
    filename = file.path(temporal_folder, "interpolator.nc"),
    .overwrite = TRUE
  )
#> ℹ Checking meteorology object...
#> ✔ meteorology object ok
#> ℹ Creating interpolator...
#> Warning: No interpolation parameters provided, using defaults
#> ℹ Set the `params` argument to modify parameter default values
#> • Calculating smoothed variables...
#> • Updating intial_Rp parameter with the actual stations mean distance...
#> ✔ Interpolator created.
#> ℹ Total number of stations: 189
#> ℹ Number of stations with available data: 185
#> ℹ Number of stations used for MAE calculation: 185
#> ℹ Number of parameters combinations to test: 4
#> ℹ Starting evaluation of parameter combinations for "MinTemperature"...
#> • Evaluating N: 10, alpha: 1...
#> • Evaluating N: 10, alpha: 10...
#> • Evaluating N: 20, alpha: 1...
#> • Evaluating N: 20, alpha: 10...
#> ✔ Calibration done: Minimum MAE: 1.60272632009794; N: 10; alpha: 1
#> ℹ Total number of stations: 189
#> ℹ Number of stations with available data: 185
#> ℹ Number of stations used for MAE calculation: 185
#> ℹ Number of parameters combinations to test: 4
#> ℹ Starting evaluation of parameter combinations for "MaxTemperature"...
#> • Evaluating N: 10, alpha: 1...
#> • Evaluating N: 10, alpha: 10...
#> • Evaluating N: 20, alpha: 1...
#> • Evaluating N: 20, alpha: 10...
#> ✔ Calibration done: Minimum MAE: 1.53562144932804; N: 10; alpha: 1
#> ℹ Total number of stations: 189
#> ℹ Number of stations with available data: 185
#> ℹ Number of stations used for MAE calculation: 185
#> ℹ Number of parameters combinations to test: 4
#> ℹ Starting evaluation of parameter combinations for "DewTemperature"...
#> • Evaluating N: 10, alpha: 1...
#> • Evaluating N: 10, alpha: 10...
#> • Evaluating N: 20, alpha: 1...
#> • Evaluating N: 20, alpha: 10...
#> ✔ Calibration done: Minimum MAE: 2.52763750837408; N: 10; alpha: 10
#> ℹ Creating nc file following the NetCDF-CF conventions <https://cfconventions.org/cf-conventions/cf-conventions.html>
#> ℹ Adding spatial info to nc file
#> ✔ Done
```

This way we can create and calibrate the interpolator once, and using it
in future sessions, avoiding the time consuming step of calibrating
every time.

### Interpolation validation

The interpolation process can be cross validated. `meteoland` offers the
possibility with the
[`interpolation_cross_validation()`](https://emf-creaf.github.io/meteoland/reference/interpolator_calibration.md)
function. This function takes an interpolator object and calculates
different error measures.

``` r
cross_validation <- interpolation_cross_validation(interpolator, verbose = FALSE)
cross_validation$errors
#> # A tibble: 5,670 × 21
#>    dates               station stationID MinTemperature_error
#>    <dttm>                <int> <chr>                    <dbl>
#>  1 2022-04-01 00:00:00       1 C6                       1.05 
#>  2 2022-04-02 00:00:00       1 C6                       0.797
#>  3 2022-04-03 00:00:00       1 C6                       3.71 
#>  4 2022-04-04 00:00:00       1 C6                       1.25 
#>  5 2022-04-05 00:00:00       1 C6                       2.54 
#>  6 2022-04-06 00:00:00       1 C6                       0.750
#>  7 2022-04-07 00:00:00       1 C6                      -0.124
#>  8 2022-04-08 00:00:00       1 C6                       1.76 
#>  9 2022-04-09 00:00:00       1 C6                       1.68 
#> 10 2022-04-10 00:00:00       1 C6                       3.13 
#> # ℹ 5,660 more rows
#> # ℹ 17 more variables: MaxTemperature_error <dbl>,
#> #   RangeTemperature_error <dbl>, RelativeHumidity_error <dbl>,
#> #   Radiation_error <dbl>, Precipitation_error <dbl>,
#> #   MinTemperature_predicted <dbl>, MaxTemperature_predicted <dbl>,
#> #   RangeTemperature_predicted <dbl>, RelativeHumidity_predicted <dbl>,
#> #   Radiation_predicted <dbl>, Precipitation_predicted <dbl>, …
cross_validation$station_stats
#> # A tibble: 189 × 20
#>    station stationID MinTemperature_station_bias MaxTemperature_station_bias
#>      <int> <chr>                           <dbl>                       <dbl>
#>  1       1 C6                             1.13                        -0.990
#>  2       2 C7                            -0.682                       -1.00 
#>  3       3 C8                            -0.0987                      -0.186
#>  4       4 C9                            -1.63                         0.979
#>  5       5 CC                             0.749                       -2.04 
#>  6       6 CD                             0.730                       -3.04 
#>  7       7 CE                            -1.35                        -1.22 
#>  8       8 CG                            -0.963                       -0.134
#>  9       9 CI                             1.50                        -0.107
#> 10      10 CJ                             0.905                       -2.35 
#> # ℹ 179 more rows
#> # ℹ 16 more variables: RangeTemperature_station_bias <dbl>,
#> #   RelativeHumidity_station_bias <dbl>, Radiation_station_bias <dbl>,
#> #   MinTemperature_station_mae <dbl>, MaxTemperature_station_mae <dbl>,
#> #   RangeTemperature_station_mae <dbl>, RelativeHumidity_station_mae <dbl>,
#> #   Radiation_station_mae <dbl>, TotalPrecipitation_station_observed <dbl>,
#> #   TotalPrecipitation_station_predicted <dbl>, …
cross_validation$dates_stats
#> # A tibble: 30 × 19
#>    dates               MinTemperature_date_bias MaxTemperature_date_bias
#>    <dttm>                                 <dbl>                    <dbl>
#>  1 2022-04-01 00:00:00                -0.0650                    0.00705
#>  2 2022-04-02 00:00:00                -0.0653                   -0.0375 
#>  3 2022-04-03 00:00:00                -0.0932                   -0.199  
#>  4 2022-04-04 00:00:00                 0.00444                  -0.0577 
#>  5 2022-04-05 00:00:00                -0.0363                   -0.168  
#>  6 2022-04-06 00:00:00                -0.0254                   -0.0960 
#>  7 2022-04-07 00:00:00                -0.0556                   -0.0636 
#>  8 2022-04-08 00:00:00                -0.0423                   -0.0879 
#>  9 2022-04-09 00:00:00                -0.00476                  -0.152  
#> 10 2022-04-10 00:00:00                 0.000356                 -0.0217 
#> # ℹ 20 more rows
#> # ℹ 16 more variables: RangeTemperature_date_bias <dbl>,
#> #   RelativeHumidity_date_bias <dbl>, Radiation_date_bias <dbl>,
#> #   MinTemperature_date_mae <dbl>, MaxTemperature_date_mae <dbl>,
#> #   RangeTemperature_date_mae <dbl>, RelativeHumidity_date_mae <dbl>,
#> #   Radiation_date_mae <dbl>, TotalPrecipitation_date_observed <dbl>,
#> #   TotalPrecipitation_date_predicted <dbl>, …
cross_validation$r2
#> $MinTemperature
#> [1] 0.9078197
#> 
#> $MaxTemperature
#> [1] 0.9480262
#> 
#> $RangeTemperature
#> [1] 0.7381034
#> 
#> $RelativeHumidity
#> [1] 0.5231079
#> 
#> $Radiation
#> [1] 0.8179651
```

### Interpolation utils

`meteoland` also offers some utilities to work with the interpolated
data.

#### Temporal summary of interpolated data

`meteoland` works at the daily scale. But sometimes the data needs to be
aggregated into bigger temporal scales (monthly, quarterly, yearly…).
This can be done with the
[`summarise_interpolated_data()`](https://emf-creaf.github.io/meteoland/reference/summarise_interpolated_data.md)
function. This function takes the result of `interpolate_data` and
creates summaries in the desired frequency.  
The function returns the same interpolated data given as input, but with
the weekly summary in an additional column.

``` r
summarise_interpolated_data(
  points_interpolated,
  fun = "mean",
  frequency = "week"
)
#> Simple feature collection with 15 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.7578958 ymin: 41.31541 xmax: 2.98657 ymax: 42.6336
#> Geodetic CRS:  WGS 84
#> # A tibble: 15 × 7
#>    plot_id elevation slope aspect             geometry interpolated_data 
#>  * <chr>       <dbl> <dbl>  <dbl>          <POINT [°]> <list>            
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
#> # ℹ 1 more variable: weekly_mean <list>
```

#### Calculating rainfall erosivity

`meteoland` also offers the possibility of calculating the rainfall
erosivity value, with the
[`precipitation_rainfall_erosivity()`](https://emf-creaf.github.io/meteoland/reference/precipitation_rainfall_erosivity.md)
function. This can be used for individual locations:

``` r
precipitation_rainfall_erosivity(
  points_interpolated$interpolated_data[[1]],
  longitude = sf::st_coordinates(points_interpolated$geometry[[1]])[,1],
  scale = 'month'
)
#>        4 
#> 54.12229
```

But also for all locations in the results obtained from the call to
[`interpolate_data()`](https://emf-creaf.github.io/meteoland/reference/interpolate_data.md):

``` r
points_interpolated |>
  mutate(erosivity = precipitation_rainfall_erosivity(
    interpolated_data,
    longitude = sf::st_coordinates(geometry)[,1],
    scale = 'month'
  ))
#> Simple feature collection with 15 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.7578958 ymin: 41.31541 xmax: 2.98657 ymax: 42.6336
#> Geodetic CRS:  WGS 84
#> # A tibble: 15 × 7
#>    plot_id elevation slope aspect             geometry interpolated_data 
#>  * <chr>       <dbl> <dbl>  <dbl>          <POINT [°]> <list>            
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
#> # ℹ 1 more variable: erosivity <list>
```

#### Piping all together

`meteoland` new data flows also allows for piping all processes:

``` r
points_interpolated <- points_to_interpolate_example |>
  interpolate_data(interpolator) |>
  summarise_interpolated_data(
    fun = "mean",
    frequency = "week"
  ) |>
  summarise_interpolated_data(
    fun = "max",
    frequency = "month"
  ) |>
  mutate(
    monthly_erosivity = precipitation_rainfall_erosivity(
      interpolated_data,
      longitude = sf::st_coordinates(geometry)[,1],
      scale = 'month'
    )
  )
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

points_interpolated
#> Simple feature collection with 15 features and 8 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.7578958 ymin: 41.31541 xmax: 2.98657 ymax: 42.6336
#> Geodetic CRS:  WGS 84
#> # A tibble: 15 × 9
#>    plot_id elevation slope aspect             geometry interpolated_data 
#>  * <chr>       <dbl> <dbl>  <dbl>          <POINT [°]> <list>            
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
#> # ℹ 3 more variables: weekly_mean <list>, monthly_max <list>,
#> #   monthly_erosivity <list>
```

### Interpolation on raster data

We can use the
[`interpolate_data()`](https://emf-creaf.github.io/meteoland/reference/interpolate_data.md)
function in a raster type of data. All we need in this case is the
topography information in a `stars` object, with `elevation`, `aspect`
and `slope` variables as raster attributes:

``` r
raster_to_interpolate_example
#> stars object with 2 dimensions and 3 attributes
#> attribute(s):
#>                  Min.    1st Qu.    Median      Mean   3rd Qu.     Max.
#> elevation  240.000000 370.000000 447.00000 460.32231 525.00000 786.0000
#> slope        1.432096   5.720433  11.34812  13.07343  19.75851  31.0719
#> aspect       5.194427  74.744881 174.36932 181.67923 291.03751 360.0000
#> dimension(s):
#>   from  to  offset    delta refsys x/y
#> x  155 165 0.03648  0.01058 WGS 84 [x]
#> y  110 120   42.92 -0.01058 WGS 84 [y]
```

In this case, the raster is a 0.01 degree grid (10x10 cells) in central
Catalonia. As the raster is inside the area covered by the interpolator
object we created before, we will use it.

``` r
raster_interpolated <- raster_to_interpolate_example |>
  interpolate_data(interpolator)
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
#> ℹ Binding together interpolation results
#> ✔ Interpolation process finished

raster_interpolated
#> stars object with 3 dimensions and 14 attributes
#> attribute(s):
#>                              Min.     1st Qu.      Median       Mean    3rd Qu.
#> MeanTemperature         2.8490648  11.4140076  13.7598510  13.112524  15.735024
#> MinTemperature         -3.9757928   5.2174718   7.2458877   6.133785   8.331064
#> MaxTemperature          7.2863484  14.4514055  18.2719087  17.649855  21.016983
#> Precipitation           0.0000000   0.0000000   0.0000000   2.032412   0.000000
#> MeanRelativeHumidity   38.4267872  57.9520715  64.6876407  65.897412  72.914524
#> MinRelativeHumidity    29.1965595  41.4457445  47.1793793  49.929963  55.104881
#> MaxRelativeHumidity    57.7022192  99.8411153 100.0000000  96.895373 100.000000
#> Radiation               7.0825040  16.3638164  21.1332488  19.681308  23.761188
#> WindSpeed               0.1360482   0.4943011   0.9828521   1.226064   1.760093
#> WindDirection           5.6223719 133.2179877 192.2048919 184.924892 266.634100
#> PET                     1.0797807   2.4104736   3.1072622   2.994170   3.633948
#> elevation             240.0000000 370.0000000 447.0000000 460.322314 525.000000
#> slope                   1.4320962   5.7204332  11.3481197  13.073426  19.758511
#> aspect                  5.1944275  74.7448807 174.3693237 181.679232 291.037506
#>                             Max. NA's
#> MeanTemperature        19.994531    0
#> MinTemperature         11.397862    0
#> MaxTemperature         26.415294    0
#> Precipitation          13.948289    0
#> MeanRelativeHumidity  100.000000    0
#> MinRelativeHumidity    91.400189    0
#> MaxRelativeHumidity   100.000000    0
#> Radiation              27.506054    0
#> WindSpeed               3.782339    0
#> WindDirection         341.434389 1331
#> PET                     4.680851    0
#> elevation             786.000000    0
#> slope                  31.071896    0
#> aspect                360.000000    0
#> dimension(s):
#>      from to         offset    delta  refsys point x/y
#> x       1 11          1.671  0.01058  WGS 84 FALSE [x]
#> y       1 11          41.76 -0.01058  WGS 84 FALSE [y]
#> date    1 30 2022-04-01 UTC   1 days POSIXct FALSE
```

As we can see, the returned object is the same `stars` raster provided
to the function, with the interpolated meteorological variables as new
attributes. Dimensions now include the date, besides the input’s
latitude and longitude.

#### Temporal aggregation in raster

Like with the point location example, interpolated data in raster format
can also be aggregated temporally.

``` r
summarise_interpolated_data(
  raster_interpolated,
  fun = "mean",
  frequency = "week"
)
#> stars object with 3 dimensions and 11 attributes
#> attribute(s):
#>                              Min.     1st Qu.      Median       Mean    3rd Qu.
#> MeanTemperature         3.4231242  11.3148273  12.9934658  12.389613  15.344793
#> MinTemperature         -2.8484754   3.6641267   7.1416277   5.430246   8.110321
#> MaxTemperature          7.5006989  15.8406904  17.4581732  16.914350  19.987648
#> Precipitation           0.0000000   0.0000000   0.6909051   1.761854   2.198979
#> MeanRelativeHumidity   43.5236496  57.1458476  65.6950977  64.026954  70.938816
#> MinRelativeHumidity    32.3377893  41.3946450  48.1248785  48.389657  54.801677
#> MaxRelativeHumidity    70.3819446  95.5026366  99.4423268  94.921346 100.000000
#> Radiation              12.0625342  17.5779569  19.5813399  19.729452  22.125191
#> WindSpeed               0.4260045   0.9835564   1.0015541   1.394959   1.659769
#> WindDirection         145.0180598 145.0181215 175.2971446 175.297140 205.576159
#> PET                     1.5434263   2.6915520   2.9987228   2.988443   3.319860
#>                             Max. NA's
#> MeanTemperature        17.027711    0
#> MinTemperature          9.419550    0
#> MaxTemperature         22.354696    0
#> Precipitation           6.150887    0
#> MeanRelativeHumidity   87.508839    0
#> MinRelativeHumidity    70.124105    0
#> MaxRelativeHumidity   100.000000    0
#> Radiation              25.537471    0
#> WindSpeed               2.903910    0
#> WindDirection         205.576212  363
#> PET                     3.979590    0
#> dimension(s):
#>      from to          offset    delta  refsys point x/y
#> time    1  5 2022-03-28 CEST   7 days POSIXct    NA    
#> x       1 11           1.671  0.01058  WGS 84 FALSE [x]
#> y       1 11           41.76 -0.01058  WGS 84 FALSE [y]
```

In this case the result is the aggregated variables as attributes, and
the time dimension is aggregated in the desired frequency.

#### Piping raster interpolation

As with points, raster interpolation and utilities can also be piped.
This way, if we are only interested in the mean monthly temperature in
our study area, we can do:

``` r
monthly_mean_temperature <- raster_to_interpolate_example |>
  interpolate_data(interpolator, variables = "Temperature") |>
  summarise_interpolated_data(
    fun = "max",
    frequency = "month",
    variable = "MeanTemperature"
  )
#> ℹ Starting interpolation...
#> • Interpolating temperature...
#> ✔ Interpolation done...
#> ℹ Binding together interpolation results
#> ✔ Interpolation process finished

plot(monthly_mean_temperature)
```

![](tidy-meteoland_files/figure-html/raster_piped-1.png)

## Appendix

### Equivalence table

| Function (\< 2.0.0) | Equivalence (\>= 2.0.0) | deprecated |
|----|----|----|
| `averagearea` | No equivalence, aggregating by area can be done with the `sf` package | `TRUE` |
| `correctionpoint`, `correctionpoints`, `correctionpoints.errors`, `correction_series`, `defaultCorrectionParams` | No equivalence, better bias correction methods are provided by other packages (see package `MBC` for example) | `TRUE` |
| `defaultInterpolationParams` | No change | `FALSE` |
| `download_*` functions | No equivalence, weather download functions are now provided by `meteospain` package | `TRUE` |
| `extractdates`, `extractgridindex`, `extractgridpoints`, `extractNetCDF`, `extractvars` | No equivalence, not needed as the meteo objects are now `sf` objects | `TRUE` |
| `humidity_*` conversion tools | No change | `FALSE` |
| `interpolation.calibration` | `interpolator_calibration` | `TRUE` |
| `interpolation.calibration.fmax` | `interpolator_calibration` | `TRUE` |
| `interpolation.coverage` | No equivalence | `TRUE` |
| `interpolation.cv` | `interpolation_cross_validation` | `TRUE` |
| `interpolationgrid`, `interpolationpixels`, `interpolationpoints` | `interpolate_data` | `TRUE` |
| `mergegrid`, `mergepoints` | No equivalence, meteorological objects are now `sf` objects and can be merged, joined or filtered as any data.frame | `TRUE` |
| `meteocomplete` | `complete_meteo` | `TRUE` |
| `meteoplot` | No equivalence, meteo objects are now sf objects and can be plotted as any other data.frame | `TRUE` |
| `Meteorology_*_Data` | No equivalence, spatial classes based on `sp` are now deprecated in `meteoland` | `TRUE` |
| `penman`, `penmanmonteith` | No changes | `FALSE` |
| `plot.interpolation.cv` | No equivalence | `TRUE` |
| `precipitation_concentration` | No equivalence, precipitation_concentration utility is deprecated and will be removed in future versions | `TRUE` |
| `precipitation_rainfallErosivity` | `precipitation_rainfall_erosivity` | `TRUE` |
| `radiation_*` utility functions | No change | `FALSE` |
| `readmeteorology*` | No equivalence, spatial classes based on `sp` are now deprecated in `meteoland` | `TRUE` |
| `readNetCDF*` | No equivalence, NetCDF files can be managed with more recent and up to date R packages (ncmeta, stars…) | `TRUE` |
| `readWindNinjaWindFields` | No equivalence | `TRUE` |
| `reshapemeteospain` | `meteospain2meteoland` | `TRUE` |
| `reshapeworldmet` | `worldmet2meteoland` | `TRUE` |
| `reshapeweathercan` | No equivalence, `weathercan` package was removed from CRAN and the functions are deprecated | `TRUE` |
| `Spatial**Meteorology`, `Spatial**Topography` | No equivalence, spatial classes based on `sp` are now deprecated in `meteoland` | `TRUE` |
| `summary*` | `summarise_interpolate_data`, `summarise_interpolator` | `TRUE` |
| `utils_*` | No change | `FALSE` |
| `weathergeneration`, `defaultGenerationParams` | No equivalence, current weather generation methods are currently deprecated because they operate with classes that are deprecated themselves, but for future versions, we plan to keep the functionality in new functions. | `TRUE` |
| `writemeteorology*` | No equivalence, spatial classes based on `sp` are now deprecated in `meteoland` | `TRUE` |
