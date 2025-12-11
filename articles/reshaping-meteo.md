# Reshaping meteorological data for meteoland

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

## Meteorological data format

For the interpolation of meteorological variables on our target
locations, we need meteorological data for a reference set of locations.
In `meteoland` this is a `sf` object with the spatial coordinates of our
reference locations (usually meteorological stations) and daily values
of the meteorological variables needed to perform the interpolation,
*i.e.*:

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

`meteoland` expects names to be as in the example:

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
a more complete interpolation.

## Converting meteorological data to `meteoland` format

Meteorological data can come in many formats and with different names
for the same variables. As we saw above, we need to convert it to a
`meteoland` compliant format (`sf` object with correct names).

### Converting from data frames

If we have our meteorological data in a data.frame (*i.e.* we obtained
it from our own weather stations, or download it from other source in
this format) we can just simply transform it to the desired format:

``` r
unformatted_meteo
#> # A tibble: 15 × 7
#>    date       station latitude longitude min_temp max_temp    rh
#>    <date>     <chr>      <dbl>     <dbl>    <dbl>    <dbl> <dbl>
#>  1 2022-12-01 a           41.4     -0.33    1.60      6.01  73.4
#>  2 2022-12-02 a           41.4     -0.33   11.5      20.0   54.4
#>  3 2022-12-03 a           41.4     -0.33   -4.62      5.22  56.1
#>  4 2022-12-04 a           41.4     -0.33    9.97     21.6   90.6
#>  5 2022-12-05 a           41.4     -0.33   13.7      21.0   52.8
#>  6 2022-12-01 b           40.1      0.12   16.9      28.3   46.7
#>  7 2022-12-02 b           40.1      0.12   -0.931    10.2   20  
#>  8 2022-12-03 b           40.1      0.12    8.52     14.6   43.0
#>  9 2022-12-04 b           40.1      0.12    8.53     20.7   42.2
#> 10 2022-12-05 b           40.1      0.12    8.30     24.0   76.7
#> 11 2022-12-01 c           42        1.12    6.68     16.4   51.8
#> 12 2022-12-02 c           42        1.12   13.8      21.0   34.0
#> 13 2022-12-03 c           42        1.12   22.4      32.3   48.8
#> 14 2022-12-04 c           42        1.12    0.214     7.73  43.7
#> 15 2022-12-05 c           42        1.12   13.1      18.5   61.1
```

``` r
ready_meteo <- unformatted_meteo |>
  # convert names to correct ones
  dplyr::mutate(
    MinTemperature = min_temp,
    MaxTemperature = max_temp,
    MeanRelativeHumidity = rh
  ) |>
  # transform to sf (WGS84)
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = sf::st_crs(4326)
  )

ready_meteo
#> Simple feature collection with 15 features and 8 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -0.33 ymin: 40.11 xmax: 1.12 ymax: 42
#> Geodetic CRS:  WGS 84
#> # A tibble: 15 × 9
#>    date       station min_temp max_temp    rh MinTemperature MaxTemperature
#>  * <date>     <chr>      <dbl>    <dbl> <dbl>          <dbl>          <dbl>
#>  1 2022-12-01 a          1.60      6.01  73.4          1.60            6.01
#>  2 2022-12-02 a         11.5      20.0   54.4         11.5            20.0 
#>  3 2022-12-03 a         -4.62      5.22  56.1         -4.62            5.22
#>  4 2022-12-04 a          9.97     21.6   90.6          9.97           21.6 
#>  5 2022-12-05 a         13.7      21.0   52.8         13.7            21.0 
#>  6 2022-12-01 b         16.9      28.3   46.7         16.9            28.3 
#>  7 2022-12-02 b         -0.931    10.2   20           -0.931          10.2 
#>  8 2022-12-03 b          8.52     14.6   43.0          8.52           14.6 
#>  9 2022-12-04 b          8.53     20.7   42.2          8.53           20.7 
#> 10 2022-12-05 b          8.30     24.0   76.7          8.30           24.0 
#> 11 2022-12-01 c          6.68     16.4   51.8          6.68           16.4 
#> 12 2022-12-02 c         13.8      21.0   34.0         13.8            21.0 
#> 13 2022-12-03 c         22.4      32.3   48.8         22.4            32.3 
#> 14 2022-12-04 c          0.214     7.73  43.7          0.214           7.73
#> 15 2022-12-05 c         13.1      18.5   61.1         13.1            18.5 
#> # ℹ 2 more variables: MeanRelativeHumidity <dbl>, geometry <POINT [°]>
```

And *voilà*, we have our meteo data in the correct format

### Meteorological data from other R packages

`meteoland` offers transformation functions for meteorological data
downloaded from the
[`meteospain`](https://emf-creaf.github.io/meteospain/) and
[`worldmet`](https://openair-project.github.io/worldmet/) R packages.

#### `meteospain` data

For data coming from `meteospain` package we have the
`meteospain2meteoland` function that transforming the data for us:

``` r
library(meteospain)
get_meteo_from(
  "meteogalicia",
  meteogalicia_options('daily', as.Date("2022-12-01"), as.Date("2022-12-05"))
) |>
  meteospain2meteoland()
```

#### `worldmet` data

For data coming from `worldmet` package we have the `worldmet2meteoland`
function that do the reshaping for us:

``` r
library(worldmet)
worldmet::importNOAA("081120-99999", year = 2022) |>
  worldmet2meteoland()
```

### Meteorological data from raster sources

As we have seen, we need the meteorological reference data in a `sf`
object (points). To be able to create an interpolator from a raster, we
need to transform the cell values to points (*i.e.* using the cell
center coordinates) for each variable and day and use it to create the
interpolator.

So if we have a multi-layered raster with several dates of
meteorological data:

    > Remember that the raster, besides the meteo data, needs to contain also the topographic
    (elevation, aspect and slope) data for the interpolator to work.

``` r
raster_meteo_reference
#> stars object with 3 dimensions and 14 attributes
#> attribute(s):
#>                               Min.     1st Qu.     Median       Mean
#> MeanTemperature         3.47453413  11.4535839  13.940206  13.471788
#> MinTemperature         -3.27100714   4.3824425   6.969667   5.860040
#> MaxTemperature          7.16265820  14.7798101  18.901768  18.420680
#> Precipitation           0.00000000   0.0000000   0.000000   1.217155
#> MeanRelativeHumidity   35.08268335  56.7720829  64.269391  65.520741
#> MinRelativeHumidity    26.85783275  38.7257795  45.058448  48.591400
#> MaxRelativeHumidity    53.84205927 100.0000000 100.000000  96.878881
#> Radiation               7.54372475  15.9837124  20.690395  19.595816
#> WindSpeed               0.02164994   0.9119314   1.252240   1.385929
#> WindDirection           0.18621266  71.4293342 198.955399 181.615512
#> PET                     1.09027039   2.4105288   3.176816   3.087621
#> elevation             240.00000000 370.0000000 447.000000 460.322314
#> slope                   1.43209624   5.7204332  11.348120  13.073426
#> aspect                  5.19442749  74.7448807 174.369324 181.679232
#>                           3rd Qu.       Max. NA's
#> MeanTemperature        16.1442989  20.781408    0
#> MinTemperature          8.2924049  11.071295    0
#> MaxTemperature         22.0316730  28.801911    0
#> Precipitation           0.2922964  21.000409    0
#> MeanRelativeHumidity   75.5124486 100.000000    0
#> MinRelativeHumidity    55.7307897  90.243329    0
#> MaxRelativeHumidity   100.0000000 100.000000    0
#> Radiation              23.7299248  27.982274    0
#> WindSpeed               1.7444530   5.811866   30
#> WindDirection         264.0404014 359.908196 1350
#> PET                     3.7736818   5.625855    0
#> elevation             525.0000000 786.000000    0
#> slope                  19.7585106  31.071896    0
#> aspect                291.0375061 360.000000    0
#> dimension(s):
#>      from to         offset    delta  refsys point x/y
#> x       1 11          1.671  0.01058  WGS 84 FALSE [x]
#> y       1 11          41.76 -0.01058  WGS 84 FALSE [y]
#> date    1 30 2022-04-01 UTC   1 days POSIXct FALSE
```

we need to convert it to points:

``` r
points_meteo_reference <- names(raster_meteo_reference) |>
  # for each variable
  purrr::map(
    # take the variable raster
    ~ raster_meteo_reference[.x] |>
      # convert to sf
      sf::st_as_sf(as_points = TRUE, na.rm = FALSE) |>
      # pivot the data for dates to be in one column
      tidyr::pivot_longer(cols = -geometry, names_to = "dates", values_to = .x) |>
      # convert to tibble to fasten the process
      dplyr::as_tibble() |>
      # convert to date and create stationID
      dplyr::mutate(
        dates = as.Date(dates),
        stationID = as.character(geometry)
      )
  ) |>
  # join all variables
  purrr::reduce(dplyr::left_join) |>
  # create the points sf object
  sf::st_as_sf()
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`
#> Joining with `by = join_by(geometry, dates, stationID)`

points_meteo_reference
#> Simple feature collection with 3630 features and 16 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1.6762 ymin: 41.65133 xmax: 1.781988 ymax: 41.75712
#> Geodetic CRS:  WGS 84
#> # A tibble: 3,630 × 17
#>             geometry dates      MeanTemperature stationID         MinTemperature
#>          <POINT [°]> <date>               <dbl> <chr>                      <dbl>
#>  1 (1.6762 41.75712) 2022-04-01            6.24 c(1.676199866843…          0.541
#>  2 (1.6762 41.75712) 2022-04-02            6.64 c(1.676199866843…         -1.50 
#>  3 (1.6762 41.75712) 2022-04-03            4.82 c(1.676199866843…         -3.13 
#>  4 (1.6762 41.75712) 2022-04-04            6.53 c(1.676199866843…         -1.59 
#>  5 (1.6762 41.75712) 2022-04-05            9.05 c(1.676199866843…         -1.51 
#>  6 (1.6762 41.75712) 2022-04-06           11.8  c(1.676199866843…          3.70 
#>  7 (1.6762 41.75712) 2022-04-07           14.1  c(1.676199866843…          3.67 
#>  8 (1.6762 41.75712) 2022-04-08           15.1  c(1.676199866843…          7.71 
#>  9 (1.6762 41.75712) 2022-04-09           13.6  c(1.676199866843…          6.79 
#> 10 (1.6762 41.75712) 2022-04-10           11.6  c(1.676199866843…          4.94 
#> # ℹ 3,620 more rows
#> # ℹ 12 more variables: MaxTemperature <dbl>, Precipitation <dbl>,
#> #   MeanRelativeHumidity <dbl>, MinRelativeHumidity <dbl>,
#> #   MaxRelativeHumidity <dbl>, Radiation <dbl>, WindSpeed <dbl>,
#> #   WindDirection <dbl>, PET <dbl>, elevation <dbl>, slope <dbl>, aspect <dbl>
```

And now we can use it to build an interpolator object:

``` r
with_meteo(points_meteo_reference) |>
  create_meteo_interpolator()
#> ℹ Checking meteorology object...
#> ✔ meteorology object ok
#> ℹ Creating interpolator...
#> Warning: No interpolation parameters provided, using defaults
#> ℹ Set the `params` argument to modify parameter default values
#> • Calculating smoothed variables...
#> • Updating intial_Rp parameter with the actual stations mean distance...
#> ✔ Interpolator created.
#> stars object with 2 dimensions and 13 attributes
#> attribute(s):
#>                                   Min.     1st Qu.     Median       Mean
#> Temperature                 3.47453413  11.4535839  13.940206  13.471788
#> MinTemperature             -3.27100714   4.3824425   6.969667   5.860040
#> MaxTemperature              7.16265820  14.7798101  18.901768  18.420680
#> RelativeHumidity           35.08268335  56.7720829  64.269391  65.520741
#> Precipitation               0.00000000   0.0000000   0.000000   1.217155
#> Radiation                   7.54372475  15.9837124  20.690395  19.595816
#> WindDirection               0.18621266  71.4293342 198.955399 181.615512
#> WindSpeed                   0.02164994   0.9119314   1.252240   1.385929
#> elevation                 240.00000000 370.0000000 447.000000 460.322314
#> aspect                      5.19442749  74.7448807 174.369324 181.679232
#> slope                       1.43209624   5.7204332  11.348120  13.073426
#> SmoothedPrecipitation       0.25336109   1.5179424   3.874980   3.862412
#> SmoothedTemperatureRange    9.41552220  11.8383211  12.587418  12.458785
#>                               3rd Qu.       Max. NA's
#> Temperature                16.1442989  20.781408    0
#> MinTemperature              8.2924049  11.071295    0
#> MaxTemperature             22.0316730  28.801911    0
#> RelativeHumidity           75.5124486 100.000000    0
#> Precipitation               0.2922964  21.000409    0
#> Radiation                  23.7299248  27.982274    0
#> WindDirection             264.0404014 359.908196 1350
#> WindSpeed                   1.7444530   5.811866   30
#> elevation                 525.0000000 786.000000    0
#> aspect                    291.0375061 360.000000    0
#> slope                      19.7585106  31.071896    0
#> SmoothedPrecipitation       6.2789283  11.753856  505
#> SmoothedTemperatureRange   13.2623789  15.014616    0
#> dimension(s):
#>         from  to     offset  delta refsys point
#> date       1  30 2022-04-01 1 days   Date FALSE
#> station    1 121         NA     NA WGS 84  TRUE
#>                                                        values
#> date                                                     NULL
#> station POINT (1.6762 41.65133),...,POINT (1.781988 41.75712)
```
