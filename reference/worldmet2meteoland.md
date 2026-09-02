# From worldmet to meteoland meteo objects

Adapting
[`importNOAA`](https://openair-project.github.io/worldmet/reference/deprecated-isd.html)
meteo objects to meteoland meteo objects

## Usage

``` r
worldmet2meteoland(
  meteo,
  complete = FALSE,
  params = defaultInterpolationParams()
)
```

## Arguments

- meteo:

  worldmet meteo object.

- complete:

  logical indicating if the meteo data missing variables should be
  calculated (if possible). Default to FALSE.

- params:

  A list containing parameters for PET estimation. By default the result
  of
  [`defaultInterpolationParams`](https://emf-creaf.github.io/meteoland/reference/defaultInterpolationParams.md).

## Value

a compatible meteo object to use with meteoland.

## Details

This function converts
[`importNOAA`](https://openair-project.github.io/worldmet/reference/deprecated-isd.html)
meteo objects to compatible meteoland meteo objects by selecting the
needed variables and adapting the names to comply with meteoland
requirements. Also it aggregates subdaily data as well as complete
missing variables if possible (setting `complete = TRUE`)

## Examples

``` r

if (interactive()) {
  # worldmet data
  library(worldmet)
  worldmet_stations <- worldmet::getMeta(lat = 42, lon = 0, n = 2, plot = FALSE)
  worldmet_subdaily_2022 <-
    worldmet::importNOAA(worldmet_stations$code, year = 2022, hourly = TRUE)

  # just convert
  worldmet2meteoland(worldmet_subdaily_2022)
  # convert and complete
  worldmet2meteoland(worldmet_subdaily_2022, complete = TRUE)

}
#> Warning: ! The integrated surface database has been deprecated by NOAA, and data is now
#>   only available until 2025.
#> ℹ Please consider using `worldmet::import_ghcn_stations()` and
#>   `worldmet::import_ghcn_hourly()` to access data from the new Global
#>   Historical Climatology Network.
#> This warning is displayed once every 8 hours.
#>  ■■■■■■■■■■■■■■■■                  50% |  ETA:  7s
#> Warning: Provided meteo data seems to be in subdaily time steps, aggregating to daily
#> scale
#> Warning: Provided meteo data seems to be in subdaily time steps, aggregating to daily
#> scale
#> ℹ Completing missing variables if possible:
#> • RelativeHumidity
#> • MinRelativeHumidity
#> • MaxRelativeHumidity
#> • Radiation
#> • PET
#> ✔ Done
#> Simple feature collection with 730 features and 16 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -0.3333333 ymin: 41.72778 xmax: 0.53583 ymax: 42.08333
#> Geodetic CRS:  WGS 84
#> # A tibble: 730 × 17
#>    dates               stationID    elevation MeanTemperature MinTemperature
#>  * <dttm>              <chr>            <dbl>           <dbl>          <dbl>
#>  1 2022-01-01 00:00:00 080940-99999       554            7.78          2.57 
#>  2 2022-01-01 00:00:00 084515-99999       350            3.17          1    
#>  3 2022-01-02 00:00:00 080940-99999       554            7.87          3.4  
#>  4 2022-01-02 00:00:00 084515-99999       350            2.39         -1    
#>  5 2022-01-03 00:00:00 080940-99999       554            7.13          2.77 
#>  6 2022-01-03 00:00:00 084515-99999       350            5.15          0    
#>  7 2022-01-04 00:00:00 080940-99999       554            8.43          3.87 
#>  8 2022-01-04 00:00:00 084515-99999       350            6.58          1    
#>  9 2022-01-05 00:00:00 080940-99999       554            4.57         -0.433
#> 10 2022-01-05 00:00:00 084515-99999       350            6.17          0.5  
#> # ℹ 720 more rows
#> # ℹ 12 more variables: MaxTemperature <dbl>, MeanRelativeHumidity <dbl>,
#> #   MinRelativeHumidity <dbl>, MaxRelativeHumidity <dbl>, Precipitation <dbl>,
#> #   WindSpeed <dbl>, WindDirection <dbl>, Radiation <dbl>,
#> #   geometry <POINT [°]>, PET <dbl>, aspect <dbl>, slope <dbl>
```
