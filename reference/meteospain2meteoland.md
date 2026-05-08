# From meteospain to meteoland meteo objects

Adapting meteospain meteo objects to meteoland meteo objects

## Usage

``` r
meteospain2meteoland(
  meteo,
  complete = FALSE,
  params = defaultInterpolationParams()
)
```

## Arguments

- meteo:

  meteospain meteo object.

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

This function converts `meteospain` R package meteo objects to
compatible meteoland meteo objects by selecting the needed variables and
adapting the names to comply with meteoland requirements.

## Examples

``` r

if (interactive()) {
  # meteospain data
  library(meteospain)
  mg_april_2022_data <- get_meteo_from(
    "meteogalicia",
    meteogalicia_options("daily", as.Date("2022-04-01"), as.Date("2022-04-30"))
  )

  # just convert
  meteospain2meteoland(mg_april_2022_data)
  # convert and complete
  meteospain2meteoland(mg_april_2022_data, complete = TRUE)

}
#> ℹ Completing missing variables if possible:
#> • RelativeHumidity
#> • MinRelativeHumidity
#> • MaxRelativeHumidity
#> • Radiation
#> • PET
#> ✔ Done
#> Simple feature collection with 4800 features and 30 fields (with 300 geometries empty)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -9.178318 ymin: 41.8982 xmax: -6.765224 ymax: 43.70426
#> Geodetic CRS:  WGS 84
#> # A tibble: 4,800 × 31
#>    dates               service stationID station_name station_province elevation
#>  * <dttm>              <chr>   <chr>     <chr>        <chr>                <dbl>
#>  1 2022-04-01 00:00:00 meteog… 10045     Mabegondo    A Coruña                94
#>  2 2022-04-01 00:00:00 meteog… 10046     Marco da Cu… A Coruña               651
#>  3 2022-04-01 00:00:00 meteog… 10047     Pedro Murias Lugo                    51
#>  4 2022-04-01 00:00:00 meteog… 10048     O Invernade… Ourense               1026
#>  5 2022-04-01 00:00:00 meteog… 10049     Corrubedo    A Coruña                30
#>  6 2022-04-01 00:00:00 meteog… 10050     CIS Ferrol   A Coruña                37
#>  7 2022-04-01 00:00:00 meteog… 10052     Muralla      A Coruña               661
#>  8 2022-04-01 00:00:00 meteog… 10053     Campus Lugo  Lugo                   400
#>  9 2022-04-01 00:00:00 meteog… 10055     Guitiriz-Mi… Lugo                   684
#> 10 2022-04-01 00:00:00 meteog… 10056     Marroxo      Lugo                   645
#> # ℹ 4,790 more rows
#> # ℹ 25 more variables: MeanTemperature <dbl>, MinTemperature <dbl>,
#> #   MaxTemperature <dbl>, MeanRelativeHumidity <dbl>,
#> #   MinRelativeHumidity <dbl>, MaxRelativeHumidity <dbl>, Precipitation <dbl>,
#> #   WindDirection <dbl>, max_wind_direction <dbl>, WindSpeed <dbl>,
#> #   max_wind_speed <dbl>, insolation <dbl>, insolation_ratio <dbl>,
#> #   global_solar_irradiation <dbl>, mean_atmospheric_pressure <dbl>, …
```
