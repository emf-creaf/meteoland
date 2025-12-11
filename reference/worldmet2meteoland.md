# From worldmet to meteoland meteo objects

Adapting
[`importNOAA`](https://openair-project.github.io/worldmet/reference/importNOAA.html)
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
[`importNOAA`](https://openair-project.github.io/worldmet/reference/importNOAA.html)
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
#> Error in parallel_pkgs_installed(): The package "carrier" (>= 0.3.0) is required for parallel map.
```
