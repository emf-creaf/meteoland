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
#> Error in dplyr::as_tibble(httr2::resp_body_json(httr2::req_perform(meteogalicia_request),     flatten = TRUE, simplifyDataFrame = TRUE)): All columns in a tibble must be vectors.
#> ✖ Column `idTipo` is NULL.
```
