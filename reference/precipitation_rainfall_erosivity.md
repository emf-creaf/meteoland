# Precipitation rainfall erosivity

**\[experimental\]**

Function `precipitation_rainfall_erosivity()` calculates a multi-year
average of monthly rainfall erosivity using the MedREM model proposed by
Diodato and Bellochi (2010) for the Mediterranean area (see also Guerra
et al. 2016).

## Usage

``` r
precipitation_rainfall_erosivity(
  meteo_data,
  longitude,
  scale = c("month", "year"),
  average = TRUE
)
```

## Arguments

- meteo_data:

  A meteo tibble as with the dates and meteorological variables as
  returned by
  [`interpolate_data`](https://emf-creaf.github.io/meteoland/reference/interpolate_data.md)
  in the "interpolated_data" column.

- longitude:

  Longitude in degrees.

- scale:

  Character, either 'month' or 'year'. Default to 'month'

- average:

  Boolean flag to calculate multi-year averages before applying MedREM's
  formula.

## Value

A vector of values for each month (in MJ·mm·ha-1·h-1·month-1) or each
year (in MJ·mm·ha-1·h-1·yr-1), depending on the scale

## Details

MedREM model is: Rm = b0·P·sqrt(d)·(alpha + b1\*longitude), where P is
accumulated precipitation and d is maximum daily precipitation.
Parameters used for the MedREM model are b0 = 0.117, b1 = -0.015, alpha
= 2. Note that there is a mistake in Guerra et al. (2016) regarding
parameters b1 and a.

## References

Diodato, N., Bellocchi, G., 2010. MedREM, a rainfall erosivity model for
the Mediterranean region. J. Hydrol. 387, 119–127,
doi:10.1016/j.jhydrol.2010.04.003.

Guerra CA, Maes J, Geijzendorffer I, Metzger MJ (2016) An assessment of
soil erosion prevention by vegetation in Mediterranean Europe: Current
trends of ecosystem service provision. Ecol Indic 60:213–222. doi:
10.1016/j.ecolind.2015.06.043.

## Author

Miquel De Cáceres Ainsa, CREAF.

Víctor Granda García, CREAF.

## Examples

``` r
# \donttest{
interpolated_example <-
  interpolate_data(points_to_interpolate_example, meteoland_interpolator_example)
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

precipitation_rainfall_erosivity(
  meteo_data = interpolated_example$interpolated_data[[1]],
  longitude = 2.32,
  scale = "month",
  average = TRUE
)
#>        4 
#> 48.02902 
# }
```
