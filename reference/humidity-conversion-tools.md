# Humidity conversion tools

Functions to transform relative humidity to specific humidity or dew
point temperature and viceversa.

## Usage

``` r
humidity_relative2dewtemperature(Tc, HR)

humidity_dewtemperature2relative(Tc, Td, allowSaturated = FALSE)

humidity_specific2relative(Tc, HS, allowSaturated = FALSE)

humidity_relative2specific(Tc, HR)
```

## Arguments

- Tc:

  A numeric vector of temperature in degrees Celsius.

- HR:

  A numeric vector of relative humidity (in %).

- Td:

  A numeric vector of dew temperature in degrees Celsius.

- allowSaturated:

  Logical flag to allow values over 100%

- HS:

  A numeric vector of specific humidity (unitless).

## Value

A numeric vector with specific or relative humidity.

## See also

[`complete_meteo`](https://emf-creaf.github.io/meteoland/reference/complete_meteo.md)

## Author

Nicholas Martin-StPaul, INRA

Miquel De Cáceres Ainsa, CREAF
