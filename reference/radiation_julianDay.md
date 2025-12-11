# Solar radiation utility functions

Set of functions used in the calculation of incoming solar radiation and
net radiation.

## Usage

``` r
radiation_julianDay(year, month, day)

radiation_dateStringToJulianDays(dateStrings)

radiation_solarDeclination(J)

radiation_solarConstant(J)

radiation_sunRiseSet(latrad, slorad, asprad, delta)

radiation_solarElevation(latrad, delta, hrad)

radiation_daylength(latrad, slorad, asprad, delta)

radiation_daylengthseconds(latrad, slorad, asprad, delta)

radiation_potentialRadiation(solarConstant, latrad, slorad, asprad, delta)

radiation_solarRadiation(
  solarConstant,
  latrad,
  elevation,
  slorad,
  asprad,
  delta,
  diffTemp,
  diffTempMonth,
  vpa,
  precipitation
)

radiation_directDiffuseInstant(
  solarConstant,
  latrad,
  slorad,
  asprad,
  delta,
  hrad,
  R_s,
  clearday
)

radiation_directDiffuseDay(
  solarConstant,
  latrad,
  slorad,
  asprad,
  delta,
  R_s,
  clearday,
  nsteps = 24L
)

radiation_skyLongwaveRadiation(Tair, vpa, c = 0)

radiation_outgoingLongwaveRadiation(
  solarConstant,
  latrad,
  elevation,
  slorad,
  asprad,
  delta,
  vpa,
  tmin,
  tmax,
  R_s
)

radiation_netRadiation(
  solarConstant,
  latrad,
  elevation,
  slorad,
  asprad,
  delta,
  vpa,
  tmin,
  tmax,
  R_s,
  alpha = 0.08
)
```

## Arguments

- year, month, day:

  Year, month and day as integers.

- dateStrings:

  A character vector with dates in format "YYYY-MM-DD".

- J:

  Julian day (integer), number of days since January 1, 4713 BCE at noon
  UTC.

- latrad:

  Latitude (in radians North).

- slorad:

  Slope (in radians).

- asprad:

  Aspect (in radians from North).

- delta:

  Solar declination (in radians).

- hrad:

  Solar hour (in radians).

- solarConstant:

  Solar constant (in kW·m-2).

- elevation:

  Elevation above sea level (in m).

- diffTemp:

  Difference between maximum and minimum temperature (ºC).

- diffTempMonth:

  Difference between maximum and minimum temperature, averaged over 30
  days (ºC).

- vpa:

  Average daily vapor pressure (kPa).

- precipitation:

  Precipitation (in mm).

- R_s:

  Daily incident solar radiation (MJ·m-2).

- clearday:

  Boolean flag to indicate a clearsky day (vs. overcast).

- nsteps:

  Number of daily substeps.

- Tair:

  Air temperature (in degrees Celsius).

- c:

  Proportion of sky covered by clouds (0-1).

- tmin, tmax:

  Minimum and maximum daily temperature (ºC).

- alpha:

  Surface albedo (from 0 to 1).

## Value

Values returned for each function are:

- `radiation_dateStringToJulianDays`: A vector of Julian days (i.e.
  number of days since January 1, 4713 BCE at noon UTC).

- `radiation_daylength`: Day length (in hours).

- `radiation_daylengthseconds`: Day length (in seconds).

- `radiation_directDiffuseInstant`: A vector with instantaneous direct
  and diffusive radiation rates (for both SWR and PAR).

- `radiation_directDiffuseDay`: A data frame with instantaneous direct
  and diffusive radiation rates (for both SWR and PAR) for each subdaily
  time step.

- `radiation_potentialRadiation`: Daily (potential) solar radiation (in
  MJ·m-2).

- `radiation_julianDay`: Number of days since January 1, 4713 BCE at
  noon UTC.

- `radiation_skyLongwaveRadiation`: Instantaneous incoming (sky)
  longwave radiation (W·m-2).

- `radiation_outgoingLongwaveRadiation`: Daily outgoing longwave
  radiation (MJ·m-2·day-1).

- `radiation_netRadiation`: Daily net solar radiation (MJ·m-2·day-1).

- `radiation_solarConstant`: Solar constant (in kW·m-2).

- `radiation_solarDeclination`: Solar declination (in radians).

- `radiation_solarElevation`: Angle of elevation of the sun with respect
  to the horizon (in radians).

- `radiation_solarRadiation`: Daily incident solar radiation
  (MJ·m-2·day-1).

- `radiation_sunRiseSet`: Sunrise and sunset hours in hour angle
  (radians).

## Functions

- `radiation_dateStringToJulianDays()`: Date string to julian days

- `radiation_solarDeclination()`: solar declination

- `radiation_solarConstant()`: solar constant

- `radiation_sunRiseSet()`: sun rise and set

- `radiation_solarElevation()`: solar elevation

- `radiation_daylength()`: Day length

- `radiation_daylengthseconds()`: Day length seconds

- `radiation_potentialRadiation()`: Potential radiation

- `radiation_solarRadiation()`: solar Radiation

- `radiation_directDiffuseInstant()`: Direct diffuse instant

- `radiation_directDiffuseDay()`: Direct diffuse day

- `radiation_skyLongwaveRadiation()`: Sky longwave radiation

- `radiation_outgoingLongwaveRadiation()`: Outgoing longwave radiation

- `radiation_netRadiation()`: Net radiation

## Note

Code for `radiation_julianDay()`, `radiation_solarConstant()` and
`radiation_solarDeclination()` was translated to C++ from R code in
package 'insol' (by J. G. Corripio).

## References

Danby, J. M. Eqn. 6.16.4 in Fundamentals of Celestial Mechanics, 2nd ed.
Richmond, VA: Willmann-Bell, p. 207, 1988.

Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct
shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800

McMahon, T. A., M. C. Peel, L. Lowe, R. Srikanthan, and T. R. McVicar.
2013. Estimating actual, potential, reference crop and pan evaporation
using standard meteorological data: a pragmatic synthesis. Hydrology &
Earth System Sciences 17:1331–1363. See also:
http://www.fao.org/docrep/x0490e/x0490e06.htm.

Reda, I. and Andreas, A. 2003. Solar Position Algorithm for Solar
Radiation Applications. 55 pp.; NREL Report No. TP-560-34302, Revised
January 2008. http://www.nrel.gov/docs/fy08osti/34302.pdf

Spitters, C.J.T., Toussaint, H.A.J.M. and Goudriaan, J. (1986).
Separating the diffuse and direct components of global radiation and its
implications for modeling canopy photosynthesis. I. Components of
incoming radiation. Agricultural and Forest Meteorology, 38, 231–242.

## See also

[`interpolate_data`](https://emf-creaf.github.io/meteoland/reference/interpolate_data.md)

## Author

Miquel De Cáceres Ainsa, CREAF
