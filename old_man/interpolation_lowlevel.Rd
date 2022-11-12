\encoding{UTF-8}
\name{interpolation_lowlevel}
\alias{interpolation_dewtemperature}
\alias{interpolation_temperature}
\alias{interpolation_precipitation}
\alias{interpolation_wind}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Low-level interpolation functions
}
\description{
Low-level functions to interpolate meteorology (one day) on a set of points. 
}
\usage{
interpolation_dewtemperature(Xp, Yp, Zp, X, Y, Z, T, 
                             iniRp = 140000, alpha = 3.0, N = 30, 
                             iterations = 3, debug = FALSE)
interpolation_temperature(Xp, Yp, Zp, X, Y, Z, T, 
                          iniRp = 140000, alpha = 3.0, N = 30, 
                          iterations = 3, debug = FALSE)
interpolation_precipitation(Xp, Yp, Zp, X, Y, Z, P, Psmooth,
                            iniRp = 140000, alpha_event = 6.25, alpha_amount = 6.25,
                            N_event = 20, N_amount = 20, iterations = 3, popcrit = 0.5, 
                            fmax = 0.95, debug = FALSE)
interpolation_wind(Xp, Yp, WS, WD, X, Y,
                   iniRp = 140000, alpha = 2.0, N = 1, iterations = 3,
                   directionsAvailable = TRUE)
}

\arguments{
  \item{Xp, Yp, Zp}{Spatial coordinates and elevation (Zp; in m.a.s.l) of target points.}
  \item{X, Y, Z}{Spatial coordinates and elevation (Zp; in m.a.s.l) of reference locations (e.g. meteorological stations).}
  \item{T}{Temperature (e.g., minimum, maximum or dew temperature) at the reference locations (in degrees).}
  \item{P}{Precipitation at the reference locations (in mm).}
  \item{Psmooth}{Temporally-smoothed precipitation at the reference locations (in mm).}
  \item{WS, WD}{Wind speed (in m/s) and wind direction (in degrees from north clock-wise) at the reference locations.}
  \item{iniRp}{Initial truncation radius.}
  \item{iterations}{Number of station density iterations.}
  \item{debug}{Boolean flag to show extra console output.}
  \item{alpha, alpha_amount, alpha_event}{Gaussian shape parameter.}
  \item{N, N_event, N_amount}{Average number of stations with non-zero weights.}
  \item{popcrit}{Critical precipitation occurrence parameter.}
  \item{fmax}{Maximum value for precipitation regression extrapolations (0.6 equals to a maximum of 4 times extrapolation).}
  \item{directionsAvailable}{A flag to indicate that wind directions are available (i.e. non-missing) at the reference locations.}
}

\value{
All functions return a vector with interpolated values for the target points.
}
\references{
Thornton, P.E., Running, S.W., White, M. A., 1997. Generating surfaces of daily meteorological variables over large regions of complex terrain. J. Hydrol. 190, 214–251. doi:10.1016/S0022-1694(96)03128-9.

De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating daily meteorological data and downscaling climate models over landscapes. Environmental Modelling and Software 108: 186-196.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
\seealso{\code{\link{defaultInterpolationParams}}}
\examples{
data("exampleinterpolationdata")
mxt100 = exampleinterpolationdata@MaxTemperature[,100]
Psmooth100 = exampleinterpolationdata@SmoothedPrecipitation[,100]
P100 = exampleinterpolationdata@Precipitation[,100]
mismxt = is.na(mxt100)
misP = is.na(P100)
Z = exampleinterpolationdata@elevation
X = exampleinterpolationdata@coords[,1]
Y = exampleinterpolationdata@coords[,2]
Zpv = seq(0,1000, by=100)
xp = 360000
yp = 4640000
xpv = rep(xp, 11)
ypv = rep(yp, 11)

interpolation_temperature(xpv, ypv, Zpv, 
                          X[!mismxt], Y[!mismxt], Z[!mismxt], 
                          mxt100[!mismxt])
interpolation_precipitation(xpv, ypv, Zpv, 
                           X[!misP], Y[!misP], Z[!misP], 
                           P100[!misP], Psmooth100[!misP])
}