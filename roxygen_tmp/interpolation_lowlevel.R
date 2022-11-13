#' Low-level interpolation functions
#' 
#' Low-level functions to interpolate meteorology (one day) on a set of points.
#' 
#' 
#' @aliases interpolation_dewtemperature interpolation_temperature
#' interpolation_precipitation interpolation_wind
#' @param Xp,Yp,Zp Spatial coordinates and elevation (Zp; in m.a.s.l) of target
#' points.
#' @param X,Y,Z Spatial coordinates and elevation (Zp; in m.a.s.l) of reference
#' locations (e.g. meteorological stations).
#' @param T Temperature (e.g., minimum, maximum or dew temperature) at the
#' reference locations (in degrees).
#' @param P Precipitation at the reference locations (in mm).
#' @param Psmooth Temporally-smoothed precipitation at the reference locations
#' (in mm).
#' @param WS,WD Wind speed (in m/s) and wind direction (in degrees from north
#' clock-wise) at the reference locations.
#' @param iniRp Initial truncation radius.
#' @param iterations Number of station density iterations.
#' @param debug Boolean flag to show extra console output.
#' @param alpha,alpha_amount,alpha_event Gaussian shape parameter.
#' @param N,N_event,N_amount Average number of stations with non-zero weights.
#' @param popcrit Critical precipitation occurrence parameter.
#' @param fmax Maximum value for precipitation regression extrapolations (0.6
#' equals to a maximum of 4 times extrapolation).
#' @param directionsAvailable A flag to indicate that wind directions are
#' available (i.e. non-missing) at the reference locations.
#' @return All functions return a vector with interpolated values for the
#' target points.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{defaultInterpolationParams}}
#' @references Thornton, P.E., Running, S.W., White, M. A., 1997. Generating
#' surfaces of daily meteorological variables over large regions of complex
#' terrain. J. Hydrol. 190, 214–251. doi:10.1016/S0022-1694(96)03128-9.
#' 
#' De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating
#' daily meteorological data and downscaling climate models over landscapes.
#' Environmental Modelling and Software 108: 186-196.
#' @examples
#' 
#' data("exampleinterpolationdata")
#' mxt100 = exampleinterpolationdata@MaxTemperature[,100]
#' Psmooth100 = exampleinterpolationdata@SmoothedPrecipitation[,100]
#' P100 = exampleinterpolationdata@Precipitation[,100]
#' mismxt = is.na(mxt100)
#' misP = is.na(P100)
#' Z = exampleinterpolationdata@elevation
#' X = exampleinterpolationdata@coords[,1]
#' Y = exampleinterpolationdata@coords[,2]
#' Zpv = seq(0,1000, by=100)
#' xp = 360000
#' yp = 4640000
#' xpv = rep(xp, 11)
#' ypv = rep(yp, 11)
#' 
#' interpolation_temperature(xpv, ypv, Zpv, 
#'                           X[!mismxt], Y[!mismxt], Z[!mismxt], 
#'                           mxt100[!mismxt])
#' interpolation_precipitation(xpv, ypv, Zpv, 
#'                            X[!misP], Y[!misP], Z[!misP], 
#'                            P100[!misP], Psmooth100[!misP])
#' 