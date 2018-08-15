\encoding{UTF-8}
\name{defaultInterpolationParams}
\alias{defaultInterpolationParams}
\title{
Default interpolation parameters
}
\description{
Returns a list with the default parameterization for interpolation. Most parameter values are set according to Thornton et al. (1997).
}
\usage{
defaultInterpolationParams()
}
\value{
A list with the following items (default values in brackets):
\itemize{
\item{\code{initial_Rp [= 140000]}: Initial truncation radius.}
\item{\code{iterations  [= 3]}: Number of station density iterations.}
\item{\code{alpha_MinTemperature  [= 3.0]}: Gaussian shape parameter for minimum temperature.}
\item{\code{alpha_MaxTemperature  [= 3.0]}: Gaussian shape parameter for maximum temperature.}
\item{\code{alpha_DewTemperature  [= 3.0]}: Gaussian shape parameter for dew-point temperature.}
\item{\code{alpha_PrecipitationEvent  [= 5.0]}: Gaussian shape parameter for precipitation events.}
\item{\code{alpha_PrecipitationAmount  [= 5.0]}: Gaussian shape parameter for the regression of precipitation amounts.}
\item{\code{alpha_Wind  [= 3.0]}: Gaussian shape parameter for wind.}
\item{\code{N_MinTemperature  [= 30]}: Average number of stations with non-zero weights for minimum temperature.}
\item{\code{N_MaxTemperature  [= 30]}: Average number of stations with non-zero weights for maximum temperature.}
\item{\code{N_DewTemperature  [= 30]}: Average number of stations with non-zero weights for dew-point temperature.}
\item{\code{N_PrecipitationEvent  [= 5]}: Average number of stations with non-zero weights for precipitation events.}
\item{\code{N_PrecipitationAmount  [= 20]}: Average number of stations with non-zero weights for the regression of precipitation amounts.}
\item{\code{N_Wind  [= 2]}: Average number of stations with non-zero weights for wind.}
\item{\code{St_Precipitation [= 5]}: Number of days for the temporal smoothing of precipitation.}
\item{\code{St_TemperatureRange [= 15]}: Number of days for the temporal smoothing of temperature range.}
\item{\code{pop_crit  [= 0.50]}: Critical precipitation occurrence parameter.}
\item{\code{f_max [= 0.6]}: Maximum value for precipitation regression extrapolations (0.6 equals to a maximum of 4 times extrapolation).}
\item{\code{wind_height  [= 10]}: Wind measurement height (in m).}
}
}
\references{
Thornton, P.E., Running, S.W., White, M. A., 1997. Generating surfaces of daily meteorological variables over large regions of complex terrain. J. Hydrol. 190, 214–251. doi:10.1016/S0022-1694(96)03128-9.

De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating daily meteorological data and downscaling climate models over landscapes. Environmental Modelling and Software 108: 186-196.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{\code{\link{MeteorologyInterpolationData}}}

