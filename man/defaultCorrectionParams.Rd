\encoding{UTF-8}
\name{defaultCorrectionParams}
\alias{defaultCorrectionParams}
\title{
Default correction parameters
}
\description{
Returns a list with the default parameterization for statistical correction.
}
\usage{
defaultCorrectionParams()
}
\value{
A list with the following items (default values in brackets):
\itemize{
\item{\code{methods}: A list with the correction method for each variable (options are "unbias" (shift of the mean), "scaling" (factor multiplication),  "quantmap" (empirical quantile mapping) or "none" (for no correction)). Defaults are:
\itemize{
\item{\code{MeanTemperature = "unbias"}}
\item{\code{MinTemperature = "quantmap"}}
\item{\code{MaxTemperature = "quantmap"}}
\item{\code{Precipitation = "quantmap"}}
\item{\code{MeanRelativeHumidity = "unbias"}}
\item{\code{Radiation = "unbias"}}
\item{\code{WindSpeed = "quantmap"}}
}
}
\item{\code{fill_wind [= TRUE]}: A logical flag to fill wind speed values with uncorrected values when reference data is missing.}
\item{\code{allow_saturated [= FALSE]}: A logical flag to indicate whether relative humidity values above saturation (>100\%) are permitted (bias correction is performed on specific humidity).}
\item{\code{wind_height  [= 10]}: Wind measurement height (in m).}
\item{\code{qstep [= 0.01]}: a numeric value between 0 and 1. Quantile mapping is fitted only for the quantiles defined by \code{quantile(0,1,probs=seq(0,1,by=qstep)}.}
}
}
\references{
De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating daily meteorological data and downscaling climate models over landscapes. Environmental Modelling and Software 108: 186-196.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}
\seealso{\code{\link{MeteorologyInterpolationData}}, \code{\link{MeteorologyUncorrectedData}}}

