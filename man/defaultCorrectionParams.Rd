\encoding{UTF-8}
\name{defaultCorrectionParams}
\alias{defaultCorrectionParams}
\title{
Default correction parameters
}
\description{
Returns a list with the default parameterization for staistical correction.
}
\usage{
defaultCorrectionParams()
}
\value{
A list with the following items (default values in brackets):
\itemize{
\item{\code{methods}: A list with the correction method for each variable. Defaults are:
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
}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{\code{\link{MeteorologyInterpolationData}}}

