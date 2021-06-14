\encoding{UTF-8}
\name{precipitation}
\alias{precipitation_concentration}
\alias{precipitation_rainfallErosivity}
\title{
   Precipitation utils
}
\description{
Function \code{precipitation_concentration()} calculates daily precipitation concentration (Martin-Vide et al. 2004). Function \code{precipitation_rainfallErosivity()} calculates a multi-year average of monthly rainfall erosivity using the MedREM model proposed by Diodato and Bellochi (2010) for the Mediterranean area (see also Guerra et al. 2016).
}
\usage{
precipitation_concentration(p)
precipitation_rainfallErosivity(x, long, scale = "month", average = TRUE)
}
\arguments{
  \item{p}{A numeric vector with daily precipitation values.}
  \item{x}{A data frame of meteorological variables (dates as row names and variables as columns).}
  \item{long}{Longitude in degrees.}
  \item{scale}{Either 'month' or 'year'.}
  \item{average}{Boolean flag to calculate multi-year averages before applying MedREM's formula.}
}
\value{
Function \code{precipitation_concentration()}  returns a value between 0 (equal distribution of rainfall) and 1 (one day concentrates all rainfall). Function \code{precipitation_rainfallErosivity()} returns a vector of twelve values (one for each month) (in MJ·mm·ha-1·h-1·month-1) or one value (in in MJ·mm·ha-1·h-1·yr-1) depending on the scale.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
}
\details{
MedREM model is: Rm = b0·P·sqrt(d)·(alpha + b1*longitude), where P is cumulated precipitation and d is maximum daily precipitation. Parameters used for the MedREM model are b0 = 0.117, b1 = -0.015, alpha = 2. Note that there is a mistake in Guerra et al. (2016) regarding parameters b1 and a.
}
\references{
Diodato, N., Bellocchi, G., 2010. MedREM, a rainfall erosivity model for the Mediter-ranean region. J. Hydrol. 387, 119–127, doi:10.1016/j.jhydrol.2010.04.003.

Guerra CA, Maes J, Geijzendorffer I, Metzger MJ (2016) An assessment of soil erosion prevention by vegetation in Mediterranean Europe: Current trends of ecosystem service provision. Ecol Indic 60:213–222. doi: 10.1016/j.ecolind.2015.06.043.

Martin-Vide J (2004) Spatial distribution of a daily precipitation concentration index in peninsular Spain. International Journal of Climatology 24, 959–971. doi:10.1002/joc.1030.
}

