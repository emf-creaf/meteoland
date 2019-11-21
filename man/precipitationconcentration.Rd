\encoding{UTF-8}
\name{precipitation}
\alias{precipitation_concentration}
\alias{precipitation_rainfallErosivity}
\title{
   Precipitation utils
}
\description{
Function \code{precipitation_concentration()} calculates daily precipitation concentration (Martin-Vide et al. 2004). Function \code{precipitation_rainfallErosivity()} calculates a multi-year average of monthly rainfall erosivity using the MedREM model proposed by Diodato and Bellochi (2010) for the Mediterranean area.
}
\usage{
precipitation_concentration(p)
precipitation_rainfallErosivity(x, long)
}
\arguments{
  \item{p}{A numeric vector with daily precipitation values.}
  \item{x}{A data frame of meteorological variables (dates as row names and variables as columns).}
  \item{long}{Longitude in degrees.}
}
\value{
Function \code{precipitation_concentration()}  returns a value between 0 (equal distribution of rainfall) and 1 (one day concentrates all rainfall). Function \code{precipitation_rainfallErosivity()} returns a vector of twelve values (one for each month) of rainfall erosivity (in MJ·mm·ha-1·h-1·yr-1).
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC.
}
\references{
Diodato, N., Bellocchi, G., 2010. MedREM, a rainfall erosivity model for the Mediter-ranean region. J. Hydrol. 387, 119–127, doi:10.1016/j.jhydrol.2010.04.003.

Martin-Vide J (2004) Spatial distribution of a daily precipitation concentration index in peninsular Spain. International Journal of Climatology 24, 959–971. doi:10.1002/joc.1030.
}

