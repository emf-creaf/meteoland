\name{parseworldmet}
\alias{parseweathercan}
\alias{parseworldmet}
\title{
Parse data from worldmet or weathercan
}
\description{
Shapes weather station data read using the 'worldmet' or 'weathercan' R packages into formats useful for meteoland
}
\usage{
parseworldmet(hourly_data, output = "SpatialPointsMeteorology", complete = TRUE, verbose = TRUE)
parseweathercan(hourly_data, daily_data = NULL, output = "SpatialPointsMeteorology", complete = TRUE, verbose = TRUE)
}
\arguments{
  \item{hourly_data}{
     Hourly weather data. In the case of \code{parseworldmet}, a tibble or data frame returned by function 'importNOAA'. In the case of \code{parseweathercan}, a tibble or data frame returned by function 'weather_dl' with 'interval="hour"'.
  }
  \item{daily_data}{
     Daily weather data (only for \code{parseweathercan}), a tibble or data frame returned by function 'weather_dl' with 'interval="day"'.
  }
  \item{output}{
     Kind of output desired. Either '\code{\link{SpatialPointsTopography}}', '\code{\link{SpatialPointsMeteorology}}' or '\code{\link{MeteorologyInterpolationData}}'.
  }
  \item{complete}{ A flag to indicate that missing variables should be completed using function \code{\link{meteocomplete}}}
  \item{verbose}{A flag to show information of the parse process in the console output.}
}
\value{
An object of the class indicated in \code{output}.
}
\details{
Note that to have precipitation included in downloads from 'worldmet' you should set 'precip = TRUE' when calling function 'importNOAA'. In the case of weathercan, precipitation is only provided for daily data (i.e. setting 'interval="day"' when calling 'weather_dl'), whereas wind speed and relative humidity are only availave for hourly data (i.e., setting 'interval="hour"' when calling 'weather_dl'). Hence, in \code{meteoland} we recommend downloading both daily and hourly data and then calling function \code{parseweathercan} to merge the two sources.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}
\seealso{
\code{\link{meteocomplete}}}
