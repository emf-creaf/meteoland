\name{reshapeworldmet}
\alias{reshapeweathercan}
\alias{reshapeworldmet}
\title{
Reshapes data from 'worldmet' or 'weathercan'
}
\description{
Reshapes weather station data acquired using the 'worldmet' or 'weathercan' R packages into formats useful for meteoland
}
\usage{
reshapeworldmet(hourly_data, output = "SpatialPointsMeteorology", 
                proj4string = NULL, complete = TRUE,
                verbose = TRUE)
reshapeweathercan(hourly_data, daily_data = NULL, output = "SpatialPointsMeteorology",
                  proj4string = NULL, complete = TRUE, verbose = TRUE)
}
\arguments{
  \item{hourly_data}{
     Hourly weather data. In the case of \code{reshapeworldmet}, a tibble or data frame returned by function 'importNOAA'. In the case of \code{reshapeweathercan}, a tibble or data frame returned by function 'weather_dl' with 'interval="hour"'.
  }
  \item{daily_data}{
     Daily weather data (only for \code{reshapeweathercan}), a tibble or data frame returned by function 'weather_dl' with 'interval="day"'.
  }
  \item{output}{
     Kind of output desired. Either '\code{\link{SpatialPointsTopography}}', '\code{\link{SpatialPointsMeteorology}}' or '\code{\link{MeteorologyInterpolationData}}'.
  }
  \item{proj4string}{
     A string or CRS to change the coordinate reference system of the output. If NULL the spatial reference will be geographic coordinates (i.e. \code{CRS("+proj=longlat")}). When reshaping to \code{MeteorologyInterpolationData} it is recommended to use a reference system with meters in units, such as UTM.
  }
  \item{complete}{ A flag to indicate that missing variables should be completed using function \code{\link{meteocomplete}}}
  \item{verbose}{A flag to show information of the reshape process in the console output.}
}
\value{
An object of the class indicated in \code{output}.
}
\details{
Note that to have precipitation included in downloads from 'worldmet' you should set 'precip = TRUE' when calling function 'importNOAA'. In the case of weathercan, precipitation is only provided for daily data (i.e. setting 'interval="day"' when calling 'weather_dl'), whereas wind speed and relative humidity are only availave for hourly data (i.e., setting 'interval="hour"' when calling 'weather_dl'). Hence, in \code{meteoland} we recommend downloading both daily and hourly data and then calling function \code{reshapeweathercan} to merge the two sources.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}
\seealso{
\code{\link{meteocomplete}}}
