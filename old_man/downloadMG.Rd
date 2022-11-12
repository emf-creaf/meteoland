\encoding{UTF-8}
\name{MeteoGalicia download}
\alias{downloadMGstationlist}
\alias{downloadMGhistorical}
\alias{downloadMGcurrentday}

\title{
Download data from MeteoGalicia
}
\description{
DEPRECATED. Download data from the Galician Meterology Agency (MeteoGalicia)
}
\usage{
downloadMGstationlist()
downloadMGhistorical(date_from, date_to, station_id = NULL,  
                    verbose = TRUE)
downloadMGcurrentday(station_id = NULL, daily = TRUE, verbose = TRUE)
}
\arguments{
  \item{date_from, date_to}{Strings or objects of class \code{\link{Date}} specifying first and last date of the desired period.}
  \item{station_id}{A string vector containing station ids (the list of stations presently operative is given by \code{downloadMGstationlist}). If NULL all stations with available data are returned.}
  \item{verbose}{Boolean flag to print process information.}
  \item{daily}{Boolean flag. Are data to be returned at a daily or hourly scale?}
}
\details{
 See available data services of MeteoGalicia at https://www.meteogalicia.gal/web/RSS/rssIndex.action?request_locale=es.
}
\value{
Function \code{downloadMGstationlist} returns a \code{\link{SpatialPointsDataFrame-class}} object containing the list of MeteoGalicia weather stations currently operative. 
Function \code{downloadMGhistorical} downloads data for the specified MG weather stations (or all) and dates and returns a \code{\link{SpatialPointsMeteorology-class}} object with the downloaded meteorology for each station (point).

Function \code{downloadMGcurrentday} downloads recent weather data (the last 24h) from all currently available stations and returns data frame if \code{daily = FALSE} or a \code{\link{SpatialPointsDataFrame-class}} object with observations aggregated at the daily scale otherwise. 
}
\note{
Since ver. 1.0.1, weather data download functions included in \code{meteoland} make internal calls to functions in package \code{meteospain}. For an enhanced flexibility, users are recommended to call functions in \code{meteospain} themselves, and then to use function \code{\link{reshapemeteospain}} to generate data suitable for \code{meteoland}. 
}
\references{
MeteoGalicia (from the Conselleria de Medio Ambiente, Territorio e Vivenda of Xunta de Galicia) should be acknowledged as source of information when using this data.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

\seealso{
\code{\link{SpatialPointsMeteorology-class}}
}