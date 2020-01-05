\encoding{UTF-8}
\name{SMC download}
\alias{downloadSMCvarmetadata}
\alias{downloadSMCcurrentday}
\alias{downloadSMCstationlist}
\alias{downloadSMChistorical}

\title{
Download data from SMC
}
\description{
Download data from the Catalan automatic weather station network (XEMA from Servei \enc{Meteorològic}{Meteorologic} de Catalunya) 
}
\usage{
downloadSMCvarmetadata(api, type = "current")
downloadSMCstationlist(api, date = NULL)
downloadSMCcurrentday(api, daily_meteoland = TRUE, variable_code=NULL,
                      station_id=NULL, date = Sys.Date(), verbose=TRUE)
downloadSMChistorical(api, dates, station_id=NULL, variable_code=NULL, export = FALSE, 
                      exportDir = getwd(), exportFormat = "meteoland/txt",
                      metadataFile = "MP.txt", verbose=TRUE)                      
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api}{String with the SMC API key (the procedure to apply for an api key is explained in https://apidocs.meteocat.gencat.cat/ ).}
  \item{type}{Either 'current' or 'historical' for current day variable metadata or historical variable metadata, respectively.}
  \item{daily_meteoland}{When \code{daily_meteoland = TRUE} all the usual variables in meteoland are downloaded and aggregated at the daily scale. When \code{daily_meteoland = FALSE}, the user can choose the desired variables using \code{variable_code}. }
  \item{variable_code}{A character vector with variable codes to be queried. If \code{NULL} in \code{downloadSMChistorical} all the usual variables for meteoland are downloaded. Otherwise, the results are returned as a dara frame.}
  \item{dates}{An object of class \code{\link{Date}} with dates comprised within the last two years.}
  \item{date}{An object of class \code{\link{Date}}. By default the current day in the case of \code{downloadSMCcurrentday()}. In the case of \code{downloadSMCstationlist()} a date for which operational stations are queried.}
  \item{station_id}{A string vector containing station ids (the list of stations for which climatic series are available is given by \code{downloadSMCstationlist()}). If \code{NULL}, all available stations are queried. Otherwise, only the data corresponding to the specified stations will be returned.}
  \item{export}{If \code{export = FALSE} the downloaded data is stored in memory. Otherwise the result is written on the disk (using the format specified in \code{exportFormat}).}
  \item{exportDir}{Output directory for downloaded meteorology.}
  \item{exportFormat}{Format of meteorological data. Current accepted formats are \code{"castanea"} and \code{"meteoland"}.}
  \item{metadataFile}{The name of the file that will store the meta data describing all written files.}
  \item{verbose}{Boolean flag to print process information.}
}
\details{
API key needs to be requested from SMC (https://apidocs.meteocat.gencat.cat/).
}
\value{
Function \code{downloadSMCstationlist} returns a \code{\link{SpatialPointsDataFrame-class}} object containing the list of SMC operational weather stations for the date given.

Function \code{downloadSMCvarmetadata} returns a data frame with weather variables, their units and acronym to be used in queries (see parameter \code{variable_code}).

Function \code{downloadSMCcurrentday} downloads recent weather (the last 24h or the weather for a given date) from all currently available stations and returns data frame if \code{daily_meteoland = FALSE} or a \code{\link{SpatialPointsDataFrame-class}} object with observations aggregated at the daily scale otherwise. 

Function \code{downloadSMChistorical} downloads historical daily weather corresponding to a given time period from a set (or all currently available) stations. Results are returned (or exported) after formatting data as a \code{\link{SpatialPointsMeteorology-class}} if \code{variable_code = NULL}, or as a data frame otherwise.
}
\references{
Servei \enc{Meteorològic}{Meteorologic} de Catalunya (SMC) should be acknowledged as author of information when accessing weather data with these functions.
}
\author{
Antoine Cabon, CTFC

Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}

\seealso{
\code{\link{SpatialPointsMeteorology-class}}
}
