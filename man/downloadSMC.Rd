\encoding{UTF-8}
\name{SMC download}
\alias{downloadSMCvarmetadata}
\alias{downloadSMCcurrentday}

\title{
Download data from SMC
}
\description{
Download data from the Catalan weather station network (XEMA from Servei Meteorològic de Catalunya) 
}
\usage{
downloadSMCvarmetadata(api)
downloadSMCcurrentday(api, daily = TRUE, variable_code=NULL,
                      station_id=NULL, date = Sys.Date(), verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api}{String with the SMC API key (see https://apidocs.meteocat.gencat.cat/.)}
  \item{daily}{Boolean flag. Are data to be returned at a daily or hourly scale?. When \code{daily = TRUE} all the usual variables in meteoland are downloaded. When \code{daily = FALSE}, the user can choose the desired variables using \code{variable_code}. }
  \item{variable_code}{A character vector with variable codes to be queried.}
  \item{date}{An object of class \code{\link{Date}}. Normally the current day.}
  \item{station_id}{A string vector containing station ids (the list of stations for which historical climatic series are available is given by ...).}
  \item{verbose}{Boolean flag to print process information.}
}
\details{
API key needs to be requested from SMC (https://apidocs.meteocat.gencat.cat/).
}
\value{
Function \code{downloadSMCvarmetadata} returns a data frame with weatehr variables , theri units and acronym to be used in queries (see parameter \code{variable_code}).
Function \code{downloadSMCcurrentday} downloads recent weather (the last 24h) from all currently available stations and returns data frame if \code{daily = FALSE} or a \code{\link{SpatialPointsDataFrame-class}} object with observations aggregated at the daily scale otherwise. 
}
\references{
Servei Meteorològic de Catalunya (SMC) should be acknowledged as author of information when accessing weather data with these functions.
}
\author{
Antoine Cabon, CTFC

Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}

\seealso{
\code{\link{SpatialPointsMeteorology-class}}
}
