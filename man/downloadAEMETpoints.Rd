\encoding{UTF-8}
\name{AEMET download}
\alias{downloadAEMETstationdata}
\alias{downloadAEMETpoints}
\alias{downloadAEMETcurrentday}

\title{
Download data from AEMET
}
\description{
Download data from the Spanish National Meterology Agency (AEMET)
}
\usage{
downloadAEMETstationdata(api, station_id)
downloadAEMETpoints(api, dates, station_id, export = FALSE, exportDir = getwd(), 
                    exportFormat = "meteoland", metadatafile = "MP.txt", verbose = TRUE)
downloadAEMETcurrentday(api, station_id, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api}{String with the AEMET API key (see https://opendata.aemet.es/).}
  \item{dates}{An object of class \code{\link{Date}}.}
  \item{station_id}{A string vector with station ids (they can be known by calling to \code{downloadAEMETstationlist}).}
  \item{export}{If \code{export = FALSE} the downloaded data is stored in memory. Otherwise the result is written in the disk (using the format specified in \code{exportFormat}).}
  \item{exportDir}{Output directory for downloaded meteorology.}
  \item{exportFormat}{Format of meteorological data. Current accepted formats are \code{"castanea"} and \code{"meteoland"}.}
  \item{metadatafile}{The name of the file that will store the meta data describing all written files.}
  \item{verbose}{
Boolean flag to print process information.}
}
\details{
API keys need to be acquired from AEMET (https://opendata.aemet.es/)
}
\value{
Function \code{downloadAEMETstationdata} returns an object \code{\link{SpatialPointsDataFrame-class}} with information on the available weather stations (for historical climate retrieval using \code{downloadAEMETpoints}). If \code{export = FALSE}, function \code{downloadAEMETpoints} returns an object of class \code{\link{SpatialPointsMeteorology-class}} with the downloaded meteorology for each station (point). Otherwise the function returns an object of class \code{\link{SpatialPointsDataFrame-class}} containing the meta data of the files written in the disk. Function \code{downloadAEMETcurrentday} returns a \code{\link{SpatialPointsDataFrame-class}} with weather stations in rows and recent weather (averages of the last 24h) in columns. 
}
\note{
The list of stations available in \code{downloadAEMETcurrentday} (current observations) is different than the list given by \code{downloadAEMETstationdata} and available in \code{downloadAEMETpoints} (stations with historical climate series).
}
\references{
AEMET should be acknowledged as author of information when using this data.
}
\author{
Antoine Cabon, Centre Tecnologic Forestal de Catalunya

Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya
}

\seealso{
\code{\link{SpatialPointsMeteorology-class}}
}
