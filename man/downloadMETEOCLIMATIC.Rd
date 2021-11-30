\encoding{UTF-8}
\name{Meteoclimatic download}
\alias{downloadMETEOCLIMATICstationlist}
\alias{downloadMETEOCLIMATICcurrentday}

\title{
Download data from Meteoclimatic network
}
\description{
DEPRECATED. Download data from the Spanish Automatic Stations Network (non-professional)
}
\usage{
downloadMETEOCLIMATICstationlist(station_id = "ESCAT")
downloadMETEOCLIMATICcurrentday(station_id = "ESCAT")
}
\arguments{
  \item{station_id}{A string vector containing station ids (the list of stations for which current day climatic data is available is given by \code{downloadMETEOCLIMATICstationlist}). By default, returns the list of stations in Catalonia.}
}
\details{
Meteoclimatic is a non-professional automatic stations network, maintained by volunteers that share the data from their climatic stations. Data offered by these stations has not passed any quality control.
}
\value{
Function \code{downloadMETEOCLIMATICstationlist} returns a \code{\link{SpatialPointsDataFrame-class}} object containing the list of Meteoclimatic weather stations for which data is available based on the station_id codes provided.

Function \code{downloadMETEOCLIMATICcurrentday} downloads recent weather (for the current day) from all currently available stations and returns a \code{\link{SpatialPointsDataFrame-class}} object with observations. Only acummulated precipitation, maximum and minimum temperature and relative humidity are returned. 
}
\note{
Since ver. 1.0.1, weather data download functions included in \code{meteoland} make internal calls to functions in package \code{meteospain}. For an enhanced flexibility, users are recommended to call functions in \code{meteospain} themselves, and then to use function \code{\link{reshapemeteospain}} to generate data suitable for \code{meteoland}. 
}
\references{
Meteoclimatic should be acknowledged as author of information when using this data.
}
\author{
Víctor Granda, EMF-CREAF

Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
}

\seealso{
\code{\link{SpatialPointsMeteorology-class}}
}
