\encoding{UTF-8}
\name{Meteoclimatic download}
\alias{downloadMETEOCLIMATICstationlist}
\alias{downloadMETEOCLIMATICcurrentday}

\title{
Download data from Meteoclimatic network
}
\description{
Download data from the Spanish Automatic Stations Network (non-professional)
}
\usage{
downloadMETEOCLIMATICstationlist(station_id = "ESCAT", spatial = TRUE)
downloadMETEOCLIMATICcurrentday(station_id = "ESCAT")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{station_id}{A string vector containing station ids (the list of stations for which current day climatic data is available is given by \code{downloadMETEOCLIMATICstationlist}).}
  \item{spatial}{Returned object format. If \code{TRUE} (default), a \code{\link{SpatialPointsDataFrame-class}} object is returned. If \code{FALSE}, a data.frame is returned instead.}
}
\details{
Meteoclimatic is a non-professional automatic stations network, maintained by volunteers that share the data from their climatic stations. Data offered by these stations has not passed any quality control.
}
\value{
Function \code{downloadMETEOCLIMATICstationlist} returns a \code{\link{SpatialPointsDataFrame-class}} object containing the list of Meteoclimatic weather stations for which data is available based on the station_id codes provided, or a data.frame with the same information if \code{spatial = FALSE}.

Function \code{downloadMETEOCLIMATICcurrentday} downloads recent weather (for the current day) from all currently available stations and returns a \code{\link{SpatialPointsDataFrame-class}} object with observations. 
}
\note{
Only acummulated precipitation, maximum and minimum temperature and relative humidity are returned.
}
\references{
Meteoclimatic should be acknowledged as author of information when using this data.
}
\author{
Víctor Granda, EMF-CREAF

Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}

\seealso{
\code{\link{SpatialPointsMeteorology-class}}
}
