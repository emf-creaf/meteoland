\encoding{UTF-8}
\name{writemeteorologypoint}
\alias{writemeteorologypoint}
\alias{writemeteorologypointfiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Writes point meteorology to the disk
}
\description{
Functions to write point meteorological data to ascii files in different formats.
}
\usage{
writemeteorologypoint(data, file, format = "meteoland")
writemeteorologypointfiles(object, dir=getwd(), format = "meteoland",
                           metadatafile="MP.txt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{An data frame with meteorological data.}
  \item{file}{A string with the file name to be written.}
  \item{format}{Output format of meteorological data. Current accepted formats are \code{"castanea"} or \code{"meteoland"}.}
  \item{object}{An object of class \code{\link{SpatialPointsMeteorology-class}} with the meteorological data to be written.}
  \item{dir}{Output directory for meteorology data.}
  \item{metadatafile}{The name of the file that will store the meta data describing all written files.}
}

\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya

Nicolas Martin, INRA-Avignon
}

\seealso{
\code{\link{readmeteorologypoint}}, \code{\link{SpatialPointsMeteorology-class}}
}
