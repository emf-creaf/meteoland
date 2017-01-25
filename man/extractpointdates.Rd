\encoding{UTF-8}
\name{extractpointdates}
\alias{extractpointdates}
\title{
   Extracts meteorological data
}
\description{
Extracts meteorological data from an object.
}
\usage{
extractpointdates(points, dates = NULL)
}
\arguments{
  \item{points}{
An object of class \code{\link{SpatialPointsMeteorology}}. Alternatively, an object of class \code{\link{SpatialPointsDataFrame-class}} containing the meta data (columns \code{dir} and \code{filename}) of meteorological files that will be sequentially read from the disk.
}
  \item{dates}{A vector of \code{\link{Date}} with a (subset) of dates to be extracted. }
}
\value{
A \code{list} with the same lenght as \code{dates}. Each element of the list is an object of class \code{\link{SpatialPointsDataFrame}} with the meteorological data for all the points (in rows) and variables (in columns).
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya
}

