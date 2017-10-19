\encoding{UTF-8}
\name{readmeteorologypoint}
\alias{readmeteorologypoint}
\alias{readmeteorologypointfiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads point meteorology from the disk
}
\description{
Functions to read point meteorological data from ascii files in different formats.
}
\usage{
readmeteorologypoint(file, dates = NULL, format = "meteoland/txt", sep="\t")
readmeteorologypointfiles(points, files=NULL, dates = NULL, format="meteoland/txt", 
                          sep="\t")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{A string of the file to be read.}
  \item{points}{An object of class \code{\link{SpatialPoints-class}} (in this case \code{files} cannot be \code{NULL}) or object of class \code{\link{SpatialPointsDataFrame-class}} with two data columns: '\code{dir}' and '\code{filename}' (and possibly '\code{format}').}
  \item{files}{A vector of strings to be read (when \code{points} is of class \code{\link{SpatialPoints-class}}). Length and order must match \code{points}.}
  \item{dates}{Object of class \code{"Date"} describing a subset of dates to be extracted from meteorological series. If \code{NULL} the whole period read from files is kept.}
  \item{format}{Format of meteorological data. Current accepted formats are \code{"meteoland/txt"},  \code{"meteoland/rds"}, \code{"castanea/txt"} and \code{"castanea/rds"}.}
  \item{sep}{The field separator character for ascii text files (see \code{\link{read.table}}).}
}
\details{
In \code{readmeteorologypointfiles} the value of \code{format} is used as default but can be overloaded if \code{points} includes a column '\code{format}'.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya

Nicolas Martin, INRA-Avignon
}

\seealso{
\code{\link{writemeteorologypoint}}, \code{\link{read.table}}, \code{\link{SpatialPointsMeteorology-class}}
}
