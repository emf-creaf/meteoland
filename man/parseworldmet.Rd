\name{parseworldmet}
\alias{parseworldmet}
\title{
Parse data from worldmet
}
\description{
Shapes weather station data read using the worldmet package into formats useful for meteoland
}
\usage{
parseworldmet(x, output = "SpatialPointsMeteorology", complete = TRUE, verbose = TRUE)
}
\arguments{
  \item{x}{
     Tibble or data frame returned by function 'importNOAA' from package 'worldmet'.
  }
  \item{output}{
     Kind of output desired. Either '\code{\link{SpatialPointsTopography}}', '\code{\link{SpatialPointsMeteorology}}' or '\code{\link{MeteorologyInterpolationData}}'.
  }
  \item{complete}{ A flag to indicate that missing variables should be completed using function \code{\link{meteocomplete}}}
  \item{verbose}{A flag to show information of the parse process in the console output.}
}
\value{
An object of the class indicated in \code{output}.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC
}
\seealso{
\code{\link{meteocomplete}}}
