\encoding{UTF-8}
\name{extractdates}
\alias{extractdates}
\alias{extractvars}
\alias{extractgridindex}
\alias{extractgridpoints}
\title{
   Extracts meteorological data
}
\description{
Extracts meteorological data from an object.
}
\usage{
extractdates(object, dates = NULL, verbose=FALSE)
extractvars(object, vars, verbose=FALSE)
extractgridindex(grid, index)
extractgridpoints(grid, points, verbose=FALSE)
}
\arguments{
  \item{object}{An object of class \code{\link{SpatialPointsMeteorology}}, \code{\link{SpatialGridMeteorology}} or \code{\link{SpatialPixelsMeteorology}}. 
}
  \item{dates}{A vector of \code{\link{Date}} with a (subset) of dates to be extracted. If \code{NULL} all dates will be returned. }
  \item{vars}{A character vector with the set of variables to be extracted.}
  \item{grid}{An object of class \code{\link{SpatialGridMeteorology-class}} or \code{\link{SpatialPixelsMeteorology-class}} with the meteorological data for a full grid or a subset of grid cells, respectively. Alternatively, a string specifying a NetCDF to be read from the disk.}
  \item{index}{An integer with a grid index.}
  \item{points}{An object of class \code{\link{SpatialPoints}}.}
  \item{verbose}{Boolean flag to print process information.}
}
\details{
Function \code{extractpoints} is deprecated, because its functionality can be achieved using subsetting of spatial classes \code{\link{SpatialGridMeteorology}} and \code{\link{SpatialPixelsMeteorology}}.
}
\value{
\itemize{
\item{Function \code{extractdates()}, returns a \code{list} with the same length as \code{dates}. Each element of the list is a spatial object (\code{\link{SpatialPointsDataFrame}}, \code{\link{SpatialGridDataFrame}} or \code{\link{SpatialPixelsDataFrame}}, depending on the input) with the meteorological data for all the spatial elements. If only one date is asked, the function returns directly the spatial object, without embedding it into a list.}
\item{Function \code{extractvars()}, returns a \code{list} with the same length as \code{vars}. Each element of the list is a spatial object (\code{\link{SpatialPointsDataFrame}}, \code{\link{SpatialGridDataFrame}} or \code{\link{SpatialPixelsDataFrame}}, depending on the input) with the meteorological data for all the spatial elements. If only one variable is asked, the function returns directly the spatial object, without embedding it into a list.}
\item{Function \code{extractgridindex()} returns a data frame.}
\item{Function \code{extractgridpoints()} returns an object of class \code{\link{SpatialPointsMeteorology}}.}
}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}

