\encoding{UTF-8}
\name{summarypoints}
\alias{summarypoints}
\alias{summarygrid}
\alias{summarypixels}
\title{
Summaries of meteorological data
}
\description{
Summarizes the meteorology of a set of spatial points or a set of grids 
}
\usage{
summarypoints(points, var, fun = mean, freq = NULL, dates = NULL, months = NULL, ...)
summarygrid(griddata, dates = NULL)
summarypixels(pixelsdata, dates = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points}{
    An object of class \code{\link{SpatialPointsMeteorology-class}} with the coordinates and meteorological data of the locations for which summaries are desired. Alternatively, an object of class \code{\link{SpatialPointsDataFrame-class}} containing the meta data (columns \code{dir},  \code{filename} and possibly \code{format}) of meteorological files that will be sequentially read from the disk.
  }
  \item{var}{The name of the meteorological variable to be summarized.}
  \item{fun}{The function to be calculated on values of each point. If \code{freq} is specified, the function will be calculated by intervals.}
  \item{freq}{
     A string giving an interval specification (e.g., \code{"week"}, \code{"month"}, \code{"quarter"} or \code{"year"}). If \code{NULL} then no intervals are defined.
   }
  \item{dates}{
An object of class \code{\link{Date}} to define the period to be summarized. If \code{dates = NULL} then all dates in \code{points} are processed.
 }
 \item{months}{
  A numeric vector to indicate the subset of months for which summary is desired (e.g. \code{c(7,8)} for July and August). This parameter allows studing particular seasons, when combined with \code{freq}. For example \code{freq = "years"} and \code{months = 6:8} leads to summarizing summer months of each year.
 }
  \item{\dots}{
Additional parameters to \code{fun}.
}
\item{griddata}{An object of class \code{\link{SpatialGridMeteorology-class}} with the meteorological data for a grid. Alternatively, a \code{\link{data.frame}} containing the meta data (columns \code{dir} and \code{filename}) of grid meteorological files that will be sequentially read from the disk.}
\item{pixelsdata}{An object of class \code{\link{SpatialPixelsMeteorology-class}} with the meteorological data for a set of grid cells. Alternatively, a \code{\link{data.frame}} containing the meta data (columns \code{dir} and \code{filename}) of grid meteorological files that will be sequentially read from the disk.}
}
\value{
Function \code{summarypoints} returns an object of class \code{\link{SpatialPointsDataFrame}} containing summaries (either one variable or several if \code{freq} is specified). Functions \code{summarygrid} and \code{summarypixels} return an object of class \code{\link{SpatialGridDataFrame}} and \code{\link{SpatialPixelsDataFrame}}, respectively, containing the summary of all meteorogical variables (sums or precipitation and PET; means for the other variables) calculated over the specified period. 
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya

Antoine Cabon, Centre Tecnologic Forestal de Catalunya
}
\seealso{
\code{\link{SpatialPointsMeteorology-class}}
}
\examples{
data(examplegridtopography)
data(exampleinterpolationdata)

#Creates spatial topography points from the grid
p = 1:2
points = as(examplegridtopography,"SpatialPoints")[p]
points = spTransform(points, exampleinterpolationdata@proj4string)
spt = SpatialPointsTopography(points, examplegridtopography$elevation[p],
                              examplegridtopography$slope[p],
                              examplegridtopography$aspect[p])

#Interpolation of two points for the whole time period (2000-2003)
mp = interpolationpoints(exampleinterpolationdata, spt)

#PET sums by months
mp.sum = summarypoints(mp, var="PET", freq="months")

mp.sum
}