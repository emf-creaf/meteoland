\encoding{UTF-8}
\name{summarypoints}
\alias{summarypoint}
\alias{summarypoints}
\alias{summarygrid}
\alias{summarypixels}
\alias{summaryinterpolationdata}
\title{
Summaries of meteorological data
}
\description{
Summarizes the meteorology of a single location, a set of spatial points, pixels in a grid, or weather stations of interpolation data.
}
\usage{
summarypoint(x, var, fun = mean, freq = NULL, dates = NULL, months = NULL, ...)
summarypoints(points, var, fun = mean, freq = NULL, dates = NULL, months = NULL, ...)
summarygrid(grid, var, fun = mean, freq = NULL, dates = NULL, months = NULL, ...)
summarypixels(pixels, var, fun = mean, freq = NULL, dates = NULL, months = NULL, ...)
summaryinterpolationdata(object, var, fun = mean, freq = NULL, dates = NULL, 
                         months = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame with dates in rows and meteorological variables in columns.}
  \item{points}{
    An object of class \code{\link{SpatialPointsMeteorology-class}} with the coordinates and meteorological data of the locations for which summaries are desired. Alternatively, an object of class \code{\link{SpatialPointsDataFrame-class}} containing the meta data (columns \code{dir},  \code{filename} and possibly \code{format}) of meteorological files that will be sequentially read from the disk. Finally, \code{points} can also be a string pointing to a netCDF.
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
  \item{grid}{An object of class \code{\link{SpatialGridMeteorology-class}} with the meteorological data for a grid, or a string pointing to a NetCDF.}
  \item{pixels}{An object of class \code{\link{SpatialPixelsMeteorology-class}} with the meteorological data for  grid pixels, or a string pointing to a NetCDF.}
  \item{object}{An object of class \code{\link{MeteorologyInterpolationData-class}}.}
}
\details{
If \code{var="ALL"} then function \code{summarypoints} produces a summary of all variables with default statistics and returns an object of class \code{SpatialPointsMeteorology}.
}
\value{
\itemize{
\item{Function \code{summarypoint} returns a named vector of values with dates as names.}
\item{Functions \code{summarypoints} and \code{summaryinterpolationdata} return an object of class \code{\link{SpatialPointsDataFrame}} containing summaries (either one variable or several if \code{freq} is specified).}
\item{Functions \code{summarygrid} and \code{summarypixels} return an object of class \code{\link{SpatialGridDataFrame}} and \code{\link{SpatialPixelsDataFrame}}, respectively, containing the summaries analogously to \code{summarypoints}. }
}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CTFC

Antoine Cabon, CTFC
}
\seealso{
\code{\link{SpatialPointsMeteorology-class}}
}
\examples{
data(examplegridtopography)
data(exampleinterpolationdata)

#Creates spatial topography points from the grid
p = 1:2
spt = as(examplegridtopography, "SpatialPointsTopography")[p]

#Interpolation of two points for the whole time period (2000-2003)
mp = interpolationpoints(exampleinterpolationdata, spt)

#PET sums by months
mp.sum = summarypoints(mp, var="PET", freq="months", fun=sum)

mp.sum
}