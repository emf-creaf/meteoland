\encoding{UTF-8}
\name{meteoplot}
\alias{meteoplot}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots meteorological series
}
\description{
Simple plotting of meteorological series from an object of class \code{\link{SpatialPointsMeteorology-class}} or \code{\link{SpatialGridMeteorology-class}}.
}
\usage{
meteoplot(object, index, var="MeanTemperature", 
          fun=NULL, freq=NULL, dates = NULL, months = NULL, add = FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{SpatialPointsMeteorology-class}} or \code{\link{SpatialGridMeteorology-class}}. Alternatively, an object of class \code{\link{SpatialPointsDataFrame-class}} containing the meta data (columns \code{dir} and \code{filename}) of meteorological files.}
  \item{index}{An integer to indicate the point in a \code{\link{SpatialPointsMeteorology-class}} object or the grid cell in a \code{\link{SpatialGridMeteorology-class}} object.}
  \item{var}{The meteorological variable to be plotted.}
  \item{fun}{The name of a function to be calculated for summaries (only valid if \code{freq} is specified).}
  \item{freq}{
     A string giving an interval specification for summaries (e.g., \code{"week"}, \code{"month"}, \code{"quarter"} or \code{"year"}).
   }
  \item{dates}{
An object of class \code{\link{Date}} to define the period to be plotted. If \code{dates = NULL} then all dates in \code{object} are processed.
 }
 \item{months}{
  A numeric vector to indicate the subset of months for which plotting is desired (e.g. \code{c(7,8)} for July and August). When combined with \code{fun} and \code{freq}, this parameter allows plotting summaries for particular seasons. For example  \code{fun = "sum"} \code{freq = "years"} and \code{months = 6:8} leads to plotting the sum over summer months of each year.
 }
 \item{add}{A flag to indicate wether drawing should be done on the current plot (using function \code{lines}).}
  \item{...}{Additional parameters for functions \code{plot} or \code{lines}.}
}
\details{
Daily precipitation is plotted using bars (i.e. \code{type = "h"} when calling \code{\link{plot}}). Otherwise the function draws lines (i.e. \code{type = "l"} when calling \code{\link{plot}}). If \code{object} is of class \code{\link{SpatialPointsDataFrame-class}} then the function reads the meteorological data to be plotted from the disk.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{
\code{\link{summarypoints}}
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

#Plot interpolated meteorological series
meteoplot(mp,1, ylab="Daily mean temperature")

meteoplot(mp,1, ylab="Monthly mean temperature", fun=mean, freq="months")

}
