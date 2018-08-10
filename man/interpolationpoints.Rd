\encoding{UTF-8}
\name{interpolationpoints}
\alias{interpolationpoints}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolates meteorological variables for a set of points.
}
\description{
Interpolates meteorological data for points using an object of class \code{\link{MeteorologyInterpolationData-class}}.
}
\usage{
interpolationpoints(object, points, dates = NULL, export = FALSE,
                    exportDir = getwd(), exportFormat = "meteoland/txt",
                    metadatafile = "MP.txt", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{MeteorologyInterpolationData-class}}.}
  \item{points}{An object of class \code{\link{SpatialPointsTopography-class}}.}
  \item{dates}{An object of class \code{\link{Date}}. If this is \code{NULL} then all dates in \code{object} are processed.}
  \item{export}{
    If \code{export = FALSE} the result of interpolation is stored in memory. Otherwise the result is written in the disk (using the format specified in \code{exportFormat}).
  }
  \item{exportDir}{Output directory for interpolated meteorology data.}
  \item{exportFormat}{Export format for meteorological data (see \code{\link{writemeteorologypoint}}).}
  \item{metadatafile}{The name of the ascii text file that will store the meta data describing all written files.}
  \item{verbose}{Boolean flag to print process information.}
}
\value{
If \code{export = FALSE}, function \code{interpolationpoints} returns an object of \code{\link{SpatialPointsMeteorology-class}}. If \code{export = TRUE}, the function returns an object of class \code{\link{SpatialPointsDataFrame-class}} containing the meta data of the files written in the disk.
}
\references{
De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating daily meteorological data and downscaling climate models over landscapes. Environmental Modelling and Software 108: 186-196.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{\code{\link{penman}}, \code{\link{SpatialPointsTopography-class}}, \code{\link{MeteorologyInterpolationData}}}

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
meteoplot(mp,1, ylab="Mean temperature")
}
