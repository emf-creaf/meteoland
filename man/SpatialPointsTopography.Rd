\encoding{UTF-8}
\name{SpatialPointsTopography}
\alias{SpatialPointsTopography}

\title{Creates a 'SpatialPointsTopography'}
\description{
Function \code{SpatialPointsTopography} creates an object of class \code{\link{SpatialPointsTopography-class}} containing topographic variables for a set of points.
}
\usage{
SpatialPointsTopography(points, elevation, slope, aspect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points}{An object of class \code{\link{SpatialPoints-class}}.}
  \item{elevation}{Elevation values (in m) of the points.}
  \item{slope}{Slope values (in degrees) of the points.}
  \item{aspect}{Aspect values (in degrees from North) of the points.}
}
\value{
Function \code{SpatialPointsTopography} returns an object '\code{\link{SpatialPointsTopography-class}}'.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{
\code{\link{SpatialPointsTopography-class}}
}
\examples{
data(examplegridtopography)

#Creates spatial topography points from the grid
p = 1:2
points = as(examplegridtopography,"SpatialPoints")[p]
spt = SpatialPointsTopography(points, examplegridtopography$elevation[p],
                              examplegridtopography$slope[p],
                              examplegridtopography$aspect[p])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
