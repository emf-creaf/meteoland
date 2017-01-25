\encoding{UTF-8}
\name{SpatialGridTopography}
\alias{SpatialGridTopography}

\title{Creates a 'SpatialGridTopography'}
\description{
Function \code{SpatialGridTopography} creates an object of class \code{\link{SpatialGridTopography-class}} containing topographic variables over a landscape.
}
\usage{
SpatialGridTopography(grid, elevation, proj4string = CRS(as.character(NA)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{An object of class \code{\link{GridTopology-class}} or  \code{\link{SpatialGrid-class}}.}
  \item{elevation}{A vector of elevation values for all cells of the grid (in m).}
  \item{proj4string}{An object of class \code{\link{CRS-class}}.}
}
\details{
Slope and aspect calculations were adapted from functions in package 'SDMTools', which used the approach described in Burrough & McDonell (1998).

The rate of change (delta) of the surface in the horizontal \code{(dz/dx)} and vertical \code{(dz/dy)} directions from the center cell determines the slope and aspect. The values of the center cell and its eight neighbors determine the horizontal and vertical deltas. The neighbors are identified as letters from 'a' to 'i', with 'e' representing the cell for which the aspect is being calculated. The rate of change in the x direction for cell 'e' is calculated with the algorithm:

\code{[dz/dx] = ((c + 2f + i) - (a + 2d + g) / (8 * x_cell_size)}

The rate of change in the y direction for cell 'e' is calculated with the following algorithm:

\code{[dz/dy] = ((g + 2h + i) - (a + 2b + c)) / (8 * y_cell_size)}

The algorithm calculates slope as: \code{rise_run = sqrt ( [dz/dx]2 + [dz/dy]2 ])}.

From this value , one can calculate the slope in degrees or radians as:

\code{slope_degrees = ATAN (rise_run) * 57.29578}

\code{slope_radians = ATAN (rise_run)}

Taking the rate of change in both the x and y direction for cell 'e', aspect is calculated using:

\code{aspect = 57.29578 * atan2 ([dz/dy], -[dz/dx])}

The aspect value is then converted to compass direction values (0-360 degrees).
}
\value{
Function \code{SpatialGridTopography} returns an object '\code{\link{SpatialGridTopography-class}}'.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{
\code{\link{extractSpatialData}}, \code{\link{SpatialGridTopography-class}}
}
\references{
Burrough, P. A. and McDonell, R.A., 1998. Principles of Geographical Information Systems (Oxford University Press, New York), p. 190.
}
\examples{
data(examplegridtopography)

#Display data
spplot(examplegridtopography, type="elevation", scales=list(draw=TRUE))
spplot(examplegridtopography, type="slope", scales=list(draw=TRUE))
spplot(examplegridtopography, type="aspect", scales=list(draw=TRUE))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
