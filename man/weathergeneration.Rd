\encoding{UTF-8}
\name{weathergeneration}
\alias{weathergeneration}

\title{
Weather generation
}
\description{
A semiparametric multivariate, multisite weather generator with low-frequency variability.
}
\usage{
weathergeneration(object, params = defaultGenerationParams(), verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{SpatialPointsMeteorology-class}}, \code{\link{SpatialGridMeteorology-class}} or \code{\link{SpatialPixelsMeteorology-class}}.}
  \item{params}{A list with parameters for the weather generator (see \code{\link{defaultGenerationParams}}).}
  \item{verbose}{Boolean flag to print process information.}
}
\value{
}
\details{

}
\references{
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa,  CTFC
}
\seealso{\code{\link{defaultGenerationParams}}}

\examples{
data(examplegridtopography)
data(exampleinterpolationdata)

#Creates spatial topography points from the grid
p = 1:2
spt = as(examplegridtopography, "SpatialPointsTopography")[p]

#Interpolation of two points for the whole time period (2000-2003)
mp = interpolationpoints(exampleinterpolationdata, spt)

#Plot interpolated meteorological series
meteoplot(mp,1, ylab="Mean temperature")

#Generate a stochastic series using interpolated data as template
y = weathergeneration(mp)

#Plot generated meteorological series
meteoplot(y,1, ylab="Mean temperature")
}
