\encoding{UTF-8}
\name{MeteorologyDownscalingData}
\alias{MeteorologyDownscalingData}
\title{
Creates an object of class 'MeteorologyDownscalingData'
}
\description{
Initializes an object for statistical downscaling of meteorological data over landscapes.
}
\usage{
MeteorologyDownscalingData(points, historicdata, futuredata, dates,
                           params = defaultDownscalingParams())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points}{An object of class \code{\link{SpatialPoints}}.}
  \item{historicdata}{Historic (reference) meteorological data used to calibrate correction factors when compared with observations. A vector of data frames (one per point) or a single data frame containing the meta data (columns \code{dir} and \code{filename}) of meteorological files that will be read from the disk. }
  \item{futuredata}{Future (predicted) meteorological data to be corrected. A vector of data frames (one per point) or a single data frame containing the meta data (columns \code{dir} and \code{filename}) of meteorological files that will be read from the disk. }
  \item{dates}{Object of class \code{"Date"} describing the time period for which meteorological downscaling is possible (corresponding to \code{futuredata}).}
  \item{params}{A \code{"list"} containing downscaling parameters. }
}
\details{
See downscaling details in vignettes or in \code{\link{downscalingpoints}}.
}
\value{
An object of class \code{\linkS4class{MeteorologyDownscalingData}.}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}

\seealso{
\code{\linkS4class{MeteorologyDownscalingData}}, \code{\link{exampledownscalingdata}}, \code{\link{defaultDownscalingParams}}.
}
