\encoding{UTF-8}
\name{MeteorologyUncorrectedData}
\alias{MeteorologyUncorrectedData}
\title{
Creates an object of class 'MeteorologyUncorrectedData'
}
\description{
Initializes an object for statistical correction of meteorological data over landscapes.
}
\usage{
MeteorologyUncorrectedData(points, reference_data, projection_data, dates,
                           params = defaultCorrectionParams())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points}{An object of class \code{\link{SpatialPoints}}.}
  \item{reference_data}{Reference (historic) meteorological data used to calibrate correction factors when compared with observations. A vector of data frames (one per point) or a single data frame containing the meta data (columns \code{dir} and \code{filename}) of meteorological files that will be read from the disk. }
  \item{projection_data}{Projected meteorological data to be corrected. A vector of data frames (one per point) or a single data frame containing the meta data (columns \code{dir} and \code{filename}) of meteorological files that will be read from the disk. }
  \item{dates}{Object of class \code{"Date"} describing the time period for which meteorological correction is possible (corresponding to \code{projection_data}).}
  \item{params}{A \code{"list"} containing correction parameters. }
}
\details{
See correction details in vignettes or in \code{\link{correctionpoints}}.
}
\value{
An object of class \code{\linkS4class{MeteorologyUncorrectedData}.}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}

\seealso{
\code{\linkS4class{MeteorologyUncorrectedData}}, \code{\link{examplecorrectiondata}}, \code{\link{defaultCorrectionParams}}.
}
