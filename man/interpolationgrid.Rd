\encoding{UTF-8}
\name{interpolationgrid}
\alias{interpolationgrid}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolates meteorological variables over a grid.
}
\description{
Interpolates meteorological data over grids using objects of class \code{\link{MeteorologyInterpolationData-class}}.
}
\usage{
interpolationgrid(object, grid, dates, export = FALSE,
                  exportDir=getwd(), exportFormat = "netCDF",
                  metadatafile = "MG.txt", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{MeteorologyInterpolationData-class}}.}
  \item{grid}{An object of class \code{\link{SpatialGridTopography-class}} representing the target landscape.}
  \item{dates}{An object of class \code{\link{Date}}.}
  \item{export}{
    If \code{export = FALSE} the result of interpolation is stored in memory. Otherwise the result is written in the disk (using the format specified in \code{exportFormat}).
  }
  \item{exportDir}{Output directory for interpolated meteorology.}
  \item{exportFormat}{Format of meteorological data. Current accepted format is \code{"netCDF"} only.}
  \item{metadatafile}{The name of the file that will store the meta data describing all written files.}
  \item{verbose}{Boolean flag to print process information.}
}
\value{
If \code{export = FALSE}, function \code{interpolationgrid} returns an object of \code{\link{SpatialGridMeteorology-class}}. If \code{export = TRUE}, the function writes the results in files and a \code{data.frame} with columns 'dir' and 'filename' is returned.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{\code{\link{penman}}, \code{\link{SpatialGridTopography}}, \code{\link{MeteorologyInterpolationData}}}

\examples{
data(examplegridtopography)
data(exampleinterpolationdata)

#Interpolation of meteorology over a grid for two days
examplegridtopography@proj4string = exampleinterpolationdata@proj4string
ml = interpolationgrid(exampleinterpolationdata, examplegridtopography,
                       as.Date(c("2001-02-03", "2001-06-03")))
#Plot PET corresponding to 2001-06-03
spplot(ml,2,"PET")
}
